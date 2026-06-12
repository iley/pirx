package desugar

import (
	"fmt"

	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/util"
)

func Run(program *ast.Program) (*ast.Program, []error) {
	dc := &desugarContext{
		types:   program.TypeTable,
		errors:  []error{},
		eqFuncs: map[string]bool{},
	}
	result := desugarProgram(dc, program)
	return result, dc.errors
}

type desugarContext struct {
	types  *ast.TypeTable
	errors []error
	// Functions generated during desugaring (struct equality), appended to the program.
	eqFuncs        map[string]bool
	generatedFuncs []ast.Function
}

func (dc *desugarContext) errorf(format string, args ...any) {
	dc.errors = append(dc.errors, fmt.Errorf(format, args...))
}

func desugarProgram(dc *desugarContext, program *ast.Program) *ast.Program {
	result := *program

	result.VariableDeclarations = make([]ast.VariableDeclaration, len(program.VariableDeclarations))
	for i := range program.VariableDeclarations {
		result.VariableDeclarations[i] = *desugarVariableDeclaration(dc, &program.VariableDeclarations[i])
	}

	result.Functions = make([]ast.Function, len(program.Functions))
	for i := range program.Functions {
		result.Functions[i] = *desugarFunction(dc, &program.Functions[i])
	}
	result.Functions = append(result.Functions, dc.generatedFuncs...)
	return &result
}

func desugarFunction(dc *desugarContext, function *ast.Function) *ast.Function {
	result := *function
	result.Body = desugarBlock(dc, function.Body)
	return &result
}

func desugarBlock(dc *desugarContext, block *ast.Block) *ast.Block {
	if block == nil {
		return nil
	}

	result := *block
	result.Statements = make([]ast.Statement, len(block.Statements))
	for i := range block.Statements {
		result.Statements[i] = desugarStatement(dc, block.Statements[i])
	}

	return &result
}

func desugarStatement(dc *desugarContext, stmt ast.Statement) ast.Statement {
	switch s := stmt.(type) {
	case *ast.VariableDeclaration:
		return desugarVariableDeclaration(dc, s)
	case *ast.ExpressionStatement:
		result := *s
		result.Expression = desugarExpression(dc, s.Expression)
		return &result
	case *ast.ReturnStatement:
		result := *s
		if s.Value != nil {
			result.Value = desugarExpression(dc, s.Value)
		}
		return &result
	case *ast.IfStatement:
		result := *s
		result.Condition = desugarExpression(dc, s.Condition)
		result.ThenBlock = *desugarBlock(dc, &s.ThenBlock)
		if s.ElseBlock != nil {
			elseBlock := desugarBlock(dc, s.ElseBlock)
			result.ElseBlock = elseBlock
		}
		return &result
	case *ast.WhileStatement:
		result := *s
		result.Condition = desugarExpression(dc, s.Condition)
		result.Body = *desugarBlock(dc, &s.Body)
		return &result
	case *ast.ForStatement:
		return desugarForStatement(dc, s)
	case *ast.BreakStatement:
		return s
	case *ast.ContinueStatement:
		return s
	case *ast.BlockStatement:
		result := *s
		result.Block = *desugarBlock(dc, &s.Block)
		return &result
	default:
		return stmt
	}
}

func desugarVariableDeclaration(dc *desugarContext, decl *ast.VariableDeclaration) *ast.VariableDeclaration {
	result := *decl
	if decl.Initializer != nil {
		result.Initializer = desugarExpression(dc, decl.Initializer)
	}
	return &result
}

// Desugar a `for init; cond; update { block }` into `{ init; while (cond) { block'; update } }`,
// where block' has each `continue` replaced with `{ update; continue; }` so the update
// expression is not skipped.
func desugarForStatement(dc *desugarContext, forStmt *ast.ForStatement) ast.Statement {
	statements := []ast.Statement{}

	if forStmt.Init != nil {
		desugaredInit := desugarStatement(dc, forStmt.Init)
		statements = append(statements, desugaredInit)
	}

	var updateStmt ast.Statement
	if forStmt.Update != nil {
		desugaredUpdate := desugarExpression(dc, forStmt.Update)
		updateStmt = &ast.ExpressionStatement{
			Loc:        desugaredUpdate.GetLocation(),
			Expression: desugaredUpdate,
		}
	}

	whileStatements := make([]ast.Statement, 0, len(forStmt.Body.Statements)+1)
	for _, stmt := range forStmt.Body.Statements {
		desugared := desugarStatement(dc, stmt)
		if updateStmt != nil {
			desugared = rewriteContinue(desugared, updateStmt)
		}
		whileStatements = append(whileStatements, desugared)
	}
	if updateStmt != nil {
		whileStatements = append(whileStatements, updateStmt)
	}

	// A missing condition means "loop forever".
	var condition ast.Expression
	if forStmt.Condition != nil {
		condition = desugarExpression(dc, forStmt.Condition)
	} else {
		condition = &ast.Literal{Loc: forStmt.Loc, BoolValue: util.BoolPtr(true), Type: ast.Bool}
	}

	whileStmt := &ast.WhileStatement{
		Loc:       forStmt.Loc,
		Condition: condition,
		Body: ast.Block{
			Loc:        forStmt.Loc,
			Statements: whileStatements,
		},
	}

	statements = append(statements, whileStmt)

	return &ast.BlockStatement{
		Loc: forStmt.Loc,
		Block: ast.Block{
			Statements: statements,
		},
	}
}

// rewriteContinue replaces each `continue` in stmt with `{ updateStmt; continue; }`,
// without descending into nested loops (which have their own continue targets).
func rewriteContinue(stmt ast.Statement, updateStmt ast.Statement) ast.Statement {
	switch s := stmt.(type) {
	case *ast.ContinueStatement:
		return &ast.BlockStatement{
			Loc: s.Loc,
			Block: ast.Block{
				Statements: []ast.Statement{updateStmt, s},
			},
		}
	case *ast.IfStatement:
		result := *s
		result.ThenBlock = *rewriteContinueBlock(&s.ThenBlock, updateStmt)
		if s.ElseBlock != nil {
			result.ElseBlock = rewriteContinueBlock(s.ElseBlock, updateStmt)
		}
		return &result
	case *ast.BlockStatement:
		result := *s
		result.Block = *rewriteContinueBlock(&s.Block, updateStmt)
		return &result
	// Don't descend into nested loops — they have their own continue targets.
	case *ast.WhileStatement, *ast.ForStatement:
		return stmt
	default:
		return stmt
	}
}

func rewriteContinueBlock(block *ast.Block, updateStmt ast.Statement) *ast.Block {
	result := *block
	result.Statements = make([]ast.Statement, len(block.Statements))
	for i, stmt := range block.Statements {
		result.Statements[i] = rewriteContinue(stmt, updateStmt)
	}
	return &result
}

func desugarExpression(dc *desugarContext, originalExpr ast.Expression) ast.Expression {
	switch expr := originalExpr.(type) {
	case *ast.Literal:
		if expr.StringValue != nil {
			return desugarStringLiteral(expr)
		}
		return expr
	case *ast.Assignment:
		return desugarAssignment(dc, expr)
	case *ast.VariableReference:
		return expr
	case *ast.FunctionCall:
		return desugarFunctionCall(dc, expr)
	case *ast.BinaryOperation:
		result := *expr
		result.Left = desugarExpression(dc, expr.Left)
		result.Right = desugarExpression(dc, expr.Right)
		if result.Operator == "==" || result.Operator == "!=" {
			return desugarEquality(dc, &result)
		}
		return &result
	case *ast.UnaryOperation:
		result := *expr
		result.Operand = desugarExpression(dc, expr.Operand)
		return &result
	case *ast.FieldAccess:
		result := *expr
		result.Object = desugarExpression(dc, expr.Object)
		return &result
	case *ast.IndexExpression:
		result := *expr
		result.Array = desugarExpression(dc, expr.Array)
		result.Index = desugarExpression(dc, expr.Index)
		return &result
	case *ast.RangeExpression:
		return desugarRangeExpression(dc, expr)
	case *ast.NewExpression:
		result := *expr
		if expr.Count != nil {
			result.Count = desugarExpression(dc, expr.Count)
		}
		return &result
	case *ast.PostfixOperator:
		result := *expr
		result.Operand = desugarExpression(dc, expr.Operand)
		return &result
	case *ast.PrefixOperator:
		result := *expr
		result.Operand = desugarExpression(dc, expr.Operand)
		return &result
	case *ast.InitializerList:
		result := *expr
		result.Elements = make([]ast.Expression, len(expr.Elements))
		for i, elem := range expr.Elements {
			result.Elements[i] = desugarExpression(dc, elem)
		}
		return &result
	}
	panic(fmt.Errorf("%s: unknown expression type: %v", originalExpr.GetLocation(), originalExpr))
}

func desugarFunctionCall(dc *desugarContext, call *ast.FunctionCall) ast.Expression {
	// Recursively desugar function call arguments
	result := *call
	result.Args = make([]ast.Expression, len(call.Args))
	for i, arg := range call.Args {
		result.Args[i] = desugarExpression(dc, arg)
	}
	switch call.FunctionName {
	case "sizeof":
		return desugarSizeof(dc, &result)
	case "int":
		return desugarIntCast(&result)
	case "printf":
		return desugarPrintf(&result)
	}
	return &result
}

// desugarPrintf wraps string varargs in cstr(). C varargs cannot carry a 16-byte
// Pirx string: vprintf's %s would consume only the data pointer, leaving all
// subsequent arguments misaligned. The format string itself stays a slice because
// it's a named argument of PirxPrintf.
func desugarPrintf(call *ast.FunctionCall) *ast.FunctionCall {
	for i, arg := range call.Args[1:] {
		if ast.String.Equals(arg.GetType()) {
			call.Args[i+1] = &ast.FunctionCall{
				Loc:          arg.GetLocation(),
				FunctionName: "cstr",
				Args:         []ast.Expression{arg},
				Type:         &ast.PointerType{ElementType: ast.Int8},
			}
		}
	}
	return call
}

func desugarSizeof(dc *desugarContext, call *ast.FunctionCall) ast.Expression {
	if len(call.Args) != 1 {
		dc.errorf("%s: size() takes exactly one argument, got %d", call.Loc, len(call.Args))
		return call
	}
	argType := call.Args[0].GetType()
	argSize, err := dc.types.GetSize(argType)
	if err != nil {
		dc.errorf("%s: invalid type %s, %v", call.Loc, argType, err)
	}
	return &ast.Literal{
		Loc:      call.Loc,
		IntValue: util.Int32Ptr(argSize),
		Type:     ast.Int,
	}
}

var intCasts = map[string]string{
	"int":     "PirxIntFromInt",
	"int8":    "PirxIntFromInt8",
	"int64":   "PirxIntFromInt64",
	"float32": "PirxIntFromFloat32",
	"float64": "PirxIntFromFloat64",
}

func desugarIntCast(call *ast.FunctionCall) *ast.FunctionCall {
	argType := call.Args[0].GetType()
	resolvedFuncName, ok := intCasts[argType.String()]
	if !ok {
		panic(fmt.Errorf("could not resolve cast from %s to int", argType))
	}
	result := *call
	result.FunctionName = resolvedFuncName
	return &result
}

func desugarStringLiteral(expr *ast.Literal) ast.Expression {
	strLen := int32(len(*expr.StringValue))
	return &ast.FunctionCall{
		Loc:          expr.Loc,
		FunctionName: "PirxString",
		Args: []ast.Expression{
			&ast.Literal{
				Loc:      expr.Loc,
				IntValue: &strLen,
				Type:     ast.Int,
			},
			expr,
		},
		Type: ast.String,
	}
}

func desugarRangeExpression(dc *desugarContext, expr *ast.RangeExpression) ast.Expression {
	return &ast.FunctionCall{
		Loc:          expr.Loc,
		FunctionName: "range",
		Args: []ast.Expression{
			desugarExpression(dc, expr.Array),
			desugarExpression(dc, expr.Start),
			desugarExpression(dc, expr.End),
		},
		Type: expr.Type,
	}
}

func desugarAssignment(dc *desugarContext, expr *ast.Assignment) ast.Expression {
	// Compound operators (+= etc.) are kept as-is: expanding `T op= V` into `T = T op V`
	// here would evaluate the target twice, duplicating side effects like `a[f()] += 1`.
	// The IR generator lowers them through a single address computation instead.
	result := *expr
	result.Target = desugarExpression(dc, expr.Target)
	result.Value = desugarExpression(dc, expr.Value)
	return &result
}

// desugarEquality lowers ==/!= on strings and structs, which have no direct machine
// representation, into function calls: PirxStringEq for strings and a generated
// field-by-field comparison function for structs. Scalar comparisons are kept as-is.
// The operands of binOp must already be desugared.
func desugarEquality(dc *desugarContext, binOp *ast.BinaryOperation) ast.Expression {
	typ := binOp.Left.GetType()

	var eqFuncName string
	if ast.String.Equals(typ) {
		eqFuncName = "PirxStringEq"
	} else if _, err := dc.types.GetStruct(typ); err == nil {
		eqFuncName = ensureStructEqFunc(dc, typ)
	} else {
		return binOp
	}

	var result ast.Expression = &ast.FunctionCall{
		Loc:          binOp.Loc,
		FunctionName: eqFuncName,
		Args:         []ast.Expression{binOp.Left, binOp.Right},
		Type:         ast.Bool,
	}
	if binOp.Operator == "!=" {
		result = &ast.UnaryOperation{Loc: binOp.Loc, Operator: "!", Operand: result, Type: ast.Bool}
	}
	return result
}

// ensureStructEqFunc generates a function comparing two values of the given struct
// type field by field (at most once per type) and returns the function's name.
func ensureStructEqFunc(dc *desugarContext, typ ast.Type) string {
	sd, err := dc.types.GetStruct(typ)
	if err != nil {
		panic(err) // The caller has already checked that typ is a struct.
	}

	name := "PirxEq_" + sd.Name
	if dc.eqFuncs[name] {
		return name
	}
	dc.eqFuncs[name] = true

	a := &ast.VariableReference{Name: "a", Type: typ}
	b := &ast.VariableReference{Name: "b", Type: typ}

	// An empty struct is always equal to another one.
	var cmp ast.Expression = &ast.Literal{BoolValue: util.BoolPtr(true), Type: ast.Bool}
	for i, field := range sd.Fields {
		fieldCmp := desugarEquality(dc, &ast.BinaryOperation{
			Left:     &ast.FieldAccess{Object: a, FieldName: field.Name, Type: field.Type},
			Operator: "==",
			Right:    &ast.FieldAccess{Object: b, FieldName: field.Name, Type: field.Type},
			Type:     ast.Bool,
		})
		if i == 0 {
			cmp = fieldCmp
		} else {
			cmp = &ast.BinaryOperation{Left: cmp, Operator: "&&", Right: fieldCmp, Type: ast.Bool}
		}
	}

	dc.generatedFuncs = append(dc.generatedFuncs, ast.Function{
		Name:       name,
		Args:       []ast.Arg{{Name: "a", Type: typ}, {Name: "b", Type: typ}},
		ReturnType: ast.Bool,
		Body: &ast.Block{
			Statements: []ast.Statement{&ast.ReturnStatement{Value: cmp}},
		},
	})

	return name
}
