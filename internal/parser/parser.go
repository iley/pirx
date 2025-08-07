package parser

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/lexer"
)

func locationFromLexeme(lex lexer.Lexeme) ast.Location {
	return lex.Loc
}

type Parser struct {
	lexer   *lexer.Lexer
	lexemes []lexer.Lexeme
	pos     int
	program *ast.Program
}

func New() *Parser {
	return &Parser{}
}

func (p *Parser) consume() (lexer.Lexeme, error) {
	if p.pos >= len(p.lexemes) {
		lex, err := p.lexer.Next()
		if err != nil {
			return lexer.Lexeme{}, err
		}
		p.lexemes = append(p.lexemes, lex)
	}
	lex := p.lexemes[p.pos]
	p.pos++
	return lex, nil
}

func (p *Parser) peek() (lexer.Lexeme, error) {
	if p.pos >= len(p.lexemes) {
		lex, err := p.lexer.Next()
		if err != nil {
			return lexer.Lexeme{}, err
		}
		p.lexemes = append(p.lexemes, lex)
	}
	return p.lexemes[p.pos], nil
}

func (p *Parser) Parse(l *lexer.Lexer) error {
	p.lexer = l
	p.lexemes = []lexer.Lexeme{}
	p.pos = 0

	lex, err := p.peek()
	if err != nil {
		return err
	}
	programLoc := locationFromLexeme(lex)

	functions := []ast.Function{}
	typeDeclarations := []ast.TypeDeclaration{}
	constantDeclarations := []ast.ConstantDeclaration{}
	variableDeclarations := []ast.VariableDeclaration{}

	for {
		lex, err := p.peek()
		if err != nil {
			return err
		}
		if lex.Type == lexer.LEX_EOF {
			break
		}

		if lex.IsKeyword("struct") {
			structDecl, err := p.parseStructDeclaration()
			if err != nil {
				return err
			}
			typeDeclarations = append(typeDeclarations, structDecl)
		} else if lex.IsKeyword("val") {
			constDecl, err := p.parseConstantDeclaration()
			if err != nil {
				return err
			}
			constantDeclarations = append(constantDeclarations, *constDecl)
			// Consume semicolon after global constant declaration
			lex, err = p.consume()
			if err != nil {
				return err
			}
			if !lex.IsPunctuation(";") {
				return fmt.Errorf("%s: expected ';' after global constant declaration, got %v", lex.Loc, lex)
			}
		} else if lex.IsKeyword("var") {
			varDecl, err := p.parseVariableDeclaration()
			if err != nil {
				return err
			}
			variableDeclarations = append(variableDeclarations, *varDecl)
			// Consume semicolon after global variable declaration
			lex, err = p.consume()
			if err != nil {
				return err
			}
			if !lex.IsPunctuation(";") {
				return fmt.Errorf("%s: expected ';' after global variable declaration, got %v", lex.Loc, lex)
			}
		} else if lex.IsKeyword("extern") || lex.IsKeyword("func") {
			fn, err := p.parseFunction()
			if err != nil {
				return err
			}
			functions = append(functions, fn)
		} else {
			return fmt.Errorf("%s: expected 'struct', 'val', 'var', 'func' or 'extern', got %v", lex.Loc, lex)
		}
	}

	if p.program == nil {
		p.program = &ast.Program{Loc: programLoc, Functions: functions, TypeDeclarations: typeDeclarations, ConstantDeclarations: constantDeclarations, VariableDeclarations: variableDeclarations}
	} else {
		p.program.Functions = append(p.program.Functions, functions...)
		p.program.TypeDeclarations = append(p.program.TypeDeclarations, typeDeclarations...)
		p.program.ConstantDeclarations = append(p.program.ConstantDeclarations, constantDeclarations...)
		p.program.VariableDeclarations = append(p.program.VariableDeclarations, variableDeclarations...)
	}
	return nil
}

func (p *Parser) GetProgram() *ast.Program {
	return p.program
}

func (p *Parser) parseFunction() (ast.Function, error) {
	// Check if it starts with 'extern'
	lex, err := p.peek()
	if err != nil {
		return ast.Function{}, err
	}

	var isExternal bool
	var startLoc ast.Location

	if lex.IsKeyword("extern") {
		// consume 'extern'
		lex, err = p.consume()
		if err != nil {
			return ast.Function{}, err
		}
		isExternal = true
		startLoc = locationFromLexeme(lex)
	}

	// consume 'func'
	lex, err = p.consume()
	if err != nil {
		return ast.Function{}, err
	}
	if !lex.IsKeyword("func") {
		if isExternal {
			return ast.Function{}, fmt.Errorf("%s: expected 'func' after 'extern', got %v", lex.Loc, lex)
		} else {
			return ast.Function{}, fmt.Errorf("%s: expected 'func', got %v", lex.Loc, lex)
		}
	}

	if !isExternal {
		startLoc = locationFromLexeme(lex)
	}

	// Parse function name
	lex, err = p.consume()
	if err != nil {
		return ast.Function{}, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return ast.Function{}, fmt.Errorf("%s: expected function name, got %v", lex.Loc, lex)
	}
	name := lex.Str

	// Parse '('
	lex, err = p.consume()
	if err != nil {
		return ast.Function{}, err
	}
	if !lex.IsPunctuation("(") {
		return ast.Function{}, fmt.Errorf("%s: expected '(', got %v", lex.Loc, lex)
	}

	// Parse arguments
	args, err := p.parseArguments()
	if err != nil {
		return ast.Function{}, err
	}

	// Parse ')'
	lex, err = p.consume()
	if err != nil {
		return ast.Function{}, err
	}
	if !lex.IsPunctuation(")") {
		return ast.Function{}, fmt.Errorf("%s: expected ')', got %v", lex.Loc, lex)
	}

	// Parse optional return type
	lex, err = p.peek()
	if err != nil {
		return ast.Function{}, err
	}

	var returnType ast.Type
	if lex.IsPunctuation(":") {
		// consume ":"
		p.consume()

		// return type
		returnType, err = p.parseType()
		if err != nil {
			return ast.Function{}, err
		}
	}

	// Check what comes next: '{' for implementation or ';' for declaration
	lex, err = p.peek()
	if err != nil {
		return ast.Function{}, err
	}

	var body *ast.Block
	if lex.IsPunctuation("{") {
		// Function has implementation
		p.consume() // consume '{'
		body, err = p.parseBlock()
		if err != nil {
			return ast.Function{}, err
		}
	} else if lex.IsPunctuation(";") {
		// Function declaration only
		p.consume() // consume ';'
		body = nil
	} else {
		return ast.Function{}, fmt.Errorf("%s: expected '{' or ';' after function signature, got %v", lex.Loc, lex)
	}

	return ast.Function{
		Loc:        startLoc,
		Name:       name,
		Args:       args,
		Body:       body,
		ReturnType: returnType,
		External:   isExternal,
	}, nil
}

func (p *Parser) parseType() (ast.Type, error) {
	lex, err := p.peek()
	if err != nil {
		return nil, err
	}

	// Check for slice type (starts with '[')
	if lex.IsPunctuation("[") {
		p.consume() // consume '['

		// Expect ']'
		lex, err = p.consume()
		if err != nil {
			return nil, err
		}
		if !lex.IsPunctuation("]") {
			return nil, fmt.Errorf("%s: expected ']' in slice type, got %v", lex.Loc, lex)
		}

		// Parse the element type
		elementType, err := p.parseType()
		if err != nil {
			return nil, err
		}

		return &ast.SliceType{ElementType: elementType}, nil
	}

	// Check for pointer type (starts with '*')
	if lex.IsOperator("*") {
		p.consume() // consume '*'

		// Parse the underlying type
		underlyingType, err := p.parseType()
		if err != nil {
			return nil, err
		}

		return &ast.PointerType{ElementType: underlyingType}, nil
	}

	// Parse base type (identifier)
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return nil, fmt.Errorf("%s: expected type, got %v", lex.Loc, lex)
	}

	return ast.NewBaseType(lex.Str), nil
}

func (p *Parser) parseNewExpression() (ast.Expression, error) {
	// consume 'new'
	newLex, err := p.consume()
	if err != nil {
		return nil, err
	}
	newLoc := locationFromLexeme(newLex)

	// consume '('
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsPunctuation("(") {
		return nil, fmt.Errorf("%s: expected '(' after 'new', got %v", lex.Loc, lex)
	}

	// parse type
	typeExpr, err := p.parseType()
	if err != nil {
		return nil, err
	}

	// check for optional count parameter
	var countExpr ast.Expression
	lex, err = p.peek()
	if err != nil {
		return nil, err
	}
	if lex.IsPunctuation(",") {
		// consume ','
		_, err = p.consume()
		if err != nil {
			return nil, err
		}

		// parse count expression
		countExpr, err = p.parseExpression()
		if err != nil {
			return nil, err
		}
	}

	// consume ')'
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsPunctuation(")") {
		return nil, fmt.Errorf("%s: expected ')' after type in new expression, got %v", lex.Loc, lex)
	}

	return &ast.NewExpression{
		Loc:      newLoc,
		TypeExpr: typeExpr,
		Count:    countExpr,
	}, nil
}

func (p *Parser) parseArguments() ([]ast.Arg, error) {
	args := []ast.Arg{}

	lex, err := p.peek()
	if err != nil {
		return nil, err
	}
	if lex.IsPunctuation(")") {
		return args, nil
	}

	for {
		// arg name
		lex, err := p.consume()
		if err != nil {
			return nil, err
		}
		if lex.Type != lexer.LEX_IDENT {
			return nil, fmt.Errorf("%s: expected arg name, got %v", lex.Loc, lex)
		}
		argLoc := locationFromLexeme(lex)
		name := lex.Str

		// colon
		lex, err = p.consume()
		if err != nil {
			return nil, err
		}
		if !lex.IsPunctuation(":") {
			return nil, fmt.Errorf("%s: expected ':' after parameter name, got %v", lex.Loc, lex)
		}

		// arg type
		typeStr, err := p.parseType()
		if err != nil {
			return nil, err
		}

		args = append(args, ast.Arg{Loc: argLoc, Name: name, Type: typeStr})

		lex, err = p.peek()
		if err != nil {
			return nil, err
		}
		if lex.IsPunctuation(")") {
			break
		}

		// comma
		lex, err = p.consume()
		if err != nil {
			return nil, err
		}
		if !lex.IsPunctuation(",") {
			return nil, fmt.Errorf("%s: expected ',' or ')', got %v", lex.Loc, lex)
		}
	}

	return args, nil
}

func (p *Parser) parseBlock() (*ast.Block, error) {
	lex, err := p.peek()
	if err != nil {
		return nil, err
	}
	blockLoc := locationFromLexeme(lex)

	statements := []ast.Statement{}

	for {
		lex, err := p.peek()
		if err != nil {
			return nil, err
		}
		if lex.Type == lexer.LEX_EOF {
			return nil, fmt.Errorf("unexpected EOF")
		}
		if lex.IsPunctuation("}") {
			break
		}

		stmt, err := p.parseStatement()
		if err != nil {
			return nil, err
		}
		statements = append(statements, stmt)

		// Only require semicolon for statements that need it
		if statementRequiresSemicolon(stmt) {
			lex, err = p.consume()
			if err != nil {
				return nil, err
			}
			if !lex.IsPunctuation(";") {
				return nil, fmt.Errorf("%s: expected ';' after statement, got %v", lex.Loc, lex)
			}
		}
	}

	// consume '}'
	_, err = p.consume()
	if err != nil {
		return nil, err
	}

	return &ast.Block{Loc: blockLoc, Statements: statements}, nil
}

// statementRequiresSemicolon returns true if the statement requires a semicolon after it
func statementRequiresSemicolon(stmt ast.Statement) bool {
	// If statements don't require semicolons since they end with a block
	if _, ok := stmt.(*ast.IfStatement); ok {
		return false
	}
	// While statements don't require semicolons since they end with a block
	if _, ok := stmt.(*ast.WhileStatement); ok {
		return false
	}
	// For statements don't require semicolons since they end with a block
	if _, ok := stmt.(*ast.ForStatement); ok {
		return false
	}
	// Block statements don't require semicolons since they end with a brace
	if _, ok := stmt.(*ast.BlockStatement); ok {
		return false
	}
	// All other statements require semicolons
	return true
}

func (p *Parser) parseStatement() (ast.Statement, error) {
	lex, err := p.peek()
	if err != nil {
		return nil, err
	}
	if lex.IsKeyword("var") {
		varDecl, err := p.parseVariableDeclaration()
		if err != nil {
			return nil, err
		}
		return varDecl, nil
	}
	if lex.IsKeyword("val") {
		constDecl, err := p.parseConstantDeclaration()
		if err != nil {
			return nil, err
		}
		return constDecl, nil
	}
	if lex.IsKeyword("return") {
		retStmt, err := p.parseReturnStatement()
		if err != nil {
			return nil, err
		}
		return retStmt, nil
	}
	// TODO: Validate that break is only used inside a loop (likely a separate AST pass).
	if lex.IsKeyword("break") {
		breakLex, err := p.consume() // consume the "break" keyword
		if err != nil {
			return nil, err
		}
		return &ast.BreakStatement{Loc: locationFromLexeme(breakLex)}, nil
	}
	// TODO: Validate that continue is only used inside a loop (likely a separate AST pass).
	if lex.IsKeyword("continue") {
		continueLex, err := p.consume() // consume the "continue" keyword
		if err != nil {
			return nil, err
		}
		return &ast.ContinueStatement{Loc: locationFromLexeme(continueLex)}, nil
	}
	if lex.IsKeyword("if") {
		ifStmt, err := p.parseIfStatement()
		if err != nil {
			return nil, err
		}
		return ifStmt, nil
	}
	if lex.IsKeyword("while") {
		whileStmt, err := p.parseWhileStatement()
		if err != nil {
			return nil, err
		}
		return whileStmt, nil
	}
	if lex.IsKeyword("for") {
		forStmt, err := p.parseForStatement()
		if err != nil {
			return nil, err
		}
		return forStmt, nil
	}
	if lex.IsPunctuation("{") {
		blockStmt, err := p.parseBlockStatement()
		if err != nil {
			return nil, err
		}
		return blockStmt, nil
	}
	expression, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
	return &ast.ExpressionStatement{Loc: expression.GetLocation(), Expression: expression}, nil
}

func (p *Parser) parseExpression() (ast.Expression, error) {
	return p.parseExpressionWithPrecedence(0)
}

func (p *Parser) parseExpressionWithPrecedence(minPrecedence int) (ast.Expression, error) {
	// Parse the left operand
	left, err := p.parsePrimaryExpression()
	if err != nil {
		return nil, err
	}

	for {
		// Check for binary operators
		lex, err := p.peek()
		if err != nil {
			return nil, err
		}

		// Check if this is a binary operator we recognize
		if lex.Type != lexer.LEX_OPERATOR || !isBinaryOperator(lex.Str) {
			break
		}

		precedence := getOperatorPrecedence(lex.Str)
		if precedence < minPrecedence {
			break
		}

		operator := lex.Str
		operatorLex, err := p.consume() // consume the operator
		if err != nil {
			return nil, err
		}
		operatorLoc := locationFromLexeme(operatorLex)

		// For left-associative operators, use precedence + 1
		// For right-associative operators, use precedence
		// Assignment is right-associative, so use same precedence
		nextMinPrecedence := precedence + 1
		if operator == "=" {
			nextMinPrecedence = precedence
		}

		right, err := p.parseExpressionWithPrecedence(nextMinPrecedence)
		if err != nil {
			return nil, err
		}

		if operator == "=" {
			// Validate that left side is a valid assignment target
			if !p.isValidAssignmentTarget(left) {
				return nil, fmt.Errorf("%s: invalid assignment target: %s", left.GetLocation(), left.String())
			}
			left = &ast.Assignment{
				Loc:    operatorLoc,
				Target: left,
				Value:  right,
			}
		} else {
			left = &ast.BinaryOperation{
				Loc:      operatorLoc,
				Left:     left,
				Operator: operator,
				Right:    right,
			}
		}
	}

	return left, nil
}

func isBinaryOperator(op string) bool {
	return op == "+" || op == "-" || op == "*" || op == "/" || op == "%" ||
		op == "==" || op == "!=" || op == "<" || op == ">" || op == "<=" || op == ">=" ||
		op == "&&" || op == "||" || op == "="
}

func getOperatorPrecedence(op string) int {
	switch op {
	case "=":
		return 1
	case "||":
		return 2
	case "&&":
		return 3
	case "==", "!=", "<", ">", "<=", ">=":
		return 4
	case "+", "-":
		return 5
	case "*", "/", "%":
		return 6
	default:
		return 0
	}
}

func (p *Parser) parsePrimaryExpression() (ast.Expression, error) {
	var expr ast.Expression
	var err error

	lex, err := p.peek()
	if err != nil {
		return nil, err
	}
	switch lex.Type {
	case lexer.LEX_KEYWORD:
		if lex.IsKeyword("true") {
			p.consume()
			expr = ast.NewBoolLiteral(true)
		} else if lex.IsKeyword("false") {
			p.consume()
			expr = ast.NewBoolLiteral(false)
		} else if lex.IsKeyword("null") {
			p.consume()
			expr = ast.NewNullLiteral()
		} else if lex.IsKeyword("new") {
			expr, err = p.parseNewExpression()
			if err != nil {
				return nil, err
			}
		} else {
			return nil, fmt.Errorf("%s: unexpected keyword %s when parsing a primary expression", lex.Loc, lex.Str)
		}
	case lexer.LEX_IDENT:
		// Look ahead to determine if this is a function call or assignment
		expr, err = p.parseIdentifierExpression()
		if err != nil {
			return nil, err
		}
	case lexer.LEX_NUMBER:
		expr, err = p.parseIntegerLiteral()
		if err != nil {
			return nil, err
		}
	case lexer.LEX_STRING:
		expr, err = p.parseStringLiteral()
		if err != nil {
			return nil, err
		}
	case lexer.LEX_OPERATOR:
		if lex.Str == "!" || lex.Str == "-" || lex.Str == "*" {
			return p.parseUnaryExpression()
		}
		if lex.Str == "&" {
			return p.parseAddressOfExpression()
		}
		return nil, fmt.Errorf("%s: unknown expression: %v", lex.Loc, lex)
	case lexer.LEX_PUNCTUATION:
		if lex.Str == "(" {
			expr, err = p.parseParenthesizedExpression()
			if err != nil {
				return nil, err
			}
		} else {
			return nil, fmt.Errorf("%s: unknown expression: %v", lex.Loc, lex)
		}
	default:
		return nil, fmt.Errorf("%s: unknown expression: %v", lex.Loc, lex)
	}

	// Handle field access, indexing, and postfix increment operations
	for {
		lex, err := p.peek()
		if err != nil {
			return nil, err
		}
		if lex.IsPunctuation(".") {
			// consume '.'
			dotLex, err := p.consume()
			if err != nil {
				return nil, err
			}
			dotLoc := locationFromLexeme(dotLex)

			// expect field name
			fieldLex, err := p.consume()
			if err != nil {
				return nil, err
			}
			if fieldLex.Type != lexer.LEX_IDENT {
				return nil, fmt.Errorf("%s: expected field name after '.', got %v", fieldLex.Loc, fieldLex)
			}

			expr = &ast.FieldAccess{
				Loc:       dotLoc,
				Object:    expr,
				FieldName: fieldLex.Str,
			}
		} else if lex.IsPunctuation("[") {
			// consume '['
			bracketLex, err := p.consume()
			if err != nil {
				return nil, err
			}
			bracketLoc := locationFromLexeme(bracketLex)

			// parse index expression
			indexExpr, err := p.parseExpression()
			if err != nil {
				return nil, err
			}

			// expect ']'
			closeLex, err := p.consume()
			if err != nil {
				return nil, err
			}
			if !closeLex.IsPunctuation("]") {
				return nil, fmt.Errorf("%s: expected ']' after index expression, got %v", closeLex.Loc, closeLex)
			}

			expr = &ast.IndexExpression{
				Loc:   bracketLoc,
				Array: expr,
				Index: indexExpr,
			}
		} else if lex.IsOperator("++") {
			// consume '++'
			incLex, err := p.consume()
			if err != nil {
				return nil, err
			}
			incLoc := locationFromLexeme(incLex)

			// Validate that operand is a valid assignment target
			if !p.isValidAssignmentTarget(expr) {
				return nil, fmt.Errorf("%s: invalid operand for postfix increment: %s", expr.GetLocation(), expr.String())
			}

			expr = &ast.PostfixOperator{
				Loc:      incLoc,
				Operator: "++",
				Operand:  expr,
			}
		} else if lex.IsOperator("--") {
			// consume '--'
			decLex, err := p.consume()
			if err != nil {
				return nil, err
			}
			decLoc := locationFromLexeme(decLex)

			// Validate that operand is a valid assignment target
			if !p.isValidAssignmentTarget(expr) {
				return nil, fmt.Errorf("%s: invalid operand for postfix decrement: %s", expr.GetLocation(), expr.String())
			}

			expr = &ast.PostfixOperator{
				Loc:      decLoc,
				Operator: "--",
				Operand:  expr,
			}
		} else {
			break
		}
	}

	return expr, nil
}

func (p *Parser) parseFunctionCall() (ast.Expression, error) {
	// function name
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return nil, fmt.Errorf("%s: expected function name, got %v", lex.Loc, lex)
	}
	callLoc := locationFromLexeme(lex)
	name := lex.Str

	// '('
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsPunctuation("(") {
		return nil, fmt.Errorf("%s: expected '(', got %v", lex.Loc, lex)
	}

	args := []ast.Expression{}

	// check for ')'
	lex, err = p.peek()
	if err != nil {
		return nil, err
	}
	if lex.IsPunctuation(")") {
		p.consume() // consume ')'
		return &ast.FunctionCall{Loc: callLoc, FunctionName: name, Args: args}, nil
	}

	for {
		arg, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		args = append(args, arg)

		lex, err := p.peek()
		if err != nil {
			return nil, err
		}
		if lex.IsPunctuation(")") {
			break
		}

		lex, err = p.consume()
		if err != nil {
			return nil, err
		}
		if !lex.IsPunctuation(",") {
			return nil, fmt.Errorf("%s: expected ',' or ')', got %v", lex.Loc, lex)
		}
	}

	// consume ')'
	_, err = p.consume()
	if err != nil {
		return nil, err
	}

	return &ast.FunctionCall{Loc: callLoc, FunctionName: name, Args: args}, nil
}

func (p *Parser) parseIntegerLiteral() (ast.Expression, error) {
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	litLoc := locationFromLexeme(lex)
	var literal *ast.Literal

	// Determine if this is a 64-bit literal (has 'l' suffix)
	is64Bit := strings.HasSuffix(lex.Str, "l")
	// Determine if this is an 8-bit literal (has 'i8' suffix)
	is8Bit := strings.HasSuffix(lex.Str, "i8")

	str := lex.Str
	if is64Bit {
		str, _ = strings.CutSuffix(lex.Str, "l")
	} else if is8Bit {
		str, _ = strings.CutSuffix(lex.Str, "i8")
	}

	// Determine the base
	base := 10
	if strings.HasPrefix(str, "0x") || strings.HasPrefix(str, "0X") {
		base = 16
		str = str[2:] // Remove "0x" or "0X" prefix
	}

	if is64Bit {
		// int64 literal
		val, err := strconv.ParseInt(str, base, 64)
		if err != nil {
			return nil, fmt.Errorf("%s: could not parse 64-bit integer: %w", lex.Loc, err)
		}
		literal = ast.NewInt64Literal(val)
	} else if is8Bit {
		// int8 literal
		val, err := strconv.ParseInt(str, base, 8)
		if err != nil {
			return nil, fmt.Errorf("%s: could not parse 8-bit integer: %w", lex.Loc, err)
		}
		literal = ast.NewInt8Literal(int8(val))
	} else {
		// int literal (32 bit)
		val, err := strconv.ParseInt(str, base, 32)
		if err != nil {
			return nil, fmt.Errorf("%s: could not parse integer: %w", lex.Loc, err)
		}
		literal = ast.NewIntLiteral(int32(val))
	}

	literal.Loc = litLoc
	return literal, nil
}

func (p *Parser) parseStringLiteral() (ast.Expression, error) {
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	litLoc := locationFromLexeme(lex)
	return &ast.Literal{Loc: litLoc, StringValue: &lex.Str}, nil
}

func (p *Parser) parseParenthesizedExpression() (ast.Expression, error) {
	// consume '('
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsPunctuation("(") {
		return nil, fmt.Errorf("%s: expected '(', got %v", lex.Loc, lex)
	}

	// parse the expression inside the parentheses
	expr, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	// consume ')'
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsPunctuation(")") {
		return nil, fmt.Errorf("%s: expected ')', got %v", lex.Loc, lex)
	}

	return expr, nil
}

func (p *Parser) parseUnaryExpression() (ast.Expression, error) {
	// consume the operator
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsOperator("!") && !lex.IsOperator("-") && !lex.IsOperator("*") {
		return nil, fmt.Errorf("%s: expected '!', '-', or '*', got %v", lex.Loc, lex)
	}
	unaryLoc := locationFromLexeme(lex)

	// parse the operand
	operand, err := p.parsePrimaryExpression()
	if err != nil {
		return nil, err
	}

	return &ast.UnaryOperation{
		Loc:      unaryLoc,
		Operator: lex.Str,
		Operand:  operand,
	}, nil
}

func (p *Parser) parseAddressOfExpression() (ast.Expression, error) {
	// consume the '&' operator
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsOperator("&") {
		return nil, fmt.Errorf("%s: expected '&', got %v", lex.Loc, lex)
	}
	addrLoc := locationFromLexeme(lex)

	// parse the operand - must be an lvalue (variable reference)
	operand, err := p.parseVariableReference()
	if err != nil {
		return nil, err
	}

	return &ast.UnaryOperation{
		Loc:      addrLoc,
		Operator: "&",
		Operand:  operand,
	}, nil
}

func (p *Parser) parseVariableDeclaration() (*ast.VariableDeclaration, error) {
	// consume 'var'
	varLex, err := p.consume()
	if err != nil {
		return nil, err
	}
	varLoc := locationFromLexeme(varLex)

	// variable name
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return nil, fmt.Errorf("%s: expected variable name, got %v", lex.Loc, lex)
	}
	name := lex.Str

	// check for optional type annotation (colon)
	var typeExpr ast.Type
	lex, err = p.peek()
	if err != nil {
		return nil, err
	}
	if lex.IsPunctuation(":") {
		// consume ':'
		_, err = p.consume()
		if err != nil {
			return nil, err
		}

		// parse type
		typeExpr, err = p.parseType()
		if err != nil {
			return nil, err
		}
	}

	// check for optional initializer
	var initializer ast.Expression
	lex, err = p.peek()
	if err != nil {
		return nil, err
	}
	if lex.IsOperator("=") {
		// consume '='
		_, err = p.consume()
		if err != nil {
			return nil, err
		}

		// parse initializer expression
		initializer, err = p.parseExpression()
		if err != nil {
			return nil, err
		}
	}

	// validate that we have either type or initializer (or both)
	if typeExpr == nil && initializer == nil {
		return nil, fmt.Errorf("%s: variable declaration must have either type annotation or initializer", varLoc)
	}

	return &ast.VariableDeclaration{
		Loc:         varLoc,
		Name:        name,
		Type:        typeExpr,
		Initializer: initializer,
	}, nil
}

func (p *Parser) parseConstantDeclaration() (*ast.ConstantDeclaration, error) {
	// consume 'val'
	valLex, err := p.consume()
	if err != nil {
		return nil, err
	}
	valLoc := locationFromLexeme(valLex)

	// constant name
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return nil, fmt.Errorf("%s: expected constant name, got %v", lex.Loc, lex)
	}
	name := lex.Str

	// check for optional type annotation (colon)
	var typeExpr ast.Type
	lex, err = p.peek()
	if err != nil {
		return nil, err
	}
	if lex.IsPunctuation(":") {
		// consume ':'
		_, err = p.consume()
		if err != nil {
			return nil, err
		}

		// parse type
		typeExpr, err = p.parseType()
		if err != nil {
			return nil, err
		}
	}

	// expect '=' for initializer
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsOperator("=") {
		return nil, fmt.Errorf("%s: expected '=' after constant name, got %v", lex.Loc, lex)
	}

	// parse initializer expression
	initializer, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	return &ast.ConstantDeclaration{
		Loc:         valLoc,
		Name:        name,
		Type:        typeExpr,
		Initializer: initializer,
	}, nil
}

func (p *Parser) parseReturnStatement() (*ast.ReturnStatement, error) {
	// consume 'return'
	retLex, err := p.consume()
	if err != nil {
		return nil, err
	}
	retLoc := locationFromLexeme(retLex)

	// Check if there's an expression to return
	lex, err := p.peek()
	if err != nil {
		return nil, err
	}

	// If the next token is a semicolon, this is a return without a value
	if lex.IsPunctuation(";") {
		return &ast.ReturnStatement{Loc: retLoc, Value: nil}, nil
	}

	// Otherwise, parse the return value expression
	expr, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	return &ast.ReturnStatement{Loc: retLoc, Value: expr}, nil
}

func (p *Parser) parseIdentifierExpression() (ast.Expression, error) {
	// Look ahead to see what follows the identifier
	if p.pos+1 < len(p.lexemes) {
		nextLex := p.lexemes[p.pos+1]
		if nextLex.IsPunctuation("(") {
			return p.parseFunctionCall()
		}
	} else {
		// Need to peek at the next lexeme from the lexer
		currentPos := p.pos
		_, err := p.consume() // consume the identifier
		if err != nil {
			return nil, err
		}
		nextLex, err := p.peek()
		if err != nil {
			return nil, err
		}
		p.pos = currentPos // reset position

		if nextLex.IsPunctuation("(") {
			return p.parseFunctionCall()
		}
	}

	// Default to variable reference if not function call
	return p.parseVariableReference()
}

func (p *Parser) parseVariableReference() (ast.Expression, error) {
	// variable name
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return nil, fmt.Errorf("%s: expected variable name, got %v", lex.Loc, lex)
	}
	varLoc := locationFromLexeme(lex)

	return &ast.VariableReference{
		Loc:  varLoc,
		Name: lex.Str,
	}, nil
}

// isValidAssignmentTarget checks if an expression can be used as an assignment target
func (p *Parser) isValidAssignmentTarget(expr ast.Expression) bool {
	switch e := expr.(type) {
	case *ast.VariableReference:
		return true
	case *ast.UnaryOperation:
		return e.Operator == "*"
	case *ast.FieldAccess:
		return true
	case *ast.IndexExpression:
		return true
	case *ast.PostfixOperator:
		return false // postfix operators can't be assignment targets
	default:
		return false
	}
}

func (p *Parser) parseIfStatement() (*ast.IfStatement, error) {
	// consume 'if'
	ifLex, err := p.consume()
	if err != nil {
		return nil, err
	}
	ifLoc := locationFromLexeme(ifLex)

	// parse condition expression
	condition, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	// parse then block
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsPunctuation("{") {
		return nil, fmt.Errorf("%s: expected '{' after if condition, got %v", lex.Loc, lex)
	}

	thenBlock, err := p.parseBlock()
	if err != nil {
		return nil, err
	}

	// check for optional else clause
	var elseBlock *ast.Block
	lex, err = p.peek()
	if err != nil {
		return nil, err
	}
	if lex.IsKeyword("else") {
		_, err = p.consume() // consume 'else'
		if err != nil {
			return nil, err
		}

		// check if this is "else if" (else followed by if)
		lex, err = p.peek()
		if err != nil {
			return nil, err
		}

		if lex.IsKeyword("if") {
			// This is "else if" - parse another if statement and wrap it in a block
			nestedIf, err := p.parseIfStatement()
			if err != nil {
				return nil, err
			}

			elseBlock = &ast.Block{
				Loc: locationFromLexeme(lex),
				Statements: []ast.Statement{
					nestedIf,
				},
			}
		} else {
			// This is regular "else" - expect opening brace
			lex, err = p.consume()
			if err != nil {
				return nil, err
			}
			if !lex.IsPunctuation("{") {
				return nil, fmt.Errorf("%s: expected '{' after else, got %v", lex.Loc, lex)
			}

			elseBlock, err = p.parseBlock()
			if err != nil {
				return nil, err
			}
		}
	}

	return &ast.IfStatement{
		Loc:       ifLoc,
		Condition: condition,
		ThenBlock: *thenBlock,
		ElseBlock: elseBlock,
	}, nil
}

func (p *Parser) parseWhileStatement() (*ast.WhileStatement, error) {
	// consume 'while'
	whileLex, err := p.consume()
	if err != nil {
		return nil, err
	}
	whileLoc := locationFromLexeme(whileLex)

	// parse condition expression
	condition, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	// parse body block
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsPunctuation("{") {
		return nil, fmt.Errorf("%s: expected '{' after while condition, got %v", lex.Loc, lex)
	}

	body, err := p.parseBlock()
	if err != nil {
		return nil, err
	}

	return &ast.WhileStatement{
		Loc:       whileLoc,
		Condition: condition,
		Body:      *body,
	}, nil
}

func (p *Parser) parseForStatement() (*ast.ForStatement, error) {
	// consume 'for'
	forLex, err := p.consume()
	if err != nil {
		return nil, err
	}
	forLoc := locationFromLexeme(forLex)

	// parse initialization statement
	var init ast.Statement
	lex, err := p.peek()
	if err != nil {
		return nil, err
	}
	if !lex.IsPunctuation(";") {
		init, err = p.parseStatement()
		if err != nil {
			return nil, err
		}
	}

	// consume ';' after init
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsPunctuation(";") {
		return nil, fmt.Errorf("%s: expected ';' after for loop initialization, got %v", lex.Loc, lex)
	}

	// parse condition expression
	var condition ast.Expression
	lex, err = p.peek()
	if err != nil {
		return nil, err
	}
	if !lex.IsPunctuation(";") {
		condition, err = p.parseExpression()
		if err != nil {
			return nil, err
		}
	}

	// consume ';' after condition
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsPunctuation(";") {
		return nil, fmt.Errorf("%s: expected ';' after for loop condition, got %v", lex.Loc, lex)
	}

	// parse update expression
	var update ast.Expression
	lex, err = p.peek()
	if err != nil {
		return nil, err
	}
	if !lex.IsPunctuation("{") {
		update, err = p.parseExpression()
		if err != nil {
			return nil, err
		}
	}

	// parse body block
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsPunctuation("{") {
		return nil, fmt.Errorf("%s: expected '{' after for loop header, got %v", lex.Loc, lex)
	}

	body, err := p.parseBlock()
	if err != nil {
		return nil, err
	}

	return &ast.ForStatement{
		Loc:       forLoc,
		Init:      init,
		Condition: condition,
		Update:    update,
		Body:      *body,
	}, nil
}

func (p *Parser) parseBlockStatement() (*ast.BlockStatement, error) {
	// consume '{'
	blockLex, err := p.consume()
	if err != nil {
		return nil, err
	}
	blockLoc := locationFromLexeme(blockLex)

	// parse the block contents
	block, err := p.parseBlock()
	if err != nil {
		return nil, err
	}

	return &ast.BlockStatement{
		Loc:   blockLoc,
		Block: *block,
	}, nil
}

func (p *Parser) parseStructDeclaration() (*ast.StructDeclaration, error) {
	// consume 'struct'
	structLex, err := p.consume()
	if err != nil {
		return nil, err
	}
	structLoc := locationFromLexeme(structLex)

	// struct name
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return nil, fmt.Errorf("%s: expected struct name, got %v", lex.Loc, lex)
	}
	name := lex.Str

	// '{'
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsPunctuation("{") {
		return nil, fmt.Errorf("%s: expected '{' after struct name, got %v", lex.Loc, lex)
	}

	// parse fields
	fields := []ast.StructField{}
	for {
		lex, err := p.peek()
		if err != nil {
			return nil, err
		}
		if lex.IsPunctuation("}") {
			break
		}

		// field name
		fieldLex, err := p.consume()
		if err != nil {
			return nil, err
		}
		if fieldLex.Type != lexer.LEX_IDENT {
			return nil, fmt.Errorf("%s: expected field name, got %v", fieldLex.Loc, fieldLex)
		}
		fieldLoc := locationFromLexeme(fieldLex)
		fieldName := fieldLex.Str

		// ':'
		lex, err = p.consume()
		if err != nil {
			return nil, err
		}
		if !lex.IsPunctuation(":") {
			return nil, fmt.Errorf("%s: expected ':' after field name, got %v", lex.Loc, lex)
		}

		// field type
		fieldType, err := p.parseType()
		if err != nil {
			return nil, err
		}

		// ';'
		lex, err = p.consume()
		if err != nil {
			return nil, err
		}
		if !lex.IsPunctuation(";") {
			return nil, fmt.Errorf("%s: expected ';' after field declaration, got %v", lex.Loc, lex)
		}

		fields = append(fields, ast.StructField{
			Loc:  fieldLoc,
			Name: fieldName,
			Type: fieldType,
		})
	}

	// consume '}'
	_, err = p.consume()
	if err != nil {
		return nil, err
	}

	return &ast.StructDeclaration{
		Loc:    structLoc,
		Name:   name,
		Fields: fields,
	}, nil
}
