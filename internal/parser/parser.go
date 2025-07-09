package parser

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/lexer"
	"github.com/iley/pirx/internal/types"
)

func locationFromLexeme(lex lexer.Lexeme) ast.Location {
	return ast.Location{Line: lex.Line, Col: lex.Col}
}

type Parser struct {
	lexer   *lexer.Lexer
	lexemes []lexer.Lexeme
	pos     int
}

func New(lex *lexer.Lexer) *Parser {
	return &Parser{lexer: lex}
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

func (p *Parser) ParseProgram() (*ast.Program, error) {
	lex, err := p.peek()
	if err != nil {
		return nil, err
	}
	programLoc := locationFromLexeme(lex)

	functions := []ast.Function{}
	externFunctions := []ast.ExternFunction{}
	structDeclarations := []ast.StructDeclaration{}

	for {
		lex, err := p.peek()
		if err != nil {
			return nil, err
		}
		if lex.Type == lexer.LEX_EOF {
			break
		}

		if lex.IsKeyword("struct") {
			structDecl, err := p.parseStructDeclaration()
			if err != nil {
				return nil, err
			}
			structDeclarations = append(structDeclarations, structDecl)
		} else if lex.IsKeyword("extern") {
			externFn, err := p.parseExternFunction()
			if err != nil {
				return nil, err
			}
			externFunctions = append(externFunctions, externFn)
		} else if lex.IsKeyword("func") {
			fn, err := p.parseFunction()
			if err != nil {
				return nil, err
			}
			functions = append(functions, fn)
		} else {
			return nil, fmt.Errorf("%d:%d: expected 'struct', 'func' or 'extern', got %v", lex.Line, lex.Col, lex)
		}
	}

	return &ast.Program{Loc: programLoc, Functions: functions, ExternFunctions: externFunctions, StructDeclarations: structDeclarations}, nil
}

func (p *Parser) parseFunction() (ast.Function, error) {
	lex, err := p.consume()
	if err != nil {
		return ast.Function{}, err
	}
	if !lex.IsKeyword("func") {
		return ast.Function{}, fmt.Errorf("%d:%d: expected 'func', got %v", lex.Line, lex.Col, lex)
	}
	funcLoc := locationFromLexeme(lex)
	// function name
	lex, err = p.consume()
	if err != nil {
		return ast.Function{}, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return ast.Function{}, fmt.Errorf("%d:%d: expected function name, got %v", lex.Line, lex.Col, lex)
	}
	name := lex.Str
	// '('
	lex, err = p.consume()
	if err != nil {
		return ast.Function{}, err
	}
	if !lex.IsPunctuation("(") {
		return ast.Function{}, fmt.Errorf("%d:%d: expected '(', got %v", lex.Line, lex.Col, lex)
	}

	args, err := p.parseArguments()
	if err != nil {
		return ast.Function{}, err
	}

	// ')'
	lex, err = p.consume()
	if err != nil {
		return ast.Function{}, err
	}
	if !lex.IsPunctuation(")") {
		return ast.Function{}, fmt.Errorf("%d:%d: expected ')', got %v", lex.Line, lex.Col, lex)
	}

	lex, err = p.peek()
	if err != nil {
		return ast.Function{}, err
	}

	var returnType types.Type
	if lex.IsPunctuation(":") {
		// return type specifier
		p.consume() // ":"

		// return type
		returnType, err = p.parseType()
		if err != nil {
			return ast.Function{}, err
		}
	}

	// '{'
	lex, err = p.consume()
	if err != nil {
		return ast.Function{}, err
	}
	if !lex.IsPunctuation("{") {
		return ast.Function{}, fmt.Errorf("%d:%d: expected '{', got %v", lex.Line, lex.Col, lex)
	}

	body, err := p.parseBlock()
	if err != nil {
		return ast.Function{}, err
	}

	return ast.Function{
		Loc:        funcLoc,
		Name:       name,
		Args:       args,
		Body:       *body,
		ReturnType: returnType,
	}, nil
}

func (p *Parser) parseExternFunction() (ast.ExternFunction, error) {
	// consume 'extern'
	lex, err := p.consume()
	if err != nil {
		return ast.ExternFunction{}, err
	}
	if !lex.IsKeyword("extern") {
		return ast.ExternFunction{}, fmt.Errorf("%d:%d: expected 'extern', got %v", lex.Line, lex.Col, lex)
	}
	externLoc := locationFromLexeme(lex)

	// consume 'func'
	lex, err = p.consume()
	if err != nil {
		return ast.ExternFunction{}, err
	}
	if !lex.IsKeyword("func") {
		return ast.ExternFunction{}, fmt.Errorf("%d:%d: expected 'func' after 'extern', got %v", lex.Line, lex.Col, lex)
	}

	// function name
	lex, err = p.consume()
	if err != nil {
		return ast.ExternFunction{}, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return ast.ExternFunction{}, fmt.Errorf("%d:%d: expected function name, got %v", lex.Line, lex.Col, lex)
	}
	name := lex.Str

	// '('
	lex, err = p.consume()
	if err != nil {
		return ast.ExternFunction{}, err
	}
	if !lex.IsPunctuation("(") {
		return ast.ExternFunction{}, fmt.Errorf("%d:%d: expected '(', got %v", lex.Line, lex.Col, lex)
	}

	args, err := p.parseArguments()
	if err != nil {
		return ast.ExternFunction{}, err
	}

	// ')'
	lex, err = p.consume()
	if err != nil {
		return ast.ExternFunction{}, err
	}
	if !lex.IsPunctuation(")") {
		return ast.ExternFunction{}, fmt.Errorf("%d:%d: expected ')', got %v", lex.Line, lex.Col, lex)
	}

	// Return type is optional for extern functions (void functions)
	lex, err = p.peek()
	if err != nil {
		return ast.ExternFunction{}, err
	}

	var returnType types.Type
	if lex.IsPunctuation(":") {
		// consume ":"
		p.consume()

		// return type
		returnType, err = p.parseType()
		if err != nil {
			return ast.ExternFunction{}, err
		}
	}

	// Require semicolon after extern function declaration
	lex, err = p.consume()
	if err != nil {
		return ast.ExternFunction{}, err
	}
	if !lex.IsPunctuation(";") {
		return ast.ExternFunction{}, fmt.Errorf("%d:%d: expected ';' after extern function declaration, got %v", lex.Line, lex.Col, lex)
	}

	return ast.ExternFunction{
		Loc:        externLoc,
		Name:       name,
		Args:       args,
		ReturnType: returnType,
	}, nil
}

func (p *Parser) parseType() (types.Type, error) {
	lex, err := p.peek()
	if err != nil {
		return nil, err
	}

	// Check for pointer type (starts with '*')
	if lex.IsOperator("*") {
		p.consume() // consume '*'

		// Parse the underlying type
		underlyingType, err := p.parseType()
		if err != nil {
			return nil, err
		}

		return types.NewPointerType(underlyingType), nil
	}

	// Parse base type (identifier)
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return nil, fmt.Errorf("%d:%d: expected type, got %v", lex.Line, lex.Col, lex)
	}

	return types.NewBaseType(lex.Str), nil
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
			return nil, fmt.Errorf("%d:%d: expected arg name, got %v", lex.Line, lex.Col, lex)
		}
		argLoc := locationFromLexeme(lex)
		name := lex.Str

		// colon
		lex, err = p.consume()
		if err != nil {
			return nil, err
		}
		if !lex.IsPunctuation(":") {
			return nil, fmt.Errorf("%d:%d: expected ':' after parameter name, got %v", lex.Line, lex.Col, lex)
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
			return nil, fmt.Errorf("%d:%d: expected ',' or ')', got %v", lex.Line, lex.Col, lex)
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
				return nil, fmt.Errorf("%d:%d: expected ';' after statement, got %v", lex.Line, lex.Col, lex)
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
			target, err := p.convertExpressionToLValue(left)
			if err != nil {
				return nil, err
			}
			left = &ast.Assignment{
				Loc:    operatorLoc,
				Target: target,
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
	lex, err := p.peek()
	if err != nil {
		return nil, err
	}
	switch lex.Type {
	case lexer.LEX_KEYWORD:
		if lex.IsKeyword("true") {
			p.consume()
			return ast.NewBoolLiteral(true), nil
		} else if lex.IsKeyword("false") {
			p.consume()
			return ast.NewBoolLiteral(false), nil
		}
		return nil, fmt.Errorf("%d:%d: unexpected keyword %s when parsing a primary expression", lex.Line, lex.Col, lex.Str)
	case lexer.LEX_IDENT:
		// Look ahead to determine if this is a function call or assignment
		return p.parseIdentifierExpression()
	case lexer.LEX_NUMBER:
		return p.parseIntegerLiteral()
	case lexer.LEX_STRING:
		return p.parseStringLiteral()
	case lexer.LEX_OPERATOR:
		if lex.Str == "!" || lex.Str == "-" || lex.Str == "*" {
			return p.parseUnaryExpression()
		}
		if lex.Str == "&" {
			return p.parseAddressOfExpression()
		}
		return nil, fmt.Errorf("%d:%d: unknown expression: %v", lex.Line, lex.Col, lex)
	case lexer.LEX_PUNCTUATION:
		if lex.Str == "(" {
			return p.parseParenthesizedExpression()
		}
		return nil, fmt.Errorf("%d:%d: unknown expression: %v", lex.Line, lex.Col, lex)
	default:
		return nil, fmt.Errorf("%d:%d: unknown expression: %v", lex.Line, lex.Col, lex)
	}
}

func (p *Parser) parseFunctionCall() (ast.Expression, error) {
	// function name
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return nil, fmt.Errorf("%d:%d: expected function name, got %v", lex.Line, lex.Col, lex)
	}
	callLoc := locationFromLexeme(lex)
	name := lex.Str

	// '('
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsPunctuation("(") {
		return nil, fmt.Errorf("%d:%d: expected '(', got %v", lex.Line, lex.Col, lex)
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
			return nil, fmt.Errorf("%d:%d: expected ',' or ')', got %v", lex.Line, lex.Col, lex)
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
	str := lex.Str
	if is64Bit {
		str, _ = strings.CutSuffix(lex.Str, "l")
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
			return nil, fmt.Errorf("%d:%d: could not parse 64-bit integer: %w", lex.Line, lex.Col, err)
		}
		literal = ast.NewInt64Literal(val)
	} else {
		// int literal (32 bit)
		val, err := strconv.ParseInt(str, base, 32)
		if err != nil {
			return nil, fmt.Errorf("%d:%d: could not parse integer: %w", lex.Line, lex.Col, err)
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
		return nil, fmt.Errorf("%d:%d: expected '(', got %v", lex.Line, lex.Col, lex)
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
		return nil, fmt.Errorf("%d:%d: expected ')', got %v", lex.Line, lex.Col, lex)
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
		return nil, fmt.Errorf("%d:%d: expected '!', '-', or '*', got %v", lex.Line, lex.Col, lex)
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
		return nil, fmt.Errorf("%d:%d: expected '&', got %v", lex.Line, lex.Col, lex)
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
		return nil, fmt.Errorf("%d:%d: expected variable name, got %v", lex.Line, lex.Col, lex)
	}
	name := lex.Str

	// colon
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsPunctuation(":") {
		return nil, fmt.Errorf("%d:%d: expected ':' after variable name, got %v", lex.Line, lex.Col, lex)
	}

	// type
	typeExpr, err := p.parseType()
	if err != nil {
		return nil, err
	}

	return &ast.VariableDeclaration{
		Loc:  varLoc,
		Name: name,
		Type: typeExpr,
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
		return nil, fmt.Errorf("%d:%d: expected variable name, got %v", lex.Line, lex.Col, lex)
	}
	varLoc := locationFromLexeme(lex)

	return &ast.VariableReference{
		Loc:  varLoc,
		Name: lex.Str,
	}, nil
}

// convertExpressionToLValue converts an expression to an lvalue for assignment targets
func (p *Parser) convertExpressionToLValue(expr ast.Expression) (ast.LValue, error) {
	switch e := expr.(type) {
	case *ast.VariableReference:
		return &ast.VariableLValue{
			Loc:  e.GetLocation(),
			Name: e.Name,
		}, nil
	case *ast.UnaryOperation:
		if e.Operator == "*" {
			return &ast.DereferenceLValue{
				Loc:        e.GetLocation(),
				Expression: e.Operand,
			}, nil
		}
		return nil, fmt.Errorf("%d:%d: invalid assignment target: %s", e.GetLocation().Line, e.GetLocation().Col, e.String())
	default:
		return nil, fmt.Errorf("%d:%d: invalid assignment target: %s", expr.GetLocation().Line, expr.GetLocation().Col, expr.String())
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
		return nil, fmt.Errorf("%d:%d: expected '{' after if condition, got %v", lex.Line, lex.Col, lex)
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
				return nil, fmt.Errorf("%d:%d: expected '{' after else, got %v", lex.Line, lex.Col, lex)
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
		return nil, fmt.Errorf("%d:%d: expected '{' after while condition, got %v", lex.Line, lex.Col, lex)
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

func (p *Parser) parseStructDeclaration() (ast.StructDeclaration, error) {
	// consume 'struct'
	structLex, err := p.consume()
	if err != nil {
		return ast.StructDeclaration{}, err
	}
	structLoc := locationFromLexeme(structLex)

	// struct name
	lex, err := p.consume()
	if err != nil {
		return ast.StructDeclaration{}, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return ast.StructDeclaration{}, fmt.Errorf("%d:%d: expected struct name, got %v", lex.Line, lex.Col, lex)
	}
	name := lex.Str

	// '{'
	lex, err = p.consume()
	if err != nil {
		return ast.StructDeclaration{}, err
	}
	if !lex.IsPunctuation("{") {
		return ast.StructDeclaration{}, fmt.Errorf("%d:%d: expected '{' after struct name, got %v", lex.Line, lex.Col, lex)
	}

	// parse fields
	fields := []ast.StructField{}
	for {
		lex, err := p.peek()
		if err != nil {
			return ast.StructDeclaration{}, err
		}
		if lex.IsPunctuation("}") {
			break
		}

		// field name
		fieldLex, err := p.consume()
		if err != nil {
			return ast.StructDeclaration{}, err
		}
		if fieldLex.Type != lexer.LEX_IDENT {
			return ast.StructDeclaration{}, fmt.Errorf("%d:%d: expected field name, got %v", fieldLex.Line, fieldLex.Col, fieldLex)
		}
		fieldLoc := locationFromLexeme(fieldLex)
		fieldName := fieldLex.Str

		// ':'
		lex, err = p.consume()
		if err != nil {
			return ast.StructDeclaration{}, err
		}
		if !lex.IsPunctuation(":") {
			return ast.StructDeclaration{}, fmt.Errorf("%d:%d: expected ':' after field name, got %v", lex.Line, lex.Col, lex)
		}

		// field type
		fieldType, err := p.parseType()
		if err != nil {
			return ast.StructDeclaration{}, err
		}

		// ';'
		lex, err = p.consume()
		if err != nil {
			return ast.StructDeclaration{}, err
		}
		if !lex.IsPunctuation(";") {
			return ast.StructDeclaration{}, fmt.Errorf("%d:%d: expected ';' after field declaration, got %v", lex.Line, lex.Col, lex)
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
		return ast.StructDeclaration{}, err
	}

	return ast.StructDeclaration{
		Loc:    structLoc,
		Name:   name,
		Fields: fields,
	}, nil
}
