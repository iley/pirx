package parser

import (
	"fmt"
	"strconv"

	"github.com/iley/pirx/internal/lexer"
)

var (
	// TODO: Find a better way of figuring out whether a function is variadic.
	VariadicFuncs = map[string]struct{}{
		"printf": struct{}{},
	}
)

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

func (p *Parser) ParseProgram() (*Program, error) {
	functions := []*Function{}

	for {
		lex, err := p.peek()
		if err != nil {
			return nil, err
		}
		if lex.Type == lexer.LEX_EOF {
			break
		}

		fn, err := p.parseFunction()
		if err != nil {
			return nil, err
		}
		functions = append(functions, fn)
	}

	return &Program{Functions: functions}, nil
}

func (p *Parser) parseFunction() (*Function, error) {
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsKeyword("func") {
		return nil, fmt.Errorf("%d:%d: expected 'func', got %v", lex.Line, lex.Col, lex)
	}
	// function name
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return nil, fmt.Errorf("%d:%d: expected function name, got %v", lex.Line, lex.Col, lex)
	}
	name := lex.Str
	// '('
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsPunctuation("(") {
		return nil, fmt.Errorf("%d:%d: expected '(', got %v", lex.Line, lex.Col, lex)
	}

	params, err := p.parseParameters()
	if err != nil {
		return nil, err
	}

	// ')'
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsPunctuation(")") {
		return nil, fmt.Errorf("%d:%d: expected ')', got %v", lex.Line, lex.Col, lex)
	}
	// '{'
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsPunctuation("{") {
		return nil, fmt.Errorf("%d:%d: expected '{', got %v", lex.Line, lex.Col, lex)
	}

	body, err := p.parseBlock()
	if err != nil {
		return nil, err
	}

	return &Function{
		Name:   name,
		Params: params,
		Body:   body,
	}, nil
}

func (p *Parser) parseParameters() ([]*Param, error) {
	params := []*Param{}

	lex, err := p.peek()
	if err != nil {
		return nil, err
	}
	if lex.IsPunctuation(")") {
		return params, nil
	}

	for {
		// param name
		lex, err := p.consume()
		if err != nil {
			return nil, err
		}
		if lex.Type != lexer.LEX_IDENT {
			return nil, fmt.Errorf("%d:%d: expected param name, got %v", lex.Line, lex.Col, lex)
		}
		name := lex.Str

		// colon
		lex, err = p.consume()
		if err != nil {
			return nil, err
		}
		if !lex.IsPunctuation(":") {
			return nil, fmt.Errorf("%d:%d: expected ':' after parameter name, got %v", lex.Line, lex.Col, lex)
		}

		// param type
		lex, err = p.consume()
		if err != nil {
			return nil, err
		}
		if lex.Type != lexer.LEX_IDENT {
			return nil, fmt.Errorf("%d:%d: expected param type, got %v", lex.Line, lex.Col, lex)
		}
		typeStr := lex.Str

		params = append(params, &Param{Name: name, Type: typeStr})

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

	return params, nil
}

func (p *Parser) parseBlock() (*Block, error) {
	statements := []Statement{}

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

		lex, err = p.consume()
		if err != nil {
			return nil, err
		}
		if !lex.IsPunctuation(";") {
			return nil, fmt.Errorf("%d:%d: expected ';' after statement, got %v", lex.Line, lex.Col, lex)
		}
	}

	// consume '}'
	_, err := p.consume()
	if err != nil {
		return nil, err
	}

	return &Block{Statements: statements}, nil
}

func (p *Parser) parseStatement() (Statement, error) {
	lex, err := p.peek()
	if err != nil {
		return Statement{}, err
	}
	if lex.IsKeyword("var") {
		varDecl, err := p.parseVariableDeclaration()
		if err != nil {
			return Statement{}, err
		}
		return Statement{VariableDeclaration: varDecl}, nil
	}
	if lex.IsKeyword("return") {
		retStmt, err := p.parseReturnStatement()
		if err != nil {
			return Statement{}, err
		}
		return Statement{ReturnStatement: retStmt}, nil
	}
	expression, err := p.parseExpression()
	if err != nil {
		return Statement{}, err
	}
	return Statement{ExpressionStatement: &ExpressionStatement{Expression: expression}}, nil
}

func (p *Parser) parseExpression() (Expression, error) {
	return p.parseExpressionWithPrecedence(0)
}

func (p *Parser) parseExpressionWithPrecedence(minPrecedence int) (Expression, error) {
	// Parse the left operand
	left, err := p.parsePrimaryExpression()
	if err != nil {
		return Expression{}, err
	}

	for {
		// Check for binary operators
		lex, err := p.peek()
		if err != nil {
			return Expression{}, err
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
		_, err = p.consume() // consume the operator
		if err != nil {
			return Expression{}, err
		}

		// For left-associative operators, use precedence + 1
		// For right-associative operators, use precedence
		nextMinPrecedence := precedence + 1

		right, err := p.parseExpressionWithPrecedence(nextMinPrecedence)
		if err != nil {
			return Expression{}, err
		}

		left = Expression{BinaryOperation: &BinaryOperation{
			Left:     left,
			Operator: operator,
			Right:    right,
		}}
	}

	return left, nil
}

func isBinaryOperator(op string) bool {
	return op == "+" || op == "-" || op == "*" || op == "/"
}

func getOperatorPrecedence(op string) int {
	switch op {
	case "+", "-":
		return 1
	case "*", "/":
		return 2
	default:
		return 0
	}
}

func (p *Parser) parsePrimaryExpression() (Expression, error) {
	lex, err := p.peek()
	if err != nil {
		return Expression{}, err
	}
	switch lex.Type {
	case lexer.LEX_IDENT:
		// Look ahead to determine if this is a function call or assignment
		return p.parseIdentifierExpression()
	case lexer.LEX_NUMBER:
		return p.parseIntegerLiteral()
	case lexer.LEX_STRING:
		return p.parseStringLiteral()
	case lexer.LEX_PUNCTUATION:
		if lex.Str == "(" {
			return p.parseParenthesizedExpression()
		}
		return Expression{}, fmt.Errorf("%d:%d: unknown expression: %v", lex.Line, lex.Col, lex)
	default:
		return Expression{}, fmt.Errorf("%d:%d: unknown expression: %v", lex.Line, lex.Col, lex)
	}
}

func (p *Parser) parseFunctionCall() (Expression, error) {
	// function name
	lex, err := p.consume()
	if err != nil {
		return Expression{}, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return Expression{}, fmt.Errorf("%d:%d: expected function name, got %v", lex.Line, lex.Col, lex)
	}
	name := lex.Str

	// '('
	lex, err = p.consume()
	if err != nil {
		return Expression{}, err
	}
	if !lex.IsPunctuation("(") {
		return Expression{}, fmt.Errorf("%d:%d: expected '(', got %v", lex.Line, lex.Col, lex)
	}

	args := []Expression{}
	_, variadic := VariadicFuncs[name]

	// check for ')'
	lex, err = p.peek()
	if err != nil {
		return Expression{}, err
	}
	if lex.IsPunctuation(")") {
		p.consume() // consume ')'
		return Expression{FunctionCall: &FunctionCall{FunctionName: name, Args: args, Variadic: variadic}}, nil
	}

	for {
		arg, err := p.parseExpression()
		if err != nil {
			return Expression{}, err
		}
		args = append(args, arg)

		lex, err := p.peek()
		if err != nil {
			return Expression{}, err
		}
		if lex.IsPunctuation(")") {
			break
		}

		lex, err = p.consume()
		if err != nil {
			return Expression{}, err
		}
		if !lex.IsPunctuation(",") {
			return Expression{}, fmt.Errorf("%d:%d: expected ',' or ')', got %v", lex.Line, lex.Col, lex)
		}
	}

	// consume ')'
	_, err = p.consume()
	if err != nil {
		return Expression{}, err
	}

	return Expression{FunctionCall: &FunctionCall{FunctionName: name, Args: args, Variadic: variadic}}, nil
}

func (p *Parser) parseIntegerLiteral() (Expression, error) {
	lex, err := p.consume()
	if err != nil {
		return Expression{}, err
	}
	val, err := strconv.Atoi(lex.Str)
	if err != nil {
		return Expression{}, fmt.Errorf("%d:%d: could not parse integer: %w", lex.Line, lex.Col, err)
	}
	return Expression{Literal: &Literal{IntValue: &val}}, nil
}

func (p *Parser) parseStringLiteral() (Expression, error) {
	lex, err := p.consume()
	if err != nil {
		return Expression{}, err
	}
	return Expression{Literal: &Literal{StringValue: &lex.Str}}, nil
}

func (p *Parser) parseParenthesizedExpression() (Expression, error) {
	// consume '('
	lex, err := p.consume()
	if err != nil {
		return Expression{}, err
	}
	if !lex.IsPunctuation("(") {
		return Expression{}, fmt.Errorf("%d:%d: expected '(', got %v", lex.Line, lex.Col, lex)
	}

	// parse the expression inside the parentheses
	expr, err := p.parseExpression()
	if err != nil {
		return Expression{}, err
	}

	// consume ')'
	lex, err = p.consume()
	if err != nil {
		return Expression{}, err
	}
	if !lex.IsPunctuation(")") {
		return Expression{}, fmt.Errorf("%d:%d: expected ')', got %v", lex.Line, lex.Col, lex)
	}

	return expr, nil
}

func (p *Parser) parseVariableDeclaration() (*VariableDeclaration, error) {
	// consume 'var'
	_, err := p.consume()
	if err != nil {
		return nil, err
	}

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
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return nil, fmt.Errorf("%d:%d: expected type, got %v", lex.Line, lex.Col, lex)
	}
	typeStr := lex.Str

	return &VariableDeclaration{
		Name: name,
		Type: typeStr,
	}, nil
}

func (p *Parser) parseReturnStatement() (*ReturnStatement, error) {
	// consume 'return'
	_, err := p.consume()
	if err != nil {
		return nil, err
	}

	// Check if there's an expression to return
	lex, err := p.peek()
	if err != nil {
		return nil, err
	}

	// If the next token is a semicolon, this is a return without a value
	if lex.IsPunctuation(";") {
		return &ReturnStatement{Value: nil}, nil
	}

	// Otherwise, parse the return value expression
	expr, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	return &ReturnStatement{Value: &expr}, nil
}

func (p *Parser) parseIdentifierExpression() (Expression, error) {
	// Look ahead to see what follows the identifier
	if p.pos+1 < len(p.lexemes) {
		nextLex := p.lexemes[p.pos+1]
		if nextLex.IsOperator("=") {
			return p.parseAssignment()
		}
		if nextLex.IsPunctuation("(") {
			return p.parseFunctionCall()
		}
	} else {
		// Need to peek at the next lexeme from the lexer
		currentPos := p.pos
		_, err := p.consume() // consume the identifier
		if err != nil {
			return Expression{}, err
		}
		nextLex, err := p.peek()
		if err != nil {
			return Expression{}, err
		}
		p.pos = currentPos // reset position

		if nextLex.IsOperator("=") {
			return p.parseAssignment()
		}
		if nextLex.IsPunctuation("(") {
			return p.parseFunctionCall()
		}
	}

	// Default to variable reference if not assignment or function call
	return p.parseVariableReference()
}

func (p *Parser) parseAssignment() (Expression, error) {
	// variable name
	lex, err := p.consume()
	if err != nil {
		return Expression{}, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return Expression{}, fmt.Errorf("%d:%d: expected variable name, got %v", lex.Line, lex.Col, lex)
	}
	varName := lex.Str

	// '='
	lex, err = p.consume()
	if err != nil {
		return Expression{}, err
	}
	if !lex.IsOperator("=") {
		return Expression{}, fmt.Errorf("%d:%d: expected '=', got %v", lex.Line, lex.Col, lex)
	}

	// value expression
	value, err := p.parseExpression()
	if err != nil {
		return Expression{}, err
	}

	return Expression{Assignment: &Assignment{
		VariableName: varName,
		Value:        value,
	}}, nil
}

func (p *Parser) parseVariableReference() (Expression, error) {
	// variable name
	lex, err := p.consume()
	if err != nil {
		return Expression{}, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return Expression{}, fmt.Errorf("%d:%d: expected variable name, got %v", lex.Line, lex.Col, lex)
	}

	return Expression{VariableReference: &VariableReference{
		Name: lex.Str,
	}}, nil
}
