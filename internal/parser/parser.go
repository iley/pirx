package parser

import (
	"fmt"
	"strconv"

	"github.com/iley/pirx/internal/lexer"
)

var (
	// TODO: Find a better way of figuring out whether a function is variadic.
	VariadicFuncs = map[string]struct{}{
		"printf": {},
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
	functions := []Function{}

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

func (p *Parser) parseFunction() (Function, error) {
	lex, err := p.consume()
	if err != nil {
		return Function{}, err
	}
	if !lex.IsKeyword("func") {
		return Function{}, fmt.Errorf("%d:%d: expected 'func', got %v", lex.Line, lex.Col, lex)
	}
	// function name
	lex, err = p.consume()
	if err != nil {
		return Function{}, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return Function{}, fmt.Errorf("%d:%d: expected function name, got %v", lex.Line, lex.Col, lex)
	}
	name := lex.Str
	// '('
	lex, err = p.consume()
	if err != nil {
		return Function{}, err
	}
	if !lex.IsPunctuation("(") {
		return Function{}, fmt.Errorf("%d:%d: expected '(', got %v", lex.Line, lex.Col, lex)
	}

	params, err := p.parseParameters()
	if err != nil {
		return Function{}, err
	}

	// ')'
	lex, err = p.consume()
	if err != nil {
		return Function{}, err
	}
	if !lex.IsPunctuation(")") {
		return Function{}, fmt.Errorf("%d:%d: expected ')', got %v", lex.Line, lex.Col, lex)
	}

	lex, err = p.peek()
	if err != nil {
		return Function{}, err
	}

	returnType := ""
	if lex.IsPunctuation(":") {
		// return type specifier
		p.consume() // ":"

		// return type
		// TODO: Support composite types.
		lex, err = p.consume()
		if err != nil {
			return Function{}, err
		}

		if lex.Type != lexer.LEX_IDENT {
			return Function{}, fmt.Errorf("%d:%d: expected type, got %v", lex.Line, lex.Col, lex)
		}
		returnType = lex.Str
	}

	// '{'
	lex, err = p.consume()
	if err != nil {
		return Function{}, err
	}
	if !lex.IsPunctuation("{") {
		return Function{}, fmt.Errorf("%d:%d: expected '{', got %v", lex.Line, lex.Col, lex)
	}

	body, err := p.parseBlock()
	if err != nil {
		return Function{}, err
	}

	return Function{
		Name:       name,
		Params:     params,
		Body:       *body,
		ReturnType: returnType,
	}, nil
}

func (p *Parser) parseParameters() ([]Param, error) {
	params := []Param{}

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

		params = append(params, Param{Name: name, Type: typeStr})

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
	_, err := p.consume()
	if err != nil {
		return nil, err
	}

	return &Block{Statements: statements}, nil
}

// statementRequiresSemicolon returns true if the statement requires a semicolon after it
func statementRequiresSemicolon(stmt Statement) bool {
	// If statements don't require semicolons since they end with a block
	if _, ok := stmt.(*IfStatement); ok {
		return false
	}
	// While statements don't require semicolons since they end with a block
	if _, ok := stmt.(*WhileStatement); ok {
		return false
	}
	// All other statements require semicolons
	return true
}

func (p *Parser) parseStatement() (Statement, error) {
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
		_, err := p.consume() // consume the "break" keyword
		if err != nil {
			return nil, err
		}
		return &BreakStatement{}, nil
	}
	// TODO: Validate that continue is only used inside a loop (likely a separate AST pass).
	if lex.IsKeyword("continue") {
		_, err := p.consume() // consume the "continue" keyword
		if err != nil {
			return nil, err
		}
		return &ContinueStatement{}, nil
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
	return &ExpressionStatement{Expression: expression}, nil
}

func (p *Parser) parseExpression() (Expression, error) {
	return p.parseExpressionWithPrecedence(0)
}

func (p *Parser) parseExpressionWithPrecedence(minPrecedence int) (Expression, error) {
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
		_, err = p.consume() // consume the operator
		if err != nil {
			return nil, err
		}

		// For left-associative operators, use precedence + 1
		// For right-associative operators, use precedence
		nextMinPrecedence := precedence + 1

		right, err := p.parseExpressionWithPrecedence(nextMinPrecedence)
		if err != nil {
			return nil, err
		}

		left = &BinaryOperation{
			Left:     left,
			Operator: operator,
			Right:    right,
		}
	}

	return left, nil
}

func isBinaryOperator(op string) bool {
	return op == "+" || op == "-" || op == "*" || op == "/" || op == "%" ||
		op == "==" || op == "!=" || op == "<" || op == ">" || op == "<=" || op == ">=" ||
		op == "&&" || op == "||"
}

func getOperatorPrecedence(op string) int {
	switch op {
	case "||":
		return 1
	case "&&":
		return 2
	case "==", "!=", "<", ">", "<=", ">=":
		return 3
	case "+", "-":
		return 4
	case "*", "/", "%":
		return 5
	default:
		return 0
	}
}

func (p *Parser) parsePrimaryExpression() (Expression, error) {
	lex, err := p.peek()
	if err != nil {
		return nil, err
	}
	switch lex.Type {
	case lexer.LEX_IDENT:
		// Look ahead to determine if this is a function call or assignment
		return p.parseIdentifierExpression()
	case lexer.LEX_NUMBER:
		return p.parseIntegerLiteral()
	case lexer.LEX_STRING:
		return p.parseStringLiteral()
	case lexer.LEX_OPERATOR:
		// TODO: Add support for unary minus.
		if lex.Str == "!" {
			return p.parseUnaryExpression()
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

func (p *Parser) parseFunctionCall() (Expression, error) {
	// function name
	lex, err := p.consume()
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

	args := []Expression{}
	_, variadic := VariadicFuncs[name]

	// check for ')'
	lex, err = p.peek()
	if err != nil {
		return nil, err
	}
	if lex.IsPunctuation(")") {
		p.consume() // consume ')'
		return &FunctionCall{FunctionName: name, Args: args, Variadic: variadic}, nil
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

	return &FunctionCall{FunctionName: name, Args: args, Variadic: variadic}, nil
}

func (p *Parser) parseIntegerLiteral() (Expression, error) {
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	val, err := strconv.Atoi(lex.Str)
	if err != nil {
		return nil, fmt.Errorf("%d:%d: could not parse integer: %w", lex.Line, lex.Col, err)
	}
	return NewIntLiteral(int64(val)), nil
}

func (p *Parser) parseStringLiteral() (Expression, error) {
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	return &Literal{StringValue: &lex.Str}, nil
}

func (p *Parser) parseParenthesizedExpression() (Expression, error) {
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

func (p *Parser) parseUnaryExpression() (Expression, error) {
	// consume the operator
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsOperator("!") {
		return nil, fmt.Errorf("%d:%d: expected '!', got %v", lex.Line, lex.Col, lex)
	}

	// parse the operand
	operand, err := p.parsePrimaryExpression()
	if err != nil {
		return nil, err
	}

	return &UnaryOperation{
		Operator: lex.Str,
		Operand:  operand,
	}, nil
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
	// TODO: Support composite types.
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

	return &ReturnStatement{Value: expr}, nil
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
			return nil, err
		}
		nextLex, err := p.peek()
		if err != nil {
			return nil, err
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
		return nil, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return nil, fmt.Errorf("%d:%d: expected variable name, got %v", lex.Line, lex.Col, lex)
	}
	varName := lex.Str

	// '='
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if !lex.IsOperator("=") {
		return nil, fmt.Errorf("%d:%d: expected '=', got %v", lex.Line, lex.Col, lex)
	}

	// value expression
	value, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	return &Assignment{
		VariableName: varName,
		Value:        value,
	}, nil
}

func (p *Parser) parseVariableReference() (Expression, error) {
	// variable name
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return nil, fmt.Errorf("%d:%d: expected variable name, got %v", lex.Line, lex.Col, lex)
	}

	return &VariableReference{
		Name: lex.Str,
	}, nil
}

func (p *Parser) parseIfStatement() (*IfStatement, error) {
	// consume 'if'
	_, err := p.consume()
	if err != nil {
		return nil, err
	}

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
	var elseBlock *Block
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

			elseBlock = &Block{
				Statements: []Statement{
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

	return &IfStatement{
		Condition: condition,
		ThenBlock: *thenBlock,
		ElseBlock: elseBlock,
	}, nil
}

func (p *Parser) parseWhileStatement() (*WhileStatement, error) {
	// consume 'while'
	_, err := p.consume()
	if err != nil {
		return nil, err
	}

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

	return &WhileStatement{
		Condition: condition,
		Body:      *body,
	}, nil
}
