package parser

import (
	"fmt"
	"strconv"

	"github.com/iley/pirx/internal/lexer"
)

func locationFromLexeme(lex lexer.Lexeme) Location {
	return Location{Line: lex.Line, Col: lex.Col}
}

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
	lex, err := p.peek()
	if err != nil {
		return nil, err
	}
	programLoc := locationFromLexeme(lex)

	functions := []Function{}
	externFunctions := []ExternFunction{}
	structDeclarations := []StructDeclaration{}

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

	return &Program{Loc: programLoc, Functions: functions, ExternFunctions: externFunctions, StructDeclarations: structDeclarations}, nil
}

func (p *Parser) parseFunction() (Function, error) {
	lex, err := p.consume()
	if err != nil {
		return Function{}, err
	}
	if !lex.IsKeyword("func") {
		return Function{}, fmt.Errorf("%d:%d: expected 'func', got %v", lex.Line, lex.Col, lex)
	}
	funcLoc := locationFromLexeme(lex)
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

	args, err := p.parseArguments()
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
		Loc:        funcLoc,
		Name:       name,
		Args:       args,
		Body:       *body,
		ReturnType: returnType,
	}, nil
}

func (p *Parser) parseExternFunction() (ExternFunction, error) {
	// consume 'extern'
	lex, err := p.consume()
	if err != nil {
		return ExternFunction{}, err
	}
	if !lex.IsKeyword("extern") {
		return ExternFunction{}, fmt.Errorf("%d:%d: expected 'extern', got %v", lex.Line, lex.Col, lex)
	}
	externLoc := locationFromLexeme(lex)

	// consume 'func'
	lex, err = p.consume()
	if err != nil {
		return ExternFunction{}, err
	}
	if !lex.IsKeyword("func") {
		return ExternFunction{}, fmt.Errorf("%d:%d: expected 'func' after 'extern', got %v", lex.Line, lex.Col, lex)
	}

	// function name
	lex, err = p.consume()
	if err != nil {
		return ExternFunction{}, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return ExternFunction{}, fmt.Errorf("%d:%d: expected function name, got %v", lex.Line, lex.Col, lex)
	}
	name := lex.Str

	// '('
	lex, err = p.consume()
	if err != nil {
		return ExternFunction{}, err
	}
	if !lex.IsPunctuation("(") {
		return ExternFunction{}, fmt.Errorf("%d:%d: expected '(', got %v", lex.Line, lex.Col, lex)
	}

	args, err := p.parseArguments()
	if err != nil {
		return ExternFunction{}, err
	}

	// ')'
	lex, err = p.consume()
	if err != nil {
		return ExternFunction{}, err
	}
	if !lex.IsPunctuation(")") {
		return ExternFunction{}, fmt.Errorf("%d:%d: expected ')', got %v", lex.Line, lex.Col, lex)
	}

	// Return type is optional for extern functions (void functions)
	lex, err = p.peek()
	if err != nil {
		return ExternFunction{}, err
	}
	
	returnType := ""
	if lex.IsPunctuation(":") {
		// consume ":"
		p.consume()
		
		// return type
		lex, err = p.consume()
		if err != nil {
			return ExternFunction{}, err
		}
		if lex.Type != lexer.LEX_IDENT {
			return ExternFunction{}, fmt.Errorf("%d:%d: expected return type, got %v", lex.Line, lex.Col, lex)
		}
		returnType = lex.Str
	}

	// Require semicolon after extern function declaration
	lex, err = p.consume()
	if err != nil {
		return ExternFunction{}, err
	}
	if !lex.IsPunctuation(";") {
		return ExternFunction{}, fmt.Errorf("%d:%d: expected ';' after extern function declaration, got %v", lex.Line, lex.Col, lex)
	}

	return ExternFunction{
		Loc:        externLoc,
		Name:       name,
		Args:       args,
		ReturnType: returnType,
	}, nil
}

func (p *Parser) parseArguments() ([]Arg, error) {
	args := []Arg{}

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
		lex, err = p.consume()
		if err != nil {
			return nil, err
		}
		if lex.Type != lexer.LEX_IDENT {
			return nil, fmt.Errorf("%d:%d: expected arg type, got %v", lex.Line, lex.Col, lex)
		}
		typeStr := lex.Str

		args = append(args, Arg{Loc: argLoc, Name: name, Type: typeStr})

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

func (p *Parser) parseBlock() (*Block, error) {
	lex, err := p.peek()
	if err != nil {
		return nil, err
	}
	blockLoc := locationFromLexeme(lex)

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
	_, err = p.consume()
	if err != nil {
		return nil, err
	}

	return &Block{Loc: blockLoc, Statements: statements}, nil
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
		breakLex, err := p.consume() // consume the "break" keyword
		if err != nil {
			return nil, err
		}
		return &BreakStatement{Loc: locationFromLexeme(breakLex)}, nil
	}
	// TODO: Validate that continue is only used inside a loop (likely a separate AST pass).
	if lex.IsKeyword("continue") {
		continueLex, err := p.consume() // consume the "continue" keyword
		if err != nil {
			return nil, err
		}
		return &ContinueStatement{Loc: locationFromLexeme(continueLex)}, nil
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
	return &ExpressionStatement{Loc: expression.GetLocation(), Expression: expression}, nil
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
		operatorLex, err := p.consume() // consume the operator
		if err != nil {
			return nil, err
		}
		operatorLoc := locationFromLexeme(operatorLex)

		// For left-associative operators, use precedence + 1
		// For right-associative operators, use precedence
		nextMinPrecedence := precedence + 1

		right, err := p.parseExpressionWithPrecedence(nextMinPrecedence)
		if err != nil {
			return nil, err
		}

		left = &BinaryOperation{
			Loc:      operatorLoc,
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
	case lexer.LEX_KEYWORD:
		if lex.IsKeyword("true") {
			p.consume()
			return NewBoolLiteral(true), nil
		} else if lex.IsKeyword("false") {
			p.consume()
			return NewBoolLiteral(false), nil
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

	args := []Expression{}
	_, variadic := VariadicFuncs[name]

	// check for ')'
	lex, err = p.peek()
	if err != nil {
		return nil, err
	}
	if lex.IsPunctuation(")") {
		p.consume() // consume ')'
		return &FunctionCall{Loc: callLoc, FunctionName: name, Args: args, Variadic: variadic}, nil
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

	return &FunctionCall{Loc: callLoc, FunctionName: name, Args: args, Variadic: variadic}, nil
}

func (p *Parser) parseIntegerLiteral() (Expression, error) {
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	litLoc := locationFromLexeme(lex)
	val, err := strconv.Atoi(lex.Str)
	if err != nil {
		return nil, fmt.Errorf("%d:%d: could not parse integer: %w", lex.Line, lex.Col, err)
	}
	literal := NewIntLiteral(int64(val))
	literal.Loc = litLoc
	return literal, nil
}

func (p *Parser) parseStringLiteral() (Expression, error) {
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	litLoc := locationFromLexeme(lex)
	return &Literal{Loc: litLoc, StringValue: &lex.Str}, nil
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
	unaryLoc := locationFromLexeme(lex)

	// parse the operand
	operand, err := p.parsePrimaryExpression()
	if err != nil {
		return nil, err
	}

	return &UnaryOperation{
		Loc:      unaryLoc,
		Operator: lex.Str,
		Operand:  operand,
	}, nil
}

func (p *Parser) parseVariableDeclaration() (*VariableDeclaration, error) {
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
		Loc:  varLoc,
		Name: name,
		Type: typeStr,
	}, nil
}

func (p *Parser) parseReturnStatement() (*ReturnStatement, error) {
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
		return &ReturnStatement{Loc: retLoc, Value: nil}, nil
	}

	// Otherwise, parse the return value expression
	expr, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	return &ReturnStatement{Loc: retLoc, Value: expr}, nil
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
	assignLoc := locationFromLexeme(lex)
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
		Loc:          assignLoc,
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
	varLoc := locationFromLexeme(lex)

	return &VariableReference{
		Loc:  varLoc,
		Name: lex.Str,
	}, nil
}

func (p *Parser) parseIfStatement() (*IfStatement, error) {
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
				Loc: locationFromLexeme(lex),
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
		Loc:       ifLoc,
		Condition: condition,
		ThenBlock: *thenBlock,
		ElseBlock: elseBlock,
	}, nil
}

func (p *Parser) parseWhileStatement() (*WhileStatement, error) {
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

	return &WhileStatement{
		Loc:       whileLoc,
		Condition: condition,
		Body:      *body,
	}, nil
}

func (p *Parser) parseStructDeclaration() (StructDeclaration, error) {
	// consume 'struct'
	structLex, err := p.consume()
	if err != nil {
		return StructDeclaration{}, err
	}
	structLoc := locationFromLexeme(structLex)

	// struct name
	lex, err := p.consume()
	if err != nil {
		return StructDeclaration{}, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return StructDeclaration{}, fmt.Errorf("%d:%d: expected struct name, got %v", lex.Line, lex.Col, lex)
	}
	name := lex.Str

	// '{'
	lex, err = p.consume()
	if err != nil {
		return StructDeclaration{}, err
	}
	if !lex.IsPunctuation("{") {
		return StructDeclaration{}, fmt.Errorf("%d:%d: expected '{' after struct name, got %v", lex.Line, lex.Col, lex)
	}

	// parse fields
	fields := []StructField{}
	for {
		lex, err := p.peek()
		if err != nil {
			return StructDeclaration{}, err
		}
		if lex.IsPunctuation("}") {
			break
		}

		// field name
		fieldLex, err := p.consume()
		if err != nil {
			return StructDeclaration{}, err
		}
		if fieldLex.Type != lexer.LEX_IDENT {
			return StructDeclaration{}, fmt.Errorf("%d:%d: expected field name, got %v", fieldLex.Line, fieldLex.Col, fieldLex)
		}
		fieldLoc := locationFromLexeme(fieldLex)
		fieldName := fieldLex.Str

		// ':'
		lex, err = p.consume()
		if err != nil {
			return StructDeclaration{}, err
		}
		if !lex.IsPunctuation(":") {
			return StructDeclaration{}, fmt.Errorf("%d:%d: expected ':' after field name, got %v", lex.Line, lex.Col, lex)
		}

		// field type
		lex, err = p.consume()
		if err != nil {
			return StructDeclaration{}, err
		}
		if lex.Type != lexer.LEX_IDENT {
			return StructDeclaration{}, fmt.Errorf("%d:%d: expected field type, got %v", lex.Line, lex.Col, lex)
		}
		fieldType := lex.Str

		// ';'
		lex, err = p.consume()
		if err != nil {
			return StructDeclaration{}, err
		}
		if !lex.IsPunctuation(";") {
			return StructDeclaration{}, fmt.Errorf("%d:%d: expected ';' after field declaration, got %v", lex.Line, lex.Col, lex)
		}

		fields = append(fields, StructField{
			Loc:  fieldLoc,
			Name: fieldName,
			Type: fieldType,
		})
	}

	// consume '}'
	_, err = p.consume()
	if err != nil {
		return StructDeclaration{}, err
	}

	return StructDeclaration{
		Loc:    structLoc,
		Name:   name,
		Fields: fields,
	}, nil
}
