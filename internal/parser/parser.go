package parser

import (
	"fmt"
	"strconv"

	"github.com/iley/pirx/internal/lexer"
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
	// Parse: func main() {}
	fn, err := p.parseFunction()
	if err != nil {
		return nil, err
	}
	return &Program{Functions: []*Function{fn}}, nil
}

func (p *Parser) parseFunction() (*Function, error) {
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	if lex.Type != lexer.LEX_KEYWORD || lex.Str != "func" {
		return nil, fmt.Errorf("expected 'func', got %v", lex)
	}
	// function name
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return nil, fmt.Errorf("expected function name, got %v", lex)
	}
	name := lex.Str
	// '('
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if lex.Type != lexer.LEX_PUNCTUATION || lex.Str != "(" {
		return nil, fmt.Errorf("expected '(', got %v", lex)
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
	if lex.Type != lexer.LEX_PUNCTUATION || lex.Str != ")" {
		return nil, fmt.Errorf("expected ')', got %v", lex)
	}
	// '{'
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if lex.Type != lexer.LEX_PUNCTUATION || lex.Str != "{" {
		return nil, fmt.Errorf("expected '{', got %v", lex)
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
	if lex.Type == lexer.LEX_PUNCTUATION && lex.Str == ")" {
		return params, nil
	}

	for {
		// param name
		lex, err := p.consume()
		if err != nil {
			return nil, err
		}
		if lex.Type != lexer.LEX_IDENT {
			return nil, fmt.Errorf("expected param name, got %v", lex)
		}
		name := lex.Str

		// param type
		lex, err = p.consume()
		if err != nil {
			return nil, err
		}
		if lex.Type != lexer.LEX_IDENT {
			return nil, fmt.Errorf("expected param type, got %v", lex)
		}
		typeStr := lex.Str

		params = append(params, &Param{Name: name, Type: typeStr})

		lex, err = p.peek()
		if err != nil {
			return nil, err
		}
		if lex.Type == lexer.LEX_PUNCTUATION && lex.Str == ")" {
			break
		}

		// comma
		lex, err = p.consume()
		if err != nil {
			return nil, err
		}
		if lex.Type != lexer.LEX_PUNCTUATION || lex.Str != "," {
			return nil, fmt.Errorf("expected ',', got %v", lex)
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
		if lex.Type == lexer.LEX_PUNCTUATION && lex.Str == "}" {
			break
		}

		stmt, err := p.parseStatement()
		if err != nil {
			return nil, err
		}
		statements = append(statements, stmt)
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
		return nil, err
	}
	if lex.Type == lexer.LEX_KEYWORD && lex.Str == "var" {
		return p.parseVariableDeclaration()
	}
	expression, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
	return &ExpressionStatement{Expression: expression}, nil
}

func (p *Parser) parseExpression() (Expression, error) {
	lex, err := p.peek()
	if err != nil {
		return nil, err
	}
	switch lex.Type {
	case lexer.LEX_IDENT:
		return p.parseFunctionCall()
	case lexer.LEX_NUMBER:
		return p.parseIntegerLiteral()
	case lexer.LEX_STRING:
		return p.parseStringLiteral()
	default:
		return nil, fmt.Errorf("unknown expression: %v", lex)
	}
}

func (p *Parser) parseFunctionCall() (*FunctionCall, error) {
	// function name
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return nil, fmt.Errorf("expected function name, got %v", lex)
	}
	name := lex.Str

	// '('
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if lex.Type != lexer.LEX_PUNCTUATION || lex.Str != "(" {
		return nil, fmt.Errorf("expected '(', got %v", lex)
	}

	args := []Expression{}
	// check for ')'
	lex, err = p.peek()
	if err != nil {
		return nil, err
	}
	if lex.Type == lexer.LEX_PUNCTUATION && lex.Str == ")" {
		p.consume() // consume ')'
		return &FunctionCall{FunctionName: name, Args: args}, nil
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
		if lex.Type == lexer.LEX_PUNCTUATION && lex.Str == ")" {
			break
		}

		lex, err = p.consume()
		if err != nil {
			return nil, err
		}
		if lex.Type != lexer.LEX_PUNCTUATION || lex.Str != "," {
			return nil, fmt.Errorf("expected ',' or ')', got %v", lex)
		}
	}

	// consume ')'
	_, err = p.consume()
	if err != nil {
		return nil, err
	}

	return &FunctionCall{FunctionName: name, Args: args}, nil
}

func (p *Parser) parseIntegerLiteral() (*Literal, error) {
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	val, err := strconv.Atoi(lex.Str)
	if err != nil {
		return nil, fmt.Errorf("could not parse integer: %w", err)
	}
	return &Literal{Type: LiteralTypeInt, IntValue: val}, nil
}

func (p *Parser) parseStringLiteral() (*Literal, error) {
	lex, err := p.consume()
	if err != nil {
		return nil, err
	}
	return &Literal{Type: LiteralTypeString, StringValue: lex.Str}, nil
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
		return nil, fmt.Errorf("expected variable name, got %v", lex)
	}
	name := lex.Str

	// type
	lex, err = p.consume()
	if err != nil {
		return nil, err
	}
	if lex.Type != lexer.LEX_IDENT {
		return nil, fmt.Errorf("expected type, got %v", lex)
	}
	typeStr := lex.Str

	return &VariableDeclaration{
		Name: name,
		Type: typeStr,
	}, nil
}
