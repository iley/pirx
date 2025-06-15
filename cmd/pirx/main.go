package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/iley/pirx/internal/codegen"
	"github.com/iley/pirx/internal/lexer"
	"github.com/iley/pirx/internal/parser"
)

func main() {
	output := flag.String("o", "", "output file name")
	target := flag.String("t", "arm64-darwin", "target architecture")
	flag.Parse()

	if len(flag.Args()) != 1 {
		fmt.Println("Usage: pirx [options] <input file>")
		flag.PrintDefaults()
		os.Exit(1)
	}
	inputFileName := flag.Args()[0]

	if *output == "" {
		*output = strings.TrimSuffix(inputFileName, filepath.Ext(inputFileName)) + ".s"
	}

	inputFile, err := os.Open(inputFileName)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error opening input file: %v\n", err)
		os.Exit(1)
	}
	defer inputFile.Close()

	lex := lexer.New(inputFile)
	p := parser.New(lex)
	program, err := p.ParseProgram()
	if err != nil {
		fmt.Fprintf(os.Stderr, "error parsing program: %v\n", err)
		os.Exit(1)
	}

	outputFile, err := os.Create(*output)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error creating output file: %v\n", err)
		os.Exit(1)
	}
	defer outputFile.Close()

	var visitor parser.AstVisitor
	switch *target {
	case "arm64-darwin":
		visitor = codegen.NewARM64DarwinCodegenVisitor(outputFile)
	default:
		fmt.Fprintf(os.Stderr, "unsupported target: %s\n", *target)
		os.Exit(1)
	}

	program.Accept(visitor)

	fmt.Printf("Compiled %s to %s for target %s\n", inputFileName, *output, *target)
}
