package main

import (
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/iley/pirx/internal/codegen"
	"github.com/iley/pirx/internal/ir"
	"github.com/iley/pirx/internal/lexer"
	"github.com/iley/pirx/internal/parser"
	"github.com/iley/pirx/internal/typechecker"
)

func main() {
	outputString := flag.String("o", "", "output file name")
	noOptimize := flag.Bool("O0", false, "don't optimize the code")
	targetString := flag.String("t", "aarch64-darwin", "target architecture")
	flag.Parse()

	if len(flag.Args()) != 1 {
		fmt.Println("Usage: pirx [options] <input file>")
		flag.PrintDefaults()
		os.Exit(1)
	}
	inputFileName := flag.Args()[0]

	inputFile, err := os.Open(inputFileName)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error opening input file: %v\n", err)
		os.Exit(1)
	}
	defer func() {
		if err := inputFile.Close(); err != nil {
			fmt.Fprintf(os.Stderr, "warning: failed to close input file: %v\n", err)
		}
	}()

	lex := lexer.New(inputFile, inputFileName)
	p := parser.New(lex)
	ast, err := p.ParseProgram()
	if err != nil {
		fmt.Fprintf(os.Stderr, "error parsing program: %v\n", err)
		os.Exit(1)
	}

	typedAst, programErrors := typechecker.Run(ast)
	if len(programErrors) > 0 {
		for _, err := range programErrors {
			fmt.Fprintf(os.Stderr, "%s\n", err)
		}
		os.Exit(1)
	}
	ast = typedAst

	var output io.Writer

	if *outputString == "-" {
		output = os.Stdout
	} else {
		if *outputString == "" {
			*outputString = strings.TrimSuffix(inputFileName, filepath.Ext(inputFileName)) + ".s"
		}

		outputFile, err := os.Create(*outputString)
		if err != nil {
			fmt.Fprintf(os.Stderr, "error creating output file: %v\n", err)
			os.Exit(1)
		}
		defer func() {
			if err := outputFile.Close(); err != nil {
				fmt.Fprintf(os.Stderr, "warning: failed to close output file: %v\n", err)
			}
		}()
		output = outputFile
	}

	if *targetString == "ast" {
		fmt.Printf("%s\n", ast.String())
		return
	}

	programIr, err := ir.Generate(ast)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%v\n", err)
	}

	if !*noOptimize {
		programIr = ir.Optimize(programIr)
	}

	if *targetString == "ir" {
		programIr.Print(output)
		return
	}

	target, err := codegen.TargetFromName(*targetString)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error parsing target: %v\n", err)
		os.Exit(1)
	}

	err = codegen.Generate(output, target, programIr)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error generating machine code: %v\n", err)
		os.Exit(1)
	}
}
