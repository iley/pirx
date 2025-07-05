package main

import (
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/iley/pirx/internal/checks"
	"github.com/iley/pirx/internal/codegen"
	"github.com/iley/pirx/internal/ir"
	"github.com/iley/pirx/internal/lexer"
	"github.com/iley/pirx/internal/parser"
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
	defer inputFile.Close()

	lex := lexer.New(inputFile)
	p := parser.New(lex)
	ast, err := p.ParseProgram()
	if err != nil {
		fmt.Fprintf(os.Stderr, "error parsing program: %v\n", err)
		os.Exit(1)
	}

	programErrors := checks.Run(ast)
	if len(programErrors) > 0 {
		for _, err := range programErrors {
			fmt.Fprintf(os.Stderr, "%s\n", err)
		}
		os.Exit(1)
	}

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
		defer outputFile.Close()
		output = outputFile
	}

	if *targetString == "ast" {
		fmt.Printf("%s\n", ast.String())
		return
	}

	programIr := ir.Generate(ast)

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

	codegen.Generate(output, target, programIr)
}
