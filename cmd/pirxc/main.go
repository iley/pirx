package main

import (
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/iley/pirx/internal/codegen"
	"github.com/iley/pirx/internal/desugar"
	"github.com/iley/pirx/internal/ir"
	"github.com/iley/pirx/internal/lexer"
	"github.com/iley/pirx/internal/parser"
	"github.com/iley/pirx/internal/typechecker"
)

func getDefaultTarget() string {
	switch runtime.GOOS {
	case "darwin":
		return "aarch64-darwin"
	case "linux":
		return "x86_64-linux"
	default:
		return "x86_64-linux"
	}
}

func main() {
	outputString := flag.String("o", "", "output file name")
	noOptimize := flag.Bool("O0", false, "don't optimize the code")
	targetString := flag.String("t", getDefaultTarget(), "target architecture")
	flag.Parse()

	if len(flag.Args()) < 1 {
		fmt.Fprintln(os.Stderr, "Usage: pirx [options] <input file>")
		flag.PrintDefaults()
		os.Exit(1)
	}

	optLevel := codegen.OptEnabled
	if *noOptimize {
		optLevel = codegen.OptDisabled
	}

	inputFileNames := flag.Args()
	if len(inputFileNames) > 1 && *outputString == "" {
		fmt.Fprintln(os.Stderr, "When more than one input file name is provided, you must specify an output file name via -o")
		os.Exit(1)
	}

	var output io.Writer

	if *outputString == "-" {
		output = os.Stdout
	} else {
		if *outputString == "" {
			// We know at this point that there is only one input file name.
			*outputString = strings.TrimSuffix(inputFileNames[0], filepath.Ext(inputFileNames[0])) + ".s"
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

	p := parser.New()
	for _, inputFileName := range inputFileNames {
		inputFile, err := os.Open(inputFileName)
		if err != nil {
			fmt.Fprintf(os.Stderr, "error opening input file: %v\n", err)
			os.Exit(1)
		}
		defer inputFile.Close()

		lex := lexer.New(inputFile, inputFileName)
		err = p.Parse(lex)
		if err != nil {
			fmt.Fprintf(os.Stderr, "error parsing program: %v\n", err)
			os.Exit(1)
		}
	}
	ast := p.GetProgram()

	// If we only need to output the AST, stop immediately after parsing.
	if *targetString == "ast" {
		fmt.Printf("%s\n", ast.String())
		return
	}

	tc := typechecker.NewTypeChecker(ast)
	typedAst, programErrors := tc.Check()
	if len(programErrors) > 0 {
		for _, err := range programErrors {
			fmt.Fprintf(os.Stderr, "%s\n", err)
		}
		os.Exit(1)
	}
	ast = typedAst

	// Desugaring must run after the typechecker because it relies on types (e.g. for sizeof()).
	var desugarErrors []error
	ast, desugarErrors = desugar.Run(ast)
	if len(desugarErrors) > 0 {
		for _, err := range desugarErrors {
			fmt.Fprintf(os.Stderr, "%s\n", err)
		}
		os.Exit(1)
	}

	if *targetString == "final_ast" {
		fmt.Printf("%s\n", ast.String())
		return
	}

	irg := ir.NewGenerator()
	programIr, codegenErrors := irg.Generate(ast)
	if len(codegenErrors) > 0 {
		for _, err := range codegenErrors {
			fmt.Fprintf(os.Stderr, "%s\n", err)
		}
		os.Exit(1)
	}

	if optLevel == codegen.OptEnabled {
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

	err = codegen.Generate(output, target, programIr, optLevel)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error generating machine code: %v\n", err)
		os.Exit(1)
	}
}
