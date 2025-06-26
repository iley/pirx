package main

import (
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"syscall"
)

// CompilationConfig holds platform-specific compilation settings
type CompilationConfig struct {
	Assembler        string
	AssemblerFlags   []string
	Linker           string
	LinkerFlags      []string
	ExecutableSuffix string
}

// getCompilationConfig returns compilation configuration for the current platform
func getCompilationConfig() (*CompilationConfig, error) {
	switch runtime.GOOS {
	case "darwin":
		switch runtime.GOARCH {
		case "arm64":
			return &CompilationConfig{
				Assembler:        "as",
				AssemblerFlags:   []string{"-arch", "arm64", "-g"},
				Linker:           "ld",
				LinkerFlags:      []string{"-lSystem", "-syslibroot", getSDKPath(), "-arch", "arm64"},
				ExecutableSuffix: "",
			}, nil
		case "amd64":
			return &CompilationConfig{
				Assembler:        "as",
				AssemblerFlags:   []string{"-arch", "x86_64"},
				Linker:           "ld",
				LinkerFlags:      []string{"-lSystem", "-syslibroot", getSDKPath(), "-arch", "x86_64"},
				ExecutableSuffix: "",
			}, nil
		}
	case "linux":
		return &CompilationConfig{
			Assembler:        "as",
			AssemblerFlags:   []string{},
			Linker:           "ld",
			LinkerFlags:      []string{},
			ExecutableSuffix: "",
		}, nil
	}

	return nil, fmt.Errorf("unsupported platform: %s/%s", runtime.GOOS, runtime.GOARCH)
}

// getSDKPath returns the macOS SDK path
func getSDKPath() string {
	cmd := exec.Command("xcrun", "-sdk", "macosx", "--show-sdk-path")
	output, err := cmd.Output()
	if err != nil {
		return ""
	}
	return strings.TrimSpace(string(output))
}

// TestCase represents a single test case
type TestCase struct {
	Name         string
	PirxFile     string
	ExpectedFile string
}

// discoverTests finds all test cases in the tests directory
func discoverTests(testsDir string) ([]TestCase, error) {
	var tests []TestCase

	err := filepath.WalkDir(testsDir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}

		if !d.IsDir() && strings.HasSuffix(path, ".pirx") {
			// Extract base name without extension
			baseName := strings.TrimSuffix(filepath.Base(path), ".pirx")
			expectedFile := filepath.Join(testsDir, baseName+".out")

			// Check if expected output file exists
			if _, err := os.Stat(expectedFile); err == nil {
				tests = append(tests, TestCase{
					Name:         baseName,
					PirxFile:     path,
					ExpectedFile: expectedFile,
				})
			}
		}

		return nil
	})

	return tests, err
}

// compileTest compiles a pirx file to executable
func compileTest(config *CompilationConfig, testCase TestCase, testsDir string) (string, []string, error) {
	baseName := filepath.Base(testCase.Name)
	asmFile := filepath.Join(testsDir, baseName+".s")
	objFile := filepath.Join(testsDir, baseName+".o")
	binFile := filepath.Join(testsDir, baseName+config.ExecutableSuffix)

	// Keep track of generated files for cleanup
	generatedFiles := []string{asmFile, objFile, binFile}

	// Step 1: Compile .pirx to .s using pirx compiler
	pirxCmd := exec.Command("go", "run", "github.com/iley/pirx/cmd/pirx", "-o", asmFile, testCase.PirxFile)
	if output, err := pirxCmd.CombinedOutput(); err != nil {
		return "", generatedFiles, fmt.Errorf("pirx compilation failed: %w\nOutput: %s", err, string(output))
	}

	// Step 2: Assemble .s to .o
	asArgs := append(config.AssemblerFlags, "-o", objFile, asmFile)
	asCmd := exec.Command(config.Assembler, asArgs...)
	if output, err := asCmd.CombinedOutput(); err != nil {
		return "", generatedFiles, fmt.Errorf("assembly failed: %w\nOutput: %s", err, string(output))
	}

	// Step 3: Link .o to executable
	ldArgs := append([]string{"-o", binFile, objFile}, config.LinkerFlags...)
	ldCmd := exec.Command(config.Linker, ldArgs...)
	if output, err := ldCmd.CombinedOutput(); err != nil {
		return "", generatedFiles, fmt.Errorf("linking failed: %w\nOutput: %s", err, string(output))
	}

	return binFile, generatedFiles, nil
}

// runTest executes a test binary and returns its output
func runTest(binaryPath string) (string, error) {
	cmd := exec.Command(binaryPath)
	output, err := cmd.Output()
	if err != nil {
		// Check if it's an exit status error
		if exitError, ok := err.(*exec.ExitError); ok {
			if status, ok := exitError.Sys().(syscall.WaitStatus); ok {
				// Return the output even if there was a non-zero exit code
				// This allows tests that expect specific exit codes
				return string(output), fmt.Errorf("exit status %d", status.ExitStatus())
			}
		}
		return "", err
	}
	return string(output), nil
}

// readExpectedOutput reads the expected output from file
func readExpectedOutput(expectedFile string) (string, error) {
	content, err := os.ReadFile(expectedFile)
	if err != nil {
		return "", err
	}
	return string(content), nil
}

// cleanupFiles removes the specified files, ignoring any errors
func cleanupFiles(files []string) {
	for _, file := range files {
		os.Remove(file) // Ignore errors - files might not exist
	}
}

// runSingleTest runs a single test case and returns pass/fail status
func runSingleTest(config *CompilationConfig, testCase TestCase, testsDir string) (bool, string) {
	fmt.Printf("Running test %s... ", testCase.Name)

	// Compile test
	binaryPath, generatedFiles, err := compileTest(config, testCase, testsDir)
	if err != nil {
		return false, fmt.Sprintf("compilation error: %v", err)
	}

	// Run test
	actualOutput, err := runTest(binaryPath)
	if err != nil {
		return false, fmt.Sprintf("runtime error: %v", err)
	}

	// Read expected output
	expectedOutput, err := readExpectedOutput(testCase.ExpectedFile)
	if err != nil {
		return false, fmt.Sprintf("error reading expected output: %v", err)
	}

	// Compare outputs
	if actualOutput == expectedOutput {
		// Test passed - clean up generated files
		cleanupFiles(generatedFiles)
		return true, ""
	}

	// Test failed - leave files for inspection
	return false, fmt.Sprintf("output mismatch:\nExpected: %q\nActual:   %q", expectedOutput, actualOutput)
}

// findTestCase finds a test case by number or path
func findTestCase(tests []TestCase, identifier string) (*TestCase, error) {
	// If identifier is a path, try to match it directly
	if strings.Contains(identifier, "/") || strings.HasSuffix(identifier, ".pirx") {
		// Remove .pirx extension if present
		if strings.HasSuffix(identifier, ".pirx") {
			identifier = strings.TrimSuffix(identifier, ".pirx")
		}
		// Remove leading tests/ if present
		if strings.HasPrefix(identifier, "tests/") {
			identifier = strings.TrimPrefix(identifier, "tests/")
		}

		for _, test := range tests {
			if test.Name == identifier {
				return &test, nil
			}
		}
		return nil, fmt.Errorf("test not found: %s", identifier)
	}

	// If identifier is just a number, find test that starts with that number
	for _, test := range tests {
		if strings.HasPrefix(test.Name, identifier+"_") || test.Name == identifier {
			return &test, nil
		}
	}

	return nil, fmt.Errorf("test not found: %s", identifier)
}

func main() {
	// Get compilation configuration for current platform
	config, err := getCompilationConfig()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	// Discover tests
	testsDir := "tests"
	tests, err := discoverTests(testsDir)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error discovering tests: %v\n", err)
		os.Exit(1)
	}

	if len(tests) == 0 {
		fmt.Println("No tests found in tests/ directory")
		return
	}

	// Sort tests by name for consistent ordering
	sort.Slice(tests, func(i, j int) bool {
		return tests[i].Name < tests[j].Name
	})

	// Check for command line arguments
	var testsToRun []TestCase
	if len(os.Args) > 1 {
		// Run specific test
		testIdentifier := os.Args[1]
		testCase, err := findTestCase(tests, testIdentifier)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
		testsToRun = []TestCase{*testCase}
		fmt.Printf("Running specific test: %s\n", testCase.Name)
	} else {
		// Run all tests
		testsToRun = tests
		if len(tests) == 1 {
			fmt.Printf("Found 1 test\n")
		} else {
			fmt.Printf("Found %d tests\n", len(tests))
		}
	}

	// Run tests
	passed := 0
	failed := 0

	for _, test := range testsToRun {
		success, errorMsg := runSingleTest(config, test, testsDir)
		if success {
			fmt.Println("PASS")
			passed++
		} else {
			fmt.Printf("FAIL - %s\n", errorMsg)
			failed++
		}
	}

	// Print summary
	if failed == 0 {
		fmt.Printf("Test Results: %d passed. All good!\n", passed)
	} else {
		fmt.Printf("Test Results: %d passed, %d failed\n", passed, failed)
	}

	if failed > 0 {
		os.Exit(1)
	}
}
