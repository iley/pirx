package main

import (
	"flag"
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"sync"
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
	Name              string
	PirxFile          string
	ExpectedFile      string // .out file for success tests
	ExpectedErrorFile string // .err file for error tests
	IsErrorTest       bool   // true if this test expects compilation to fail
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
			expectedOutFile := filepath.Join(testsDir, baseName+".out")
			expectedErrFile := filepath.Join(testsDir, baseName+".err")

			// Check if expected output file exists (success test)
			if _, err := os.Stat(expectedOutFile); err == nil {
				tests = append(tests, TestCase{
					Name:              baseName,
					PirxFile:          path,
					ExpectedFile:      expectedOutFile,
					ExpectedErrorFile: "",
					IsErrorTest:       false,
				})
			}

			// Check if expected error file exists (error test)
			if _, err := os.Stat(expectedErrFile); err == nil {
				tests = append(tests, TestCase{
					Name:              baseName,
					PirxFile:          path,
					ExpectedFile:      "",
					ExpectedErrorFile: expectedErrFile,
					IsErrorTest:       true,
				})
			}
		}

		return nil
	})

	return tests, err
}

// compileTest compiles a pirx file to executable
// For error tests, it returns the compilation error message in the first return value
func compileTest(config *CompilationConfig, testCase TestCase, testsDir string) (string, []string, error) {
	baseName := filepath.Base(testCase.Name)
	asmFile := filepath.Join(testsDir, baseName+".s")
	objFile := filepath.Join(testsDir, baseName+".o")
	binFile := filepath.Join(testsDir, baseName+config.ExecutableSuffix)

	// Keep track of generated files for cleanup
	generatedFiles := []string{asmFile, objFile, binFile}

	// Step 1: Compile .pirx to .s using pirx compiler
	pirxCmd := exec.Command("go", "run", "github.com/iley/pirx/cmd/pirxc", "-o", asmFile, testCase.PirxFile)
	if output, err := pirxCmd.CombinedOutput(); err != nil {
		if testCase.IsErrorTest {
			// For error tests, return the compilation error output
			return string(output), generatedFiles, nil
		}
		return "", generatedFiles, fmt.Errorf("pirx compilation failed: %w\nOutput: %s", err, string(output))
	}

	// If this is an error test but compilation succeeded, that's a failure
	if testCase.IsErrorTest {
		return "", generatedFiles, fmt.Errorf("expected compilation to fail, but it succeeded")
	}

	// Step 2: Assemble .s to .o
	asArgs := append(config.AssemblerFlags, "-o", objFile, asmFile)
	asCmd := exec.Command(config.Assembler, asArgs...)
	if output, err := asCmd.CombinedOutput(); err != nil {
		return "", generatedFiles, fmt.Errorf("assembly failed: %w\nOutput: %s", err, string(output))
	}

	// Step 3: Link .o to executable
	ldArgs := append([]string{"-o", binFile, objFile, "stdlib/libpirx.a"}, config.LinkerFlags...)
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

// readExpectedError reads the expected error message from file
func readExpectedError(expectedErrorFile string) (string, error) {
	content, err := os.ReadFile(expectedErrorFile)
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

// TestResult represents the result of running a single test
type TestResult struct {
	TestCase TestCase
	Passed   bool
	Message  string
}

// runSingleTest runs a single test case and returns pass/fail status
func runSingleTest(config *CompilationConfig, testCase TestCase, testsDir string) (bool, string) {
	fmt.Printf("Running test %s... ", testCase.Name)

	if testCase.IsErrorTest {
		// Handle error tests
		actualError, generatedFiles, err := compileTest(config, testCase, testsDir)
		if err != nil {
			return false, fmt.Sprintf("error during compilation test: %v", err)
		}

		// Read expected error
		expectedError, err := readExpectedError(testCase.ExpectedErrorFile)
		if err != nil {
			return false, fmt.Sprintf("error reading expected error: %v", err)
		}

		// Compare error messages
		if actualError == expectedError {
			// Test passed - clean up generated files
			cleanupFiles(generatedFiles)
			return true, ""
		}

		// Test failed - leave files for inspection
		return false, fmt.Sprintf("error mismatch:\nExpected: %q\nActual:   %q", expectedError, actualError)
	} else {
		// Handle success tests
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
}

// findTestCase finds a test case by number or path
func findTestCase(tests []TestCase, identifier string) (*TestCase, error) {
	// If identifier is a path, try to match it directly
	if strings.Contains(identifier, "/") || strings.HasSuffix(identifier, ".pirx") {
		// Remove .pirx extension if present
		identifier = strings.TrimSuffix(identifier, ".pirx")
		// Remove leading tests/ if present
		identifier = strings.TrimPrefix(identifier, "tests/")

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

// findPirxFile finds a .pirx file by identifier, looking in discovered tests first,
// then trying direct file patterns
func findPirxFile(tests []TestCase, testsDir, testIdentifier string) (string, error) {
	// If identifier is a path, try to match it directly
	if strings.Contains(testIdentifier, "/") || strings.HasSuffix(testIdentifier, ".pirx") {
		// Remove .pirx extension if present
		identifier := strings.TrimSuffix(testIdentifier, ".pirx")
		// Remove leading tests/ if present
		identifier = strings.TrimPrefix(identifier, "tests/")

		for _, test := range tests {
			if test.Name == identifier {
				return test.PirxFile, nil
			}
		}
		return "", fmt.Errorf("test not found: %s", testIdentifier)
	}

	// If identifier is just a number, find test that starts with that number
	for _, test := range tests {
		if strings.HasPrefix(test.Name, testIdentifier+"_") || test.Name == testIdentifier {
			return test.PirxFile, nil
		}
	}

	// If not found in tests, try to construct the path directly
	candidates := []string{
		filepath.Join(testsDir, testIdentifier+".pirx"),
		filepath.Join(testsDir, testIdentifier+"_*.pirx"),
	}

	for _, pattern := range candidates {
		matches, _ := filepath.Glob(pattern)
		if len(matches) > 0 {
			return matches[0], nil
		}
	}

	return "", fmt.Errorf("could not find test file for '%s'", testIdentifier)
}

// getCompilationPaths returns the file paths used during compilation
func getCompilationPaths(testsDir, baseName string, config *CompilationConfig) (asmFile, objFile, binFile string, generatedFiles []string) {
	asmFile = filepath.Join(testsDir, baseName+".s")
	objFile = filepath.Join(testsDir, baseName+".o")
	binFile = filepath.Join(testsDir, baseName+config.ExecutableSuffix)
	generatedFiles = []string{asmFile, objFile, binFile}
	return
}

// compileProgram compiles a pirx file and returns either the binary path (on success)
// or error message (on failure), along with success flag and generated files for cleanup
func compileProgram(config *CompilationConfig, pirxFile, testsDir, baseName string) (result string, success bool, generatedFiles []string) {
	asmFile, objFile, binFile, generatedFiles := getCompilationPaths(testsDir, baseName, config)

	// Step 1: Compile .pirx to .s using pirx compiler
	pirxCmd := exec.Command("go", "run", "github.com/iley/pirx/cmd/pirxc", "-o", asmFile, pirxFile)
	if output, err := pirxCmd.CombinedOutput(); err != nil {
		return string(output), false, generatedFiles
	}

	// Step 2: Assemble .s to .o
	asArgs := append(config.AssemblerFlags, "-o", objFile, asmFile)
	asCmd := exec.Command(config.Assembler, asArgs...)
	if output, err := asCmd.CombinedOutput(); err != nil {
		errorMsg := fmt.Sprintf("assembly failed: %v\nOutput: %s", err, string(output))
		return errorMsg, false, generatedFiles
	}

	// Step 3: Link .o to executable
	ldArgs := append([]string{"-o", binFile, objFile, "stdlib/libpirx.a"}, config.LinkerFlags...)
	ldCmd := exec.Command(config.Linker, ldArgs...)
	if output, err := ldCmd.CombinedOutput(); err != nil {
		errorMsg := fmt.Sprintf("linking failed: %v\nOutput: %s", err, string(output))
		return errorMsg, false, generatedFiles
	}

	return binFile, true, generatedFiles
}

func runAllTests(config *CompilationConfig, tests []TestCase, testsDir string, parallelism int) {
	if len(tests) == 1 {
		fmt.Printf("Running 1 test\n")
	} else {
		fmt.Printf("Running %d tests\n", len(tests))
	}

	if parallelism <= 1 {
		// Sequential execution
		runAllTestsSequential(config, tests, testsDir)
	} else {
		// Parallel execution
		runAllTestsParallel(config, tests, testsDir, parallelism)
	}
}

func runAllTestsSequential(config *CompilationConfig, tests []TestCase, testsDir string) {
	passed := 0
	failed := 0

	for _, test := range tests {
		success, errorMsg := runSingleTest(config, test, testsDir)
		if success {
			fmt.Println("PASS")
			passed++
		} else {
			fmt.Printf("FAIL - %s\n", errorMsg)
			failed++
		}
	}

	if failed == 0 {
		fmt.Printf("Test Results: %d passed. All good!\n", passed)
	} else {
		fmt.Printf("Test Results: %d passed, %d failed\n", passed, failed)
	}

	if failed > 0 {
		os.Exit(1)
	}
}

func runAllTestsParallel(config *CompilationConfig, tests []TestCase, testsDir string, parallelism int) {
	results := make([]TestResult, len(tests))
	testChan := make(chan int, len(tests))
	var wg sync.WaitGroup

	// Start workers
	for range parallelism {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for testIndex := range testChan {
				test := tests[testIndex]
				success, errorMsg := runSingleTestQuiet(config, test, testsDir)
				results[testIndex] = TestResult{
					TestCase: test,
					Passed:   success,
					Message:  errorMsg,
				}
			}
		}()
	}

	// Queue all tests
	for i := range tests {
		testChan <- i
	}
	close(testChan)

	// Wait for all tests to complete
	wg.Wait()

	// Print results in order
	passed := 0
	failed := 0
	for _, result := range results {
		if result.Passed {
			fmt.Printf("Running test %s... PASS\n", result.TestCase.Name)
			passed++
		} else {
			fmt.Printf("Running test %s... FAIL - %s\n", result.TestCase.Name, result.Message)
			failed++
		}
	}

	if failed == 0 {
		fmt.Printf("Test Results: %d passed. All good!\n", passed)
	} else {
		fmt.Printf("Test Results: %d passed, %d failed\n", passed, failed)
	}

	if failed > 0 {
		os.Exit(1)
	}
}

// runSingleTestQuiet runs a single test case without printing progress
func runSingleTestQuiet(config *CompilationConfig, testCase TestCase, testsDir string) (bool, string) {
	if testCase.IsErrorTest {
		// Handle error tests
		actualError, generatedFiles, err := compileTest(config, testCase, testsDir)
		if err != nil {
			return false, fmt.Sprintf("error during compilation test: %v", err)
		}

		// Read expected error
		expectedError, err := readExpectedError(testCase.ExpectedErrorFile)
		if err != nil {
			return false, fmt.Sprintf("error reading expected error: %v", err)
		}

		// Compare error messages
		if actualError == expectedError {
			// Test passed - clean up generated files
			cleanupFiles(generatedFiles)
			return true, ""
		}

		// Test failed - leave files for inspection
		return false, fmt.Sprintf("error mismatch:\nExpected: %q\nActual:   %q", expectedError, actualError)
	} else {
		// Handle success tests
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
}

func runSpecificTest(config *CompilationConfig, tests []TestCase, testsDir string, testIdentifier string) {
	testCase, err := findTestCase(tests, testIdentifier)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Running specific test: %s\n", testCase.Name)

	success, errorMsg := runSingleTest(config, *testCase, testsDir)
	if success {
		fmt.Println("PASS")
		fmt.Printf("Test Results: 1 passed. All good!\n")
	} else {
		fmt.Printf("FAIL - %s\n", errorMsg)
		fmt.Printf("Test Results: 0 passed, 1 failed\n")
		os.Exit(1)
	}
}

func runProgram(config *CompilationConfig, tests []TestCase, testsDir string, testIdentifier string) {
	pirxFile, err := findPirxFile(tests, testsDir, testIdentifier)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	baseName := strings.TrimSuffix(filepath.Base(pirxFile), ".pirx")
	fmt.Printf("Compiling and running: %s\n", baseName)

	result, success, generatedFiles := compileProgram(config, pirxFile, testsDir, baseName)

	if !success {
		fmt.Printf("Compilation failed:\n%s", result)
		cleanupFiles(generatedFiles)
		return
	}

	fmt.Printf("Running program...\n")
	output, err := runTest(result) // result is the binary path
	if err != nil {
		fmt.Printf("Program output:\n%s\nProgram exited with error: %v\n", output, err)
	} else {
		fmt.Printf("Program output:\n%s", output)
	}

	cleanupFiles(generatedFiles)
}

func acceptTest(config *CompilationConfig, tests []TestCase, testsDir string, testIdentifier string) {
	pirxFile, err := findPirxFile(tests, testsDir, testIdentifier)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	baseName := strings.TrimSuffix(filepath.Base(pirxFile), ".pirx")
	fmt.Printf("Accepting output for: %s\n", baseName)

	result, success, generatedFiles := compileProgram(config, pirxFile, testsDir, baseName)

	if !success {
		// Compilation failed - save to .err file
		errFile := filepath.Join(testsDir, baseName+".err")
		if err := os.WriteFile(errFile, []byte(result), 0644); err != nil {
			fmt.Fprintf(os.Stderr, "Error writing .err file: %v\n", err)
			os.Exit(1)
		}
		fmt.Printf("Compilation failed. Saved error output to %s\n", errFile)
		cleanupFiles(generatedFiles)
		return
	}

	// Compilation successful - run the program and save output to .out file
	output, _ := runTest(result) // result is the binary path
	outFile := filepath.Join(testsDir, baseName+".out")
	if err := os.WriteFile(outFile, []byte(output), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "Error writing .out file: %v\n", err)
		os.Exit(1)
	}
	fmt.Printf("Program executed successfully. Saved output to %s\n", outFile)

	cleanupFiles(generatedFiles)
}

func printUsage() {
	fmt.Fprintf(os.Stderr, "Usage: %s [options] <command> [args]\n", os.Args[0])
	fmt.Fprintf(os.Stderr, "\nOptions:\n")
	fmt.Fprintf(os.Stderr, "  -j <num>        Number of parallel jobs (default: 1)\n")
	fmt.Fprintf(os.Stderr, "\nCommands:\n")
	fmt.Fprintf(os.Stderr, "  testall         Run all tests\n")
	fmt.Fprintf(os.Stderr, "  test <test>     Run a specific test (e.g., '05' for tests/05_xxx.pirx)\n")
	fmt.Fprintf(os.Stderr, "  run <test>      Compile and run a test program without comparison\n")
	fmt.Fprintf(os.Stderr, "  accept <test>   Run a test and save its output to .out or .err file\n")
}

func main() {
	// Parse command line flags
	var parallelism int
	flag.IntVar(&parallelism, "j", 1, "Number of parallel jobs")
	flag.Usage = printUsage
	flag.Parse()

	args := flag.Args()
	if len(args) < 1 {
		printUsage()
		os.Exit(1)
	}

	command := args[0]

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

	switch command {
	case "testall":
		runAllTests(config, tests, testsDir, parallelism)
	case "test":
		if len(args) < 2 {
			fmt.Fprintf(os.Stderr, "Error: 'test' command requires a test identifier\n")
			printUsage()
			os.Exit(1)
		}
		testIdentifier := args[1]
		runSpecificTest(config, tests, testsDir, testIdentifier)
	case "run":
		if len(args) < 2 {
			fmt.Fprintf(os.Stderr, "Error: 'run' command requires a test identifier\n")
			printUsage()
			os.Exit(1)
		}
		testIdentifier := args[1]
		runProgram(config, tests, testsDir, testIdentifier)
	case "accept":
		if len(args) < 2 {
			fmt.Fprintf(os.Stderr, "Error: 'accept' command requires a test identifier\n")
			printUsage()
			os.Exit(1)
		}
		testIdentifier := args[1]
		acceptTest(config, tests, testsDir, testIdentifier)
	default:
		fmt.Fprintf(os.Stderr, "Error: unknown command '%s'\n", command)
		printUsage()
		os.Exit(1)
	}
}
