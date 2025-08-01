package main

import (
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"syscall"

	"github.com/spf13/cobra"
)

// TestCase represents a single test case
type TestCase struct {
	Name              string
	PirxFile          string // For single file tests
	PirxDir           string // For directory tests
	ExpectedFile      string // .out file for success tests
	ExpectedErrorFile string // .err file for error tests
	IsErrorTest       bool   // true if this test expects compilation to fail
	IsDirectoryTest   bool   // true if this is a directory test
}

// TestResult represents the result of running a test
type TestResult struct {
	TestCase TestCase
	Passed   bool
	Message  string
}

var (
	parallelism int
	optLevel    string
	verbose     bool
)

var rootCmd = &cobra.Command{
	Use:   "testrunner",
	Short: "Test runner for Pirx compiler",
	Long:  "A test runner for the Pirx programming language compiler.",
	// Silence all Cobra output to avoid test changes
	SilenceUsage:  true,
	SilenceErrors: true,
}

var testallCmd = &cobra.Command{
	Use:   "testall",
	Short: "Run all tests",
	Long:  "Run all tests in the tests/ directory.",
	RunE: func(cmd *cobra.Command, args []string) error {
		tests, testsDir, err := setupTests()
		if err != nil {
			return err
		}
		runAllTests(tests, testsDir, parallelism)
		return nil
	},
}

var testCmd = &cobra.Command{
	Use:   "test <test>",
	Short: "Run a specific test",
	Long:  "Run a specific test (e.g., '05' for tests/05_xxx.pirx).",
	Args:  cobra.ExactArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		tests, testsDir, err := setupTests()
		if err != nil {
			return err
		}
		testIdentifier := args[0]
		runSpecificTest(tests, testsDir, testIdentifier)
		return nil
	},
}

var runCmd = &cobra.Command{
	Use:   "run <test>",
	Short: "Compile and run a test program",
	Long:  "Compile and run a test program without comparison.",
	Args:  cobra.ExactArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		tests, testsDir, err := setupTests()
		if err != nil {
			return err
		}
		testIdentifier := args[0]
		runProgram(tests, testsDir, testIdentifier)
		return nil
	},
}

var acceptCmd = &cobra.Command{
	Use:   "accept <test>",
	Short: "Accept test output",
	Long:  "Run a test and save its output to .out or .err file.",
	Args:  cobra.ExactArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		tests, testsDir, err := setupTests()
		if err != nil {
			return err
		}
		testIdentifier := args[0]
		acceptTest(tests, testsDir, testIdentifier)
		return nil
	},
}

var buildCmd = &cobra.Command{
	Use:   "build <test>",
	Short: "Build a test program",
	Long:  "Build a test program and leave all intermediate files for inspection.",
	Args:  cobra.ExactArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		tests, testsDir, err := setupTests()
		if err != nil {
			return err
		}
		testIdentifier := args[0]
		buildProgram(tests, testsDir, testIdentifier)
		return nil
	},
}

func main() {
	rootCmd.PersistentFlags().IntVarP(&parallelism, "jobs", "j", 1, "Number of parallel jobs")
	rootCmd.PersistentFlags().StringVarP(&optLevel, "O", "O", "", "optimization level to pass to pirx build")
	rootCmd.PersistentFlags().BoolVarP(&verbose, "verbose", "v", false, "print commands being executed")

	rootCmd.AddCommand(testallCmd)
	rootCmd.AddCommand(testCmd)
	rootCmd.AddCommand(runCmd)
	rootCmd.AddCommand(acceptCmd)
	rootCmd.AddCommand(buildCmd)

	if err := rootCmd.Execute(); err != nil {
		// Handle errors manually to maintain exact same output format
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

func setupTests() ([]TestCase, string, error) {
	testsDir := "tests"
	tests, err := discoverTests(testsDir)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error discovering tests: %v\n", err)
		os.Exit(1)
	}

	if len(tests) == 0 {
		fmt.Println("No tests found in tests/ directory")
		os.Exit(0)
	}

	// Sort tests by name for consistent ordering
	sort.Slice(tests, func(i, j int) bool {
		return tests[i].Name < tests[j].Name
	})

	return tests, testsDir, nil
}

func runAllTests(tests []TestCase, testsDir string, parallelism int) {
	if len(tests) == 1 {
		fmt.Printf("Running 1 test\n")
	} else {
		fmt.Printf("Running %d tests\n", len(tests))
	}

	if parallelism <= 1 {
		runAllTestsSequential(tests, testsDir)
	} else {
		runAllTestsParallel(tests, testsDir, parallelism)
	}
}

func runSpecificTest(tests []TestCase, testsDir string, testIdentifier string) {
	testCase, err := findTestCase(tests, testIdentifier)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Running specific test: %s\n", testCase.Name)
	success, errorMsg := runSingleTest(*testCase, testsDir)
	if success {
		fmt.Println("PASS")
		fmt.Println("Test Results: 1 passed. All good!")
	} else {
		fmt.Printf("FAIL - %s\n", errorMsg)
		fmt.Println("Test Results: 0 passed, 1 failed")
		os.Exit(1)
	}
}

func runProgram(tests []TestCase, testsDir string, testIdentifier string) {
	pirxFile, err := findPirxFile(tests, testsDir, testIdentifier)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	baseName := strings.TrimSuffix(filepath.Base(pirxFile), ".pirx")
	fmt.Printf("Compiling and running: %s\n", baseName)

	result, success, generatedFiles := compileProgram(pirxFile, testsDir, baseName)
	if !success {
		fmt.Printf("Compilation failed:\n%s\n", result)
		cleanupFiles(generatedFiles)
		os.Exit(1)
	}

	fmt.Println("Running program...")
	output, err := runTest(result)
	if err != nil {
		fmt.Printf("Runtime error: %v\n", err)
		cleanupFiles(generatedFiles)
		os.Exit(1)
	}

	fmt.Println("Program output:")
	fmt.Print(output)

	cleanupFiles(generatedFiles)
}

func buildProgram(tests []TestCase, testsDir string, testIdentifier string) {
	pirxFile, err := findPirxFile(tests, testsDir, testIdentifier)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	baseName := strings.TrimSuffix(filepath.Base(pirxFile), ".pirx")
	fmt.Printf("Building: %s\n", baseName)

	result, success, generatedFiles := compileProgram(pirxFile, testsDir, baseName)
	if !success {
		fmt.Printf("Compilation failed:\n%s\n", result)
		fmt.Printf("Intermediate files left for inspection:\n")
		for _, file := range generatedFiles {
			if _, err := os.Stat(file); err == nil {
				fmt.Printf("  %s\n", file)
			}
		}
		os.Exit(1)
	}

	fmt.Printf("Build successful. Files created:\n")
	for _, file := range generatedFiles {
		if _, err := os.Stat(file); err == nil {
			fmt.Printf("  %s\n", file)
		}
	}
	fmt.Printf("Binary: %s\n", result)
}

func acceptTest(tests []TestCase, testsDir string, testIdentifier string) {
	pirxFile, err := findPirxFile(tests, testsDir, testIdentifier)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	baseName := strings.TrimSuffix(filepath.Base(pirxFile), ".pirx")
	fmt.Printf("Accepting output for: %s\n", baseName)

	result, success, generatedFiles := compileProgram(pirxFile, testsDir, baseName)
	if !success {
		// This is a compilation error test
		outFile := filepath.Join(testsDir, baseName+".err")
		err := os.WriteFile(outFile, []byte(result), 0o644)
		if err != nil {
			fmt.Printf("Failed to write error file: %v\n", err)
			os.Exit(1)
		}
		fmt.Printf("Compilation failed. Saved error output to %s\n", outFile)
		cleanupFiles(generatedFiles)
		return
	}

	// This is a success test
	output, err := runTest(result)
	if err != nil {
		fmt.Printf("Runtime error: %v\n", err)
		cleanupFiles(generatedFiles)
		os.Exit(1)
	}

	outFile := filepath.Join(testsDir, baseName+".out")
	err = os.WriteFile(outFile, []byte(output), 0o644)
	if err != nil {
		fmt.Printf("Failed to write output file: %v\n", err)
		cleanupFiles(generatedFiles)
		os.Exit(1)
	}
	fmt.Printf("Program executed successfully. Saved output to %s\n", outFile)

	cleanupFiles(generatedFiles)
}

func runAllTestsSequential(tests []TestCase, testsDir string) {
	passed := 0
	failed := 0

	for _, test := range tests {
		success, errorMsg := runSingleTest(test, testsDir)
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
		os.Exit(1)
	}
}

func runAllTestsParallel(tests []TestCase, testsDir string, parallelism int) {
	testChan := make(chan int, len(tests))
	results := make([]TestResult, len(tests))
	var wg sync.WaitGroup

	// Start worker goroutines
	for range parallelism {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for testIndex := range testChan {
				test := tests[testIndex]
				success, errorMsg := runSingleTestQuiet(test, testsDir)
				results[testIndex] = TestResult{
					TestCase: test,
					Passed:   success,
					Message:  errorMsg,
				}
			}
		}()
	}

	for i := range tests {
		testChan <- i
	}
	close(testChan)

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

func runSingleTest(testCase TestCase, testsDir string) (bool, string) {
	fmt.Printf("Running test %s... ", testCase.Name)

	if testCase.IsErrorTest {
		// Handle error tests
		actualError, generatedFiles, err := compileTest(testCase, testsDir)
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
		binaryPath, generatedFiles, err := compileTest(testCase, testsDir)
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

func runSingleTestQuiet(testCase TestCase, testsDir string) (bool, string) {
	if testCase.IsErrorTest {
		actualError, generatedFiles, err := compileTest(testCase, testsDir)
		if err != nil {
			return false, fmt.Sprintf("error during compilation test: %v", err)
		}

		expectedError, err := readExpectedError(testCase.ExpectedErrorFile)
		if err != nil {
			return false, fmt.Sprintf("error reading expected error: %v", err)
		}

		if actualError == expectedError {
			cleanupFiles(generatedFiles)
			return true, ""
		}

		return false, fmt.Sprintf("error mismatch:\nExpected: %q\nActual:   %q", expectedError, actualError)
	} else {
		binaryPath, generatedFiles, err := compileTest(testCase, testsDir)
		if err != nil {
			return false, fmt.Sprintf("compilation error: %v", err)
		}

		actualOutput, err := runTest(binaryPath)
		if err != nil {
			return false, fmt.Sprintf("runtime error: %v", err)
		}

		expectedOutput, err := readExpectedOutput(testCase.ExpectedFile)
		if err != nil {
			return false, fmt.Sprintf("error reading expected output: %v", err)
		}

		if actualOutput == expectedOutput {
			cleanupFiles(generatedFiles)
			return true, ""
		}

		return false, fmt.Sprintf("output mismatch:\nExpected: %q\nActual:   %q", expectedOutput, actualOutput)
	}
}

func discoverTests(testsDir string) ([]TestCase, error) {
	var tests []TestCase

	err := filepath.WalkDir(testsDir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}

		if d.IsDir() && path != testsDir {
			// Check if this is a directory test
			baseName := filepath.Base(path)

			// Check if directory name matches test pattern (starts with digits followed by underscore)
			if len(baseName) >= 4 && baseName[0] >= '0' && baseName[0] <= '9' && baseName[1] >= '0' && baseName[1] <= '9' && baseName[2] >= '0' && baseName[2] <= '9' && baseName[3] == '_' {
				expectedOutFile := filepath.Join(testsDir, baseName+".out")
				expectedErrFile := filepath.Join(testsDir, baseName+".err")

				// Check if expected output file exists (success test)
				if _, err := os.Stat(expectedOutFile); err == nil {
					tests = append(tests, TestCase{
						Name:            baseName,
						PirxDir:         path,
						ExpectedFile:    expectedOutFile,
						IsErrorTest:     false,
						IsDirectoryTest: true,
					})
				}

				// Check if expected error file exists (error test)
				if _, err := os.Stat(expectedErrFile); err == nil {
					tests = append(tests, TestCase{
						Name:              baseName,
						PirxDir:           path,
						ExpectedErrorFile: expectedErrFile,
						IsErrorTest:       true,
						IsDirectoryTest:   true,
					})
				}
			}
		} else if !d.IsDir() && strings.HasSuffix(path, ".pirx") {
			// Extract base name without extension
			baseName := strings.TrimSuffix(filepath.Base(path), ".pirx")
			expectedOutFile := filepath.Join(testsDir, baseName+".out")
			expectedErrFile := filepath.Join(testsDir, baseName+".err")

			// Check if expected output file exists (success test)
			if _, err := os.Stat(expectedOutFile); err == nil {
				tests = append(tests, TestCase{
					Name:         baseName,
					PirxFile:     path,
					ExpectedFile: expectedOutFile,
					IsErrorTest:  false,
				})
			}

			// Check if expected error file exists (error test)
			if _, err := os.Stat(expectedErrFile); err == nil {
				tests = append(tests, TestCase{
					Name:              baseName,
					PirxFile:          path,
					ExpectedErrorFile: expectedErrFile,
					IsErrorTest:       true,
				})
			}
		}

		return nil
	})

	return tests, err
}

func findTestCase(tests []TestCase, identifier string) (*TestCase, error) {
	// First try exact match
	for _, test := range tests {
		if test.Name == identifier {
			return &test, nil
		}
	}

	// If not found and identifier contains path separator or .pirx extension, return not found
	if strings.Contains(identifier, "/") || strings.HasSuffix(identifier, ".pirx") {
		// Remove .pirx extension if present
		identifier = strings.TrimSuffix(identifier, ".pirx")
		// Extract just the filename if it's a path
		if strings.Contains(identifier, "/") {
			identifier = filepath.Base(identifier)
		}
		// Try exact match again with cleaned identifier
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

func findPirxFile(tests []TestCase, testsDir, testIdentifier string) (string, error) {
	// Check if testIdentifier contains a path separator or .pirx extension
	if strings.Contains(testIdentifier, "/") || strings.HasSuffix(testIdentifier, ".pirx") {
		// Remove .pirx extension if present
		identifier := strings.TrimSuffix(testIdentifier, ".pirx")
		// Extract just the filename if it's a path
		if strings.Contains(testIdentifier, "/") {
			identifier = filepath.Base(identifier)
		}
		// Try to find in discovered tests first
		for _, test := range tests {
			if test.Name == identifier {
				if test.IsDirectoryTest {
					return test.PirxDir, nil
				}
				return test.PirxFile, nil
			}
		}
		return "", fmt.Errorf("test not found: %s", testIdentifier)
	}

	// If identifier is just a number, find test that starts with that number
	for _, test := range tests {
		if strings.HasPrefix(test.Name, testIdentifier+"_") || test.Name == testIdentifier {
			if test.IsDirectoryTest {
				return test.PirxDir, nil
			}
			return test.PirxFile, nil
		}
	}

	// If not found in tests, try to construct the path directly
	candidates := []string{
		filepath.Join(testsDir, testIdentifier+".pirx"),
		filepath.Join(testsDir, testIdentifier+"_*.pirx"),
		filepath.Join(testsDir, testIdentifier),
		filepath.Join(testsDir, testIdentifier+"_*"),
	}

	for _, pattern := range candidates {
		if strings.Contains(pattern, "*") {
			matches, err := filepath.Glob(pattern)
			if err == nil && len(matches) > 0 {
				return matches[0], nil
			}
		} else {
			if _, err := os.Stat(pattern); err == nil {
				return pattern, nil
			}
		}
	}

	return "", fmt.Errorf("could not find test file for '%s'", testIdentifier)
}

func buildPirxArgs(testCase TestCase) []string {
	args := []string{"build", "-k"}
	if optLevel != "" {
		args = append(args, "-O"+optLevel)
	}

	if testCase.IsDirectoryTest {
		args = append(args, testCase.PirxDir)
	} else {
		args = append(args, testCase.PirxFile)
	}

	return args
}

func compileTest(testCase TestCase, testsDir string) (string, []string, error) {
	baseName := filepath.Base(testCase.Name)

	var binFile string
	var generatedFiles []string

	if testCase.IsDirectoryTest {
		// For directory tests, the binary will be created inside the directory
		// with the same name as the directory
		binFile = filepath.Join(testCase.PirxDir, baseName)
		generatedFiles = []string{binFile}

		// Also track potential intermediate files in the directory
		asmFile := filepath.Join(testCase.PirxDir, baseName+".s")
		objFile := filepath.Join(testCase.PirxDir, baseName+".o")
		generatedFiles = append(generatedFiles, asmFile, objFile)
	} else {
		// For single file tests, use the testsDir
		asmFile := filepath.Join(testsDir, baseName+".s")
		objFile := filepath.Join(testsDir, baseName+".o")
		binFile = filepath.Join(testsDir, baseName)
		generatedFiles = []string{asmFile, objFile, binFile}
	}

	// Use pirx build with -k flag to keep intermediate files for inspection
	args := buildPirxArgs(testCase)
	pirxCmd := exec.Command("./pirx", args...)
	if verbose {
		fmt.Printf("Executing: ./pirx %s\n", strings.Join(args, " "))
	}
	if output, err := pirxCmd.CombinedOutput(); err != nil {
		if testCase.IsErrorTest {
			// For error tests, return the compilation error output
			return string(output), generatedFiles, nil
		}
		return "", generatedFiles, fmt.Errorf("pirx build failed: %w\nOutput: %s", err, string(output))
	}

	// If this is an error test but compilation succeeded, that's a failure
	if testCase.IsErrorTest {
		return "", generatedFiles, fmt.Errorf("expected compilation to fail, but it succeeded")
	}

	return binFile, generatedFiles, nil
}

func compileProgram(pirxPath, testsDir, baseName string) (result string, success bool, generatedFiles []string) {
	// Check if pirxPath is a directory or file
	stat, err := os.Stat(pirxPath)
	if err != nil {
		return fmt.Sprintf("failed to stat %s: %v", pirxPath, err), false, nil
	}

	var binFile string
	if stat.IsDir() {
		// Directory build - binary goes inside the directory
		binFile = filepath.Join(pirxPath, baseName)
		asmFile := filepath.Join(pirxPath, baseName+".s")
		objFile := filepath.Join(pirxPath, baseName+".o")
		generatedFiles = []string{asmFile, objFile, binFile}
	} else {
		// File build - binary goes in testsDir
		asmFile := filepath.Join(testsDir, baseName+".s")
		objFile := filepath.Join(testsDir, baseName+".o")
		binFile = filepath.Join(testsDir, baseName)
		generatedFiles = []string{asmFile, objFile, binFile}
	}

	// Use pirx build with -k flag to keep intermediate files for inspection
	args := []string{"build", "-k"}
	if optLevel != "" {
		args = append(args, "-O"+optLevel)
	}
	args = append(args, pirxPath)

	pirxCmd := exec.Command("./pirx", args...)
	if verbose {
		fmt.Printf("Executing: ./pirx %s\n", strings.Join(args, " "))
	}
	if output, err := pirxCmd.CombinedOutput(); err != nil {
		return string(output), false, generatedFiles
	}

	return binFile, true, generatedFiles
}

func runTest(binaryPath string) (string, error) {
	cmd := exec.Command(binaryPath)
	output, err := cmd.CombinedOutput()
	if err != nil {
		// Check if it's a non-zero exit code
		if exitError, ok := err.(*exec.ExitError); ok {
			if status, ok := exitError.Sys().(syscall.WaitStatus); ok {
				return string(output), fmt.Errorf("exit status %d", status.ExitStatus())
			}
		}
		return string(output), err
	}
	return string(output), nil
}

func readExpectedOutput(expectedFile string) (string, error) {
	content, err := os.ReadFile(expectedFile)
	if err != nil {
		return "", err
	}
	return string(content), nil
}

func readExpectedError(expectedErrorFile string) (string, error) {
	content, err := os.ReadFile(expectedErrorFile)
	if err != nil {
		return "", err
	}
	return string(content), nil
}

func cleanupFiles(files []string) {
	for _, file := range files {
		os.Remove(file) // Ignore errors during cleanup
	}
}
