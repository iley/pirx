package main

import (
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
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
				AssemblerFlags:   []string{"-arch", "arm64"},
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
func compileTest(config *CompilationConfig, testCase TestCase, tempDir string) (string, error) {
	baseName := filepath.Base(testCase.Name)
	asmFile := filepath.Join(tempDir, baseName+".s")
	objFile := filepath.Join(tempDir, baseName+".o")
	binFile := filepath.Join(tempDir, baseName+config.ExecutableSuffix)

	// Step 1: Compile .pirx to .s using pirx compiler
	pirxCmd := exec.Command("go", "run", "github.com/iley/pirx/cmd/pirx", "-o", asmFile, testCase.PirxFile)
	if err := pirxCmd.Run(); err != nil {
		return "", fmt.Errorf("pirx compilation failed: %w", err)
	}

	// Step 2: Assemble .s to .o
	asArgs := append(config.AssemblerFlags, "-o", objFile, asmFile)
	asCmd := exec.Command(config.Assembler, asArgs...)
	if err := asCmd.Run(); err != nil {
		return "", fmt.Errorf("assembly failed: %w", err)
	}

	// Step 3: Link .o to executable
	ldArgs := append([]string{"-o", binFile, objFile}, config.LinkerFlags...)
	ldCmd := exec.Command(config.Linker, ldArgs...)
	if err := ldCmd.Run(); err != nil {
		return "", fmt.Errorf("linking failed: %w", err)
	}

	return binFile, nil
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

// runSingleTest runs a single test case and returns pass/fail status
func runSingleTest(config *CompilationConfig, testCase TestCase, tempDir string) (bool, string) {
	fmt.Printf("Running test %s... ", testCase.Name)

	// Compile test
	binaryPath, err := compileTest(config, testCase, tempDir)
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
		return true, ""
	}

	return false, fmt.Sprintf("output mismatch:\nExpected: %q\nActual: %q", expectedOutput, actualOutput)
}

func main() {
	// Get compilation configuration for current platform
	config, err := getCompilationConfig()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	// Create temporary directory for compilation artifacts
	tempDir, err := os.MkdirTemp("", "pirx-test-*")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating temp directory: %v\n", err)
		os.Exit(1)
	}
	defer os.RemoveAll(tempDir)

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

	fmt.Printf("Found %d test(s)\n\n", len(tests))

	// Run all tests
	passed := 0
	failed := 0

	for _, test := range tests {
		success, errorMsg := runSingleTest(config, test, tempDir)
		if success {
			fmt.Println("PASS")
			passed++
		} else {
			fmt.Printf("FAIL - %s\n", errorMsg)
			failed++
		}
	}

	// Print summary
	fmt.Printf("\nTest Results: %d passed, %d failed\n", passed, failed)

	if failed > 0 {
		os.Exit(1)
	}
}
