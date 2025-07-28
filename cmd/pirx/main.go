package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
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
				LinkerFlags:      []string{"-lSystem", "-syslibroot", getMacOsSdkPath(), "-arch", "arm64"},
				ExecutableSuffix: "",
			}, nil
		case "amd64":
			return &CompilationConfig{
				Assembler:        "as",
				AssemblerFlags:   []string{"-arch", "x86_64"},
				Linker:           "ld",
				LinkerFlags:      []string{"-lSystem", "-syslibroot", getMacOsSdkPath(), "-arch", "x86_64"},
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

func getMacOsSdkPath() string {
	cmd := exec.Command("xcrun", "-sdk", "macosx", "--show-sdk-path")
	output, err := cmd.Output()
	if err != nil {
		return ""
	}
	return strings.TrimSpace(string(output))
}

// getPirxRoot returns the PIRX root directory, either from PIRXROOT env var
// or by determining it from the location of the pirx binary
func getPirxRoot() (string, error) {
	if root := os.Getenv("PIRXROOT"); root != "" {
		return root, nil
	}

	execPath, err := os.Executable()
	if err != nil {
		return "", fmt.Errorf("failed to get executable path: %w", err)
	}
	return filepath.Dir(execPath), nil
}

// buildProgram compiles a pirx file to an executable
func buildProgram(config *CompilationConfig, pirxFile string) error {
	// Get PIRX root directory
	pirxRoot, err := getPirxRoot()
	if err != nil {
		return fmt.Errorf("failed to determine PIRXROOT: %w", err)
	}

	// Get directory and base name
	sourceDir := filepath.Dir(pirxFile)
	baseName := strings.TrimSuffix(filepath.Base(pirxFile), ".pirx")

	// Generate file paths in the same directory as source
	asmFile := filepath.Join(sourceDir, baseName+".s")
	objFile := filepath.Join(sourceDir, baseName+".o")
	binFile := filepath.Join(sourceDir, baseName+config.ExecutableSuffix)

	stdlibPath := filepath.Join(pirxRoot, "stdlib", "libpirx.a")
	pirxcPath := filepath.Join(pirxRoot, "pirxc")

	// Step 1: Compile .pirx to .s using pirxc compiler
	pirxCmd := exec.Command(pirxcPath, "-o", asmFile, pirxFile)
	if output, err := pirxCmd.CombinedOutput(); err != nil {
		fmt.Fprintf(os.Stderr, "pirxc compilation failed: %v\nOutput: %s\n", err, string(output))
		return err
	}

	// Step 2: Assemble .s to .o
	asArgs := append(config.AssemblerFlags, "-o", objFile, asmFile)
	asCmd := exec.Command(config.Assembler, asArgs...)
	if output, err := asCmd.CombinedOutput(); err != nil {
		fmt.Fprintf(os.Stderr, "assembly failed: %v\nOutput: %s\n", err, string(output))
		return err
	}

	// Step 3: Link .o to executable
	ldArgs := append([]string{"-o", binFile, objFile, stdlibPath}, config.LinkerFlags...)
	ldCmd := exec.Command(config.Linker, ldArgs...)
	if output, err := ldCmd.CombinedOutput(); err != nil {
		fmt.Fprintf(os.Stderr, "linking failed: %v\nOutput: %s\n", err, string(output))
		return err
	}

	fmt.Printf("Built %s\n", binFile)
	return nil
}

func printUsage() {
	fmt.Fprintf(os.Stderr, "Usage: pirx <command> [arguments]\n\n")
	fmt.Fprintf(os.Stderr, "Commands:\n")
	fmt.Fprintf(os.Stderr, "  build <file.pirx>    Build a Pirx program\n")
}

func main() {
	if len(os.Args) < 2 {
		printUsage()
		os.Exit(1)
	}

	command := os.Args[1]

	switch command {
	case "build":
		if len(os.Args) != 3 {
			fmt.Fprintf(os.Stderr, "Usage: pirx build <file.pirx>\n")
			os.Exit(1)
		}

		pirxFile := os.Args[2]

		if _, err := os.Stat(pirxFile); os.IsNotExist(err) {
			fmt.Fprintf(os.Stderr, "Error: file %s does not exist\n", pirxFile)
			os.Exit(1)
		}

		config, err := getCompilationConfig()
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}

		if err := buildProgram(config, pirxFile); err != nil {
			os.Exit(1)
		}

	default:
		fmt.Fprintf(os.Stderr, "Unknown command: %s\n", command)
		printUsage()
		os.Exit(1)
	}
}
