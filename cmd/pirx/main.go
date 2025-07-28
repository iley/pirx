package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/spf13/cobra"
)

// CompilationConfig holds platform-specific compilation settings
type CompilationConfig struct {
	Assembler        string
	AssemblerFlags   []string
	Linker           string
	LinkerFlags      []string
	ExecutableSuffix string
}

var rootCmd = &cobra.Command{
	Use:   "pirx",
	Short: "Pirx programming language build system",
	Long:  "A build system for the Pirx programming language.",
}

var buildCmd = &cobra.Command{
	Use:   "build <file.pirx>",
	Short: "Build a Pirx program",
	Long:  "Compile a Pirx source file to an executable binary.",
	Args:  cobra.ExactArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		pirxFile := args[0]

		// Check if file exists
		if _, err := os.Stat(pirxFile); os.IsNotExist(err) {
			return fmt.Errorf("file %s does not exist", pirxFile)
		}

		// Get compilation config
		config, err := getCompilationConfig()
		if err != nil {
			return fmt.Errorf("failed to get compilation config: %w", err)
		}

		// Get the keep flag value
		keepIntermediateFiles, _ := cmd.Flags().GetBool("keep")

		// Build the program
		if err := buildProgram(config, pirxFile, keepIntermediateFiles); err != nil {
			cmd.SilenceUsage = true
			return err
		}
		return nil
	},
}

func init() {
	buildCmd.Flags().BoolP("keep", "k", false, "Keep intermediate files (.s, .o)")
	rootCmd.AddCommand(buildCmd)
}

func main() {
	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}

// buildProgram compiles a pirx file to an executable
func buildProgram(config *CompilationConfig, pirxFile string, keepIntermediate bool) error {
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
		fmt.Fprintf(os.Stderr, "pirxc compilation failed: %v\nOutput: %s", err, string(output))
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

	// Clean up intermediate files unless -k flag is set
	if !keepIntermediate {
		os.Remove(asmFile)
		os.Remove(objFile)
	}

	fmt.Printf("Built %s\n", binFile)
	return nil
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

func getMacOsSdkPath() string {
	cmd := exec.Command("xcrun", "-sdk", "macosx", "--show-sdk-path")
	output, err := cmd.Output()
	if err != nil {
		return ""
	}
	return strings.TrimSpace(string(output))
}
