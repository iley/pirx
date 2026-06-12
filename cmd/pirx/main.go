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

var (
	optLevel   string
	outputFile string
)

// CompilationConfig holds platform-specific compilation settings
type CompilationConfig struct {
	Assembler      string
	AssemblerFlags []string
	Linker         string
	LinkerFlags    []string
}

var rootCmd = &cobra.Command{
	Use:   "pirx",
	Short: "Pirx programming language build system",
	Long:  "A build system for the Pirx programming language.",
}

var buildCmd = &cobra.Command{
	Use:   "build <file.pirx|file.c>... | <directory>",
	Short: "Build a Pirx program",
	Long:  "Compile one or more Pirx source files and optionally C files, or a directory containing .pirx and .c files to an executable binary.",
	Args:  cobra.MinimumNArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		var pirxFiles []string
		var cFiles []string
		var isDirectoryBuild bool
		var buildDir string

		// Process arguments - could be files or a single directory
		if len(args) == 1 {
			arg := args[0]
			if stat, err := os.Stat(arg); err != nil {
				return fmt.Errorf("path %s does not exist", arg)
			} else if stat.IsDir() {
				// Directory build
				isDirectoryBuild = true

				// Convert "." to absolute path for consistent behavior
				if arg == "." {
					var err error
					buildDir, err = os.Getwd()
					if err != nil {
						return fmt.Errorf("failed to get current directory: %w", err)
					}
				} else {
					buildDir = arg
				}

				pFiles, cFileList, err := findSourceFiles(arg)
				if err != nil {
					return fmt.Errorf("failed to find source files in directory %s: %w", arg, err)
				}
				if len(pFiles) == 0 && len(cFileList) == 0 {
					return fmt.Errorf("no .pirx or .c files found in directory %s", arg)
				}
				if len(pFiles) == 0 {
					return fmt.Errorf("no .pirx files found in directory %s (C files found but at least one .pirx file is required)", arg)
				}
				pirxFiles = pFiles
				cFiles = cFileList
			} else {
				// Single file - check if it's .pirx or .c
				if strings.HasSuffix(arg, ".pirx") {
					pirxFiles = []string{arg}
				} else if strings.HasSuffix(arg, ".c") {
					return fmt.Errorf("cannot build with only C files - at least one .pirx file is required")
				} else {
					return fmt.Errorf("unsupported file extension: %s (supported: .pirx, .c)", arg)
				}
			}
		} else {
			// Multiple files - ensure all are files, not directories
			for _, arg := range args {
				if stat, err := os.Stat(arg); err != nil {
					return fmt.Errorf("file %s does not exist", arg)
				} else if stat.IsDir() {
					return fmt.Errorf("cannot mix directories and files in build arguments")
				}

				// Separate .pirx and .c files
				if strings.HasSuffix(arg, ".pirx") {
					pirxFiles = append(pirxFiles, arg)
				} else if strings.HasSuffix(arg, ".c") {
					cFiles = append(cFiles, arg)
				} else {
					return fmt.Errorf("unsupported file extension: %s (supported: .pirx, .c)", arg)
				}
			}

			if len(pirxFiles) == 0 {
				return fmt.Errorf("at least one .pirx file is required")
			}
		}

		// When multiple files are specified (and not directory build), -o must be used
		if (len(pirxFiles) > 1 || len(cFiles) > 0) && !isDirectoryBuild && outputFile == "" {
			return fmt.Errorf("output file (-o) must be specified when compiling multiple files")
		}

		// Get compilation config
		config, err := getCompilationConfig()
		if err != nil {
			return fmt.Errorf("failed to get compilation config: %w", err)
		}

		// Get the keep flag value
		keepIntermediateFiles, _ := cmd.Flags().GetBool("keep")

		// Build the program
		binFile := outputBinaryPath(outputFile, isDirectoryBuild, buildDir, pirxFiles)
		if err := buildProgram(config, pirxFiles, cFiles, keepIntermediateFiles, optLevel, binFile); err != nil {
			cmd.SilenceUsage = true
			return err
		}
		return nil
	},
}

func init() {
	buildCmd.Flags().BoolP("keep", "k", false, "Keep intermediate files (.s, .o) next to the output binary")
	buildCmd.Flags().StringVarP(&optLevel, "O", "O", "", "optimization level (0 to disable)")
	buildCmd.Flags().StringVarP(&outputFile, "o", "o", "", "output file name")
	rootCmd.AddCommand(buildCmd)
}

func main() {
	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}

// findSourceFiles scans a directory for .pirx and .c files
func findSourceFiles(dir string) ([]string, []string, error) {
	var pirxFiles []string
	var cFiles []string
	entries, err := os.ReadDir(dir)
	if err != nil {
		return nil, nil, err
	}

	for _, entry := range entries {
		if !entry.IsDir() {
			name := entry.Name()
			fullPath := filepath.Join(dir, name)
			if strings.HasSuffix(name, ".pirx") {
				pirxFiles = append(pirxFiles, fullPath)
			} else if strings.HasSuffix(name, ".c") {
				cFiles = append(cFiles, fullPath)
			}
		}
	}

	return pirxFiles, cFiles, nil
}

// outputBinaryPath determines where the final binary goes: the -o value if
// given, otherwise inside the build directory or next to the source file.
func outputBinaryPath(outputFile string, isDirectoryBuild bool, buildDir string, pirxFiles []string) string {
	if outputFile != "" {
		return outputFile
	}
	if isDirectoryBuild {
		return filepath.Join(buildDir, filepath.Base(buildDir))
	}
	baseName := strings.TrimSuffix(filepath.Base(pirxFiles[0]), ".pirx")
	return filepath.Join(filepath.Dir(pirxFiles[0]), baseName)
}

// buildProgram compiles one or more pirx files and optionally C files to an
// executable. Intermediate files live in a temporary directory so they can
// never clobber user files (or each other); with keepIntermediate a copy of
// the .s and .o is left next to the output binary.
func buildProgram(config *CompilationConfig, pirxFiles []string, cFiles []string, keepIntermediate bool, optLevel string, binFile string) error {
	// Get PIRX root directory
	pirxRoot, err := getPirxRoot()
	if err != nil {
		return fmt.Errorf("failed to determine PIRXROOT: %w", err)
	}

	tmpDir, err := os.MkdirTemp("", "pirx-build-")
	if err != nil {
		return fmt.Errorf("failed to create temporary build directory: %w", err)
	}
	defer os.RemoveAll(tmpDir)

	baseName := strings.TrimSuffix(filepath.Base(binFile), filepath.Ext(binFile))
	asmFile := filepath.Join(tmpDir, baseName+".s")
	// The fixed object names cannot collide with each other no matter how the
	// input files are named.
	objFile := filepath.Join(tmpDir, "pirx.o")

	stdlibPath := filepath.Join(pirxRoot, "stdlib", "libpirx.a")
	pirxcPath := filepath.Join(pirxRoot, "pirxc")

	// Step 1: Compile .pirx files to .s using pirxc compiler
	args := []string{"-o", asmFile}
	if optLevel != "" {
		args = append(args, "-O"+optLevel)
	}
	args = append(args, pirxFiles...)
	pirxCmd := exec.Command(pirxcPath, args...)
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

	// Step 3: Compile C files to .o files (if any)
	var cObjectFiles []string
	for i, cFile := range cFiles {
		cObjFile := filepath.Join(tmpDir, fmt.Sprintf("c%d.o", i))
		cObjectFiles = append(cObjectFiles, cObjFile)

		// Compile C file to object file
		ccArgs := []string{"-c", "-o", cObjFile, cFile}
		ccCmd := exec.Command("cc", ccArgs...)
		if output, err := ccCmd.CombinedOutput(); err != nil {
			fmt.Fprintf(os.Stderr, "C compilation failed for %s: %v\nOutput: %s\n", cFile, err, string(output))
			return err
		}
	}

	// Step 4: Link all .o files to executable
	ldArgs := []string{"-o", binFile, objFile}
	ldArgs = append(ldArgs, cObjectFiles...)
	ldArgs = append(ldArgs, stdlibPath)
	ldArgs = append(ldArgs, config.LinkerFlags...)
	ldCmd := exec.Command(config.Linker, ldArgs...)
	if output, err := ldCmd.CombinedOutput(); err != nil {
		fmt.Fprintf(os.Stderr, "linking failed: %v\nOutput: %s\n", err, string(output))
		return err
	}

	// With -k, leave a copy of the assembly and the Pirx object next to the
	// output binary for inspection.
	if keepIntermediate {
		outputDir := filepath.Dir(binFile)
		for src, dst := range map[string]string{
			asmFile: filepath.Join(outputDir, baseName+".s"),
			objFile: filepath.Join(outputDir, baseName+".o"),
		} {
			// Never overwrite the binary itself (e.g. pirx build -k foo.pirx -o foo.s).
			if dst == filepath.Clean(binFile) {
				continue
			}
			if err := copyFile(src, dst); err != nil {
				return fmt.Errorf("failed to keep intermediate file %s: %w", dst, err)
			}
		}
	}

	fmt.Printf("Built %s\n", binFile)
	return nil
}

func copyFile(src, dst string) error {
	data, err := os.ReadFile(src)
	if err != nil {
		return err
	}
	return os.WriteFile(dst, data, 0o644)
}

// getCompilationConfig returns compilation configuration for the current platform
func getCompilationConfig() (*CompilationConfig, error) {
	switch runtime.GOOS {
	case "darwin":
		switch runtime.GOARCH {
		case "arm64":
			return &CompilationConfig{
				Assembler:      "as",
				AssemblerFlags: []string{"-arch", "arm64", "-g"},
				Linker:         "ld",
				LinkerFlags:    []string{"-lSystem", "-syslibroot", getMacOsSdkPath(), "-arch", "arm64"},
			}, nil
		}
	case "linux":
		return &CompilationConfig{
			Assembler:      "as",
			AssemblerFlags: []string{"-g"},
			Linker:         "gcc",
			LinkerFlags:    []string{},
		}, nil
	}

	return nil, fmt.Errorf("unsupported platform: %s/%s", runtime.GOOS, runtime.GOARCH)
}

// getPirxRoot returns the Pirx root directory, either from PIRXROOT env var
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
