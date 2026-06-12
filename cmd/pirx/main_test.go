package main

import "testing"

func TestOutputBinaryPath(t *testing.T) {
	tests := []struct {
		name             string
		outputFile       string
		isDirectoryBuild bool
		buildDir         string
		pirxFiles        []string
		want             string
	}{
		{
			name:       "explicit output file wins",
			outputFile: "bin/app",
			pirxFiles:  []string{"src/foo.pirx"},
			want:       "bin/app",
		},
		{
			name:       "explicit output file with .s extension is used as-is",
			outputFile: "foo.s",
			pirxFiles:  []string{"foo.pirx"},
			want:       "foo.s",
		},
		{
			name:             "directory build uses directory name",
			isDirectoryBuild: true,
			buildDir:         "tests/068_mixed_c_pirx",
			pirxFiles:        []string{"tests/068_mixed_c_pirx/main.pirx"},
			want:             "tests/068_mixed_c_pirx/068_mixed_c_pirx",
		},
		{
			name:      "single file builds next to the source",
			pirxFiles: []string{"tests/001_hello.pirx"},
			want:      "tests/001_hello",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := outputBinaryPath(tt.outputFile, tt.isDirectoryBuild, tt.buildDir, tt.pirxFiles)
			if got != tt.want {
				t.Errorf("outputBinaryPath() = %q, want %q", got, tt.want)
			}
		})
	}
}
