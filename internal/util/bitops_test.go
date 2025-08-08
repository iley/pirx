package util

import "testing"

func TestAlign(t *testing.T) {
	tests := []struct {
		name      string
		addr      int
		alignment int
		expected  int
	}{
		{
			name:      "already aligned address",
			addr:      16,
			alignment: 8,
			expected:  16,
		},
		{
			name:      "align up to next boundary",
			addr:      13,
			alignment: 8,
			expected:  16,
		},
		{
			name:      "align single byte",
			addr:      1,
			alignment: 4,
			expected:  4,
		},
		{
			name:      "zero address alignment",
			addr:      0,
			alignment: 8,
			expected:  0,
		},
		{
			name:      "align to power of 2",
			addr:      100,
			alignment: 16,
			expected:  112,
		},
		{
			name:      "align to 4-byte boundary",
			addr:      7,
			alignment: 4,
			expected:  8,
		},
		{
			name:      "align to 32-byte boundary",
			addr:      33,
			alignment: 32,
			expected:  64,
		},
		{
			name:      "align small address",
			addr:      1,
			alignment: 1,
			expected:  1,
		},
		{
			name:      "align large address",
			addr:      1000,
			alignment: 64,
			expected:  1024,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := Align(tt.addr, tt.alignment)
			if got != tt.expected {
				t.Errorf("Align(%d, %d) = %d, want %d", tt.addr, tt.alignment, got, tt.expected)
			}
		})
	}
}

func TestMinPowerOfTwo(t *testing.T) {
	tests := []struct {
		name     string
		n        int
		expected int
	}{
		{
			name:     "n = 1",
			n:        1,
			expected: 0,
		},
		{
			name:     "n = 2",
			n:        2,
			expected: 1,
		},
		{
			name:     "n = 3",
			n:        3,
			expected: 2,
		},
		{
			name:     "n = 4",
			n:        4,
			expected: 2,
		},
		{
			name:     "n = 5",
			n:        5,
			expected: 3,
		},
		{
			name:     "n = 8",
			n:        8,
			expected: 3,
		},
		{
			name:     "n = 9",
			n:        9,
			expected: 4,
		},
		{
			name:     "n = 16",
			n:        16,
			expected: 4,
		},
		{
			name:     "n = 17",
			n:        17,
			expected: 5,
		},
		{
			name:     "n = 0",
			n:        0,
			expected: 0,
		},
		{
			name:     "n = 1024",
			n:        1024,
			expected: 10,
		},
		{
			name:     "n = 1025",
			n:        1025,
			expected: 11,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := MinPowerOfTwo(tt.n)
			if got != tt.expected {
				t.Errorf("MinPowerOfTwo(%d) = %d, want %d", tt.n, got, tt.expected)
			}
		})
	}
}
