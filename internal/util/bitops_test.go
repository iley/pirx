package util

import "testing"

func TestSlice16bits(t *testing.T) {
	tests := []struct {
		name       string
		val        int64
		offsetBits int
		expected   string
	}{
		{
			name:       "zero value",
			val:        0,
			offsetBits: 0,
			expected:   "#0x0000",
		},
		{
			name:       "no offset",
			val:        0x123456789,
			offsetBits: 0,
			expected:   "#0x6789",
		},
		{
			name:       "offset 8",
			val:        0x12345678,
			offsetBits: 8,
			expected:   "#0x3456",
		},
		{
			name:       "offset 16",
			val:        0x12345678,
			offsetBits: 16,
			expected:   "#0x1234",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := Slice16bits(tt.val, tt.offsetBits)
			if got != tt.expected {
				t.Errorf("Slice16bits(%d, %d) = %s, want %s", tt.val, tt.offsetBits, got, tt.expected)
			}
		})
	}
}

func TestAlign(t *testing.T) {
	tests := []struct {
		name      string
		addr      int64
		alignment int
		expected  int64
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
