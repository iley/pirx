package util

import "fmt"

func Slice16bits(val int64, offsetBits int) string {
	res := (val >> offsetBits) & 0xffff
	return fmt.Sprintf("#0x%04X", res)
}

func Align(addr int64, alignment int) int64 {
	return (addr + int64(alignment) - 1) &^ (int64(alignment) - 1)
}
