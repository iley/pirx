package util

import "fmt"

func Slice16bits(val int, offsetBits int) string {
	res := (val << offsetBits) & 0xffff
	return fmt.Sprintf("#0x%04X", res)
}

func Align(addr, alignment int) int {
	return (addr + alignment - 1) &^ (alignment - 1)
}
