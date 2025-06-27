package util

func Align(addr int64, alignment int) int64 {
	return (addr + int64(alignment) - 1) &^ (int64(alignment) - 1)
}
