package util

func Align64(addr int64, alignment int) int64 {
	return (addr + int64(alignment) - 1) &^ (int64(alignment) - 1)
}

func Align(addr int, alignment int) int {
	return (addr + int(alignment) - 1) &^ (int(alignment) - 1)
}
