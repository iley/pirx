package util

func Align64(addr int64, alignment int) int64 {
	return (addr + int64(alignment) - 1) &^ (int64(alignment) - 1)
}

func Align(addr int, alignment int) int {
	return (addr + int(alignment) - 1) &^ (int(alignment) - 1)
}

func MinPowerOfTwo(n int) int {
	if n <= 1 {
		return 0
	}
	power := 0
	value := 1
	for value < n {
		value <<= 1
		power++
	}
	return power
}
