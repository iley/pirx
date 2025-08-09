package util

func Int32Ptr(value int) *int32 {
	int32val := int32(value)
	return &int32val
}

func Int64Ptr(value int64) *int64 {
	return &value
}
