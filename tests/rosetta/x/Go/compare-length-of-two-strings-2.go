//go:build ignore
// +build ignore

comparisonFunction := func(i, j int) bool {
	return len(strings[i]) > len(strings[j])
}

sort.SliceStable(strings, comparisonFunction)
