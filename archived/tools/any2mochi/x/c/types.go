//go:build archive

package c

// function describes a parsed C function.
// Additional location information is useful for better diagnostics
// and potential future features.
type function struct {
	name      string
	ret       string
	params    []param
	body      []string
	startLine int    // 1-indexed line of the function definition
	endLine   int    // 1-indexed line of the closing brace
	source    string // full source snippet of the function
	signature string // original clang type signature
}

type param struct {
	name string
	typ  string
}
