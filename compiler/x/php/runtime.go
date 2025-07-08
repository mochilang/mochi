//go:build slow

package phpcode

// helperMap stores snippets of runtime helper functions that the compiler
// can emit if needed. The PHP backend currently has no helpers.
var helperMap = map[string]string{}
