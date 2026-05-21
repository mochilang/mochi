// Package strings is the Mochi-semantics string runtime for the Go
// target. It is a thin shim over the Go standard library for the
// operations where Mochi and Go agree, and a real implementation for
// the operations where they don't (notably Reverse, which Mochi
// defines on the rune sequence, and Substr, which uses Mochi's
// inclusive-exclusive byte-position semantics matching the VM's
// OpSlice).
//
// MEP-43 §3.3 requires the emitter to call this package rather than
// inline its own string formatting per call site.
package strings

import (
	"strings"
	"unicode"
)

// Upper returns s with every Unicode letter mapped to its title case,
// matching Mochi's `upper(s)` builtin (VM op OpUpper).
func Upper(s string) string { return strings.ToUpper(s) }

// Lower returns s with every Unicode letter mapped to lower case,
// matching Mochi's `lower(s)` builtin (VM op OpLower).
func Lower(s string) string { return strings.ToLower(s) }

// Reverse returns s with its runes reversed. This is the rune-level
// definition Mochi uses (VM op OpReverse on a ValueStr); a byte-level
// reverse would corrupt multi-byte runes such as `"héllo"`.
func Reverse(s string) string {
	runes := []rune(s)
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}

// Contains reports whether substr appears in s. Matches Mochi's
// `contains` selector on a string.
func Contains(s, substr string) bool { return strings.Contains(s, substr) }

// IndexOf returns the byte index of the first occurrence of substr in
// s, or -1 if substr is not present. Mochi's `indexOf` builtin returns
// a byte index for parity with the VM's `OpIndex` semantics.
func IndexOf(s, substr string) int { return strings.Index(s, substr) }

// Split slices s into all substrings separated by sep and returns a
// slice of the substrings between those separators. Matches Mochi's
// `split` builtin.
func Split(s, sep string) []string { return strings.Split(s, sep) }

// Join concatenates the elements of parts to create a single string.
// The separator string sep is placed between elements. Matches
// Mochi's `join` builtin.
func Join(parts []string, sep string) string { return strings.Join(parts, sep) }

// Replace returns a copy of s with all non-overlapping instances of
// old replaced by new. Matches Mochi's `replace` builtin (which is
// the all-occurrences form; the VM does not expose a count argument).
func Replace(s, old, new string) string { return strings.ReplaceAll(s, old, new) }

// TrimSpace returns a slice of s with all leading and trailing
// Unicode whitespace removed. Matches Mochi's `trim` builtin.
func TrimSpace(s string) string { return strings.TrimSpace(s) }

// Substr returns the rune sub-sequence of s from index start to end
// (exclusive), matching Mochi's `substr(s, start, end)` builtin (and
// `substring`, which is the same operation under a different name in
// the VM dispatch table). start and end are rune indices. If start or
// end is out of range it is clamped, with start clamped before end so
// that an inverted range yields the empty string rather than panicking.
func Substr(s string, start, end int) string {
	runes := []rune(s)
	n := len(runes)
	if start < 0 {
		start = 0
	}
	if end > n {
		end = n
	}
	if start > end {
		return ""
	}
	return string(runes[start:end])
}

// HasPrefix reports whether s begins with prefix.
func HasPrefix(s, prefix string) bool { return strings.HasPrefix(s, prefix) }

// HasSuffix reports whether s ends with suffix.
func HasSuffix(s, suffix string) bool { return strings.HasSuffix(s, suffix) }

// IsWhitespace reports whether every rune in s is Unicode whitespace.
// An empty string is considered all-whitespace, matching the
// vacuous-truth convention the VM uses for `count` over empty lists.
func IsWhitespace(s string) bool {
	for _, r := range s {
		if !unicode.IsSpace(r) {
			return false
		}
	}
	return true
}
