package parser

import "strings"

// RustImportRef parses the path of `import rust "<crate>@<semver>" as
// <alias>` into its (crate, version) pair.
//
// The string is expected to carry exactly one `@`, with non-empty parts
// on either side. The crate part must additionally be a syntactically
// valid Cargo identifier: lowercase ASCII letters, digits, `_`, and `-`,
// starting with a letter. The version part is left to the bridge's
// semver parser (package3/rust/semver); this routine only enforces the
// `<crate>@<rest>` shape so a malformed path produces a parse-time
// diagnostic rather than a later wrapper-build failure.
//
// On success ok is true; on any structural problem ok is false and the
// returned crate / version are empty.
func RustImportRef(path string) (crate, version string, ok bool) {
	s := strings.Trim(path, "\"")
	i := strings.Index(s, "@")
	if i <= 0 || i == len(s)-1 {
		return "", "", false
	}
	c := s[:i]
	if !isCargoCrateName(c) {
		return "", "", false
	}
	v := s[i+1:]
	if strings.ContainsAny(v, " \t\n") {
		return "", "", false
	}
	return c, v, true
}

// isCargoCrateName implements the conservative subset of Cargo's crate
// naming rules that we accept here: ASCII lowercase letters, digits, `_`,
// `-`; must start with a letter; length 1..64.
//
// Cargo itself accepts a slightly wider set (the upper-case path was
// retired in 2018), but every crates.io publish since then is in the
// strict subset. The parser stays strict so we catch typos.
func isCargoCrateName(s string) bool {
	if len(s) == 0 || len(s) > 64 {
		return false
	}
	if !isLowerLetter(s[0]) {
		return false
	}
	for i := 0; i < len(s); i++ {
		c := s[i]
		switch {
		case isLowerLetter(c), c >= '0' && c <= '9', c == '_', c == '-':
			// ok
		default:
			return false
		}
	}
	return true
}

func isLowerLetter(c byte) bool { return c >= 'a' && c <= 'z' }
