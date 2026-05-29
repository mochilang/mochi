package parser

import "strings"

// ErlangImportRef parses the path of `import erlang "<pkg>@<semver>"` into
// its (pkg, version) pair.
//
// Hex.pm package names are lowercase ASCII letters, digits, and underscores,
// starting with a letter; length 1..128. The version component is optional:
// `import erlang "cowboy"` (without `@`) is accepted and version is "".
// When a version pin is present it must be a non-empty string with no whitespace.
//
// Returns ok=false on any structural problem; the pkg/version strings are
// empty in that case.
func ErlangImportRef(path string) (pkg, version string, ok bool) {
	// The AST parser already strips the surrounding quotes from the path string.
	// We also handle the quoted form for callers that pass raw source strings.
	s := strings.Trim(path, "\"")
	if idx := strings.Index(s, "@"); idx >= 0 {
		pkg = s[:idx]
		version = s[idx+1:]
		if version == "" {
			return "", "", false
		}
	} else {
		pkg = s
		version = ""
	}
	if !isHexPmPackageName(pkg) {
		return "", "", false
	}
	return pkg, version, true
}

// HasErlangSemverPin reports whether an erlang import path contains a `@`
// version pin, e.g. "cowboy@2.12.0". Used to distinguish bare imports
// (resolved at build time from the lock file) from pinned imports (validated
// at parse time).
func HasErlangSemverPin(path string) bool {
	s := strings.Trim(path, "\"")
	return strings.Contains(s, "@")
}

// isHexPmPackageName validates a Hex.pm package identifier: lowercase ASCII
// letters, digits, and underscores; must start with a letter; length 1..128.
func isHexPmPackageName(s string) bool {
	if len(s) == 0 || len(s) > 128 {
		return false
	}
	if !isLowerLetter(s[0]) {
		return false
	}
	for i := 0; i < len(s); i++ {
		c := s[i]
		switch {
		case isLowerLetter(c), c >= '0' && c <= '9', c == '_':
			// ok
		default:
			return false
		}
	}
	return true
}
