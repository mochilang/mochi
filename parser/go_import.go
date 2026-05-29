package parser

import "strings"

// GoImportRef parses the path of `import go "<module>@<semver>" as
// <alias>` (the MEP-74 phase 8 form) into its (module, version) pair.
//
// The path may carry quotes (the participle lexer strips them, but
// callers that pass raw lexed tokens may include them); the routine
// trims a single leading and trailing double-quote before parsing.
//
// On success ok is true and module/version carry non-empty values.
// On any structural problem (missing `@`, empty side, non-conforming
// module path syntax, whitespace in version) ok is false and module
// and version are empty.
//
// MEP-74 does *not* require the `@<semver>` shape for `import go`:
// older MEP-54-phase-10 FFI imports of stdlib packages (`import go
// "fmt"`, `import go "net/http"`) remain valid and produce ok=false
// here, with the caller responsible for distinguishing the two cases
// via [HasGoSemverPin].
func GoImportRef(path string) (module, version string, ok bool) {
	s := strings.Trim(path, "\"")
	at := strings.Index(s, "@")
	if at <= 0 || at == len(s)-1 {
		return "", "", false
	}
	m := s[:at]
	if !isGoModulePath(m) {
		return "", "", false
	}
	v := s[at+1:]
	if v == "" || strings.ContainsAny(v, " \t\n\r") {
		return "", "", false
	}
	return m, v, true
}

// HasGoSemverPin reports whether the path carries an `@<semver>` tail.
// It is the cheap precheck the parser uses to decide whether to apply
// the strict GoImportRef validator or treat the import as a stdlib /
// pre-MEP-74 FFI form (where no version pin is required).
func HasGoSemverPin(path string) bool {
	s := strings.Trim(path, "\"")
	return strings.Contains(s, "@")
}

// isGoModulePath reports whether s is a syntactically valid Go module
// path. It applies a conservative subset of golang.org/ref/mod#go-mod-file-ident:
//
//   - path is split on `/`; every segment must be non-empty
//   - every segment is composed of ASCII letters/digits and the
//     punctuation set {`.`, `_`, `-`, `~`}; the first character of a
//     segment must be a letter or digit; consecutive dots are rejected
//   - the first segment must contain at least one `.` (the Go convention
//     for fully-qualified module paths like "github.com/...", "gopkg.in/...";
//     this strict subset rejects "fmt" and "net/http" so stdlib imports
//     never accidentally satisfy the version-pin contract)
//
// The Go reference accepts a slightly wider character set (notably the
// punctuation `+`); restricting to the strict subset keeps the parser's
// diagnostic precise and matches every published module on proxy.golang.org
// as of April 2026.
func isGoModulePath(s string) bool {
	if s == "" {
		return false
	}
	segs := strings.Split(s, "/")
	if len(segs) == 0 {
		return false
	}
	// First segment must contain a dot (FQDN-style).
	if !strings.Contains(segs[0], ".") {
		return false
	}
	for _, seg := range segs {
		if !isGoModuleSegment(seg) {
			return false
		}
	}
	return true
}

// isGoModuleSegment reports whether seg is a valid Go module path
// segment under the conservative subset described in isGoModulePath.
func isGoModuleSegment(seg string) bool {
	if seg == "" {
		return false
	}
	// First char must be alnum.
	if !isGoModuleStartByte(seg[0]) {
		return false
	}
	for i := 0; i < len(seg); i++ {
		c := seg[i]
		switch {
		case c >= 'a' && c <= 'z',
			c >= 'A' && c <= 'Z',
			c >= '0' && c <= '9',
			c == '.' || c == '_' || c == '-' || c == '~':
			// ok
		default:
			return false
		}
		// Reject consecutive dots (eg. "..", "foo..bar").
		if c == '.' && i+1 < len(seg) && seg[i+1] == '.' {
			return false
		}
	}
	return true
}

func isGoModuleStartByte(c byte) bool {
	switch {
	case c >= 'a' && c <= 'z',
		c >= 'A' && c <= 'Z',
		c >= '0' && c <= '9':
		return true
	}
	return false
}
