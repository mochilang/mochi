// Package moduleproxy is the MEP-74 client for the Go module proxy
// protocol (proxy.golang.org by default; GOPROXY overrides). Phase 1
// lands the path escaping, the four endpoint methods (list, info, mod,
// zip), the h1: dirhash computation that matches go.sum, and a
// content-addressed cache.
package moduleproxy

import (
	"fmt"
	"strings"
)

// EscapePath converts an arbitrary module path to its proxy-safe form.
// The Go module proxy URL must contain only lower-case letters; each
// upper-case letter A-Z is encoded as "!a" (etc.). This matches the
// behaviour of golang.org/x/mod/module.EscapePath.
//
// Reference: https://go.dev/ref/mod#goproxy-protocol
//
// Examples:
//
//	"github.com/Spf13/Cobra"   -> "github.com/!spf13/!cobra"
//	"github.com/spf13/cobra"   -> "github.com/spf13/cobra"
//	"k8s.io/api"               -> "k8s.io/api"
//
// EscapePath returns an error if path is empty.
func EscapePath(path string) (string, error) {
	if path == "" {
		return "", fmt.Errorf("moduleproxy: empty module path")
	}
	if !pathHasUpper(path) {
		return path, nil
	}
	var b strings.Builder
	b.Grow(len(path) + 8)
	for _, r := range path {
		if r >= 'A' && r <= 'Z' {
			b.WriteByte('!')
			b.WriteRune(r - 'A' + 'a')
		} else {
			b.WriteRune(r)
		}
	}
	return b.String(), nil
}

// EscapeVersion converts a module version to its proxy-safe form. The
// rules are identical to EscapePath: upper-case A-Z become "!a".
// EscapeVersion additionally rejects empty versions and versions
// containing characters that would be ambiguous in a URL path.
func EscapeVersion(version string) (string, error) {
	if version == "" {
		return "", fmt.Errorf("moduleproxy: empty version")
	}
	for _, r := range version {
		if r == '/' || r == '\\' || r == ':' || r < 0x20 || r == 0x7f {
			return "", fmt.Errorf("moduleproxy: version %q contains forbidden character %q", version, r)
		}
	}
	if !pathHasUpper(version) {
		return version, nil
	}
	var b strings.Builder
	b.Grow(len(version) + 4)
	for _, r := range version {
		if r >= 'A' && r <= 'Z' {
			b.WriteByte('!')
			b.WriteRune(r - 'A' + 'a')
		} else {
			b.WriteRune(r)
		}
	}
	return b.String(), nil
}

// UnescapePath is the inverse of EscapePath. "!a" decodes to "A". An
// orphan "!" or one followed by an upper-case letter is an error.
func UnescapePath(path string) (string, error) {
	if path == "" {
		return "", fmt.Errorf("moduleproxy: empty escaped path")
	}
	if !strings.ContainsRune(path, '!') {
		return path, nil
	}
	var b strings.Builder
	b.Grow(len(path))
	runes := []rune(path)
	for i := 0; i < len(runes); i++ {
		r := runes[i]
		if r != '!' {
			b.WriteRune(r)
			continue
		}
		if i+1 >= len(runes) {
			return "", fmt.Errorf("moduleproxy: trailing ! in %q", path)
		}
		next := runes[i+1]
		if next < 'a' || next > 'z' {
			return "", fmt.Errorf("moduleproxy: ! must be followed by lower-case letter in %q", path)
		}
		b.WriteRune(next - 'a' + 'A')
		i++
	}
	return b.String(), nil
}

// UnescapeVersion is the inverse of EscapeVersion. Identical rules to
// UnescapePath.
func UnescapeVersion(version string) (string, error) {
	return UnescapePath(version)
}

func pathHasUpper(s string) bool {
	for _, r := range s {
		if r >= 'A' && r <= 'Z' {
			return true
		}
	}
	return false
}
