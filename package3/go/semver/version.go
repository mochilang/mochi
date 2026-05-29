// Package semver is the MEP-74 implementation of Go's module semver
// dialect. It does not aim for full SemVer 2.0 conformance; it matches
// the subset used by `cmd/go` and `golang.org/x/mod/semver` so that
// version sorting and Major/Minor extraction behave consistently with
// the rest of the Go ecosystem.
//
// Differences from spec SemVer:
//
//   - Versions are prefixed "v" (e.g. "v1.2.3").
//   - The pseudo-version family "v0.0.0-YYYYMMDDHHMMSS-abcdef" is recognized.
//   - "+incompatible" build metadata is preserved verbatim during parsing
//     but is ignored when comparing versions (matching cmd/go).
//
// The Go module proxy ordering of @v/list output is *lexicographic on
// the wire*; ordering by semver is the caller's responsibility, which
// is what Sort/Compare provide.
package semver

import (
	"fmt"
	"sort"
	"strconv"
	"strings"
)

// Version is a parsed module-semver string. Fields are populated by
// Parse; callers should not construct Version directly because the
// raw form is the authoritative canonical representation.
type Version struct {
	// Raw is the input string exactly as passed to Parse (after a
	// minimal validity check).
	Raw string
	// Major, Minor, Patch are the integer components.
	Major, Minor, Patch int
	// Pre is the dot-separated pre-release identifier list. Empty
	// slice for release versions.
	Pre []string
	// Build is the dot-separated build metadata list. Empty slice
	// when no "+..." suffix was present.
	Build []string
}

// Parse returns a parsed Version. The string must start with "v" and
// be in the form "v MAJOR . MINOR . PATCH [ - PRE ] [ + BUILD ]" with
// non-negative integer parts. Returns an error for any other shape.
func Parse(s string) (*Version, error) {
	if s == "" {
		return nil, fmt.Errorf("semver: empty version")
	}
	if s[0] != 'v' {
		return nil, fmt.Errorf("semver: %q does not start with 'v'", s)
	}
	body := s[1:]
	// Split off build metadata first so a '-' inside it would not be
	// mistaken for the prerelease separator.
	var build []string
	if i := strings.IndexByte(body, '+'); i >= 0 {
		buildStr := body[i+1:]
		body = body[:i]
		if buildStr == "" {
			return nil, fmt.Errorf("semver: empty build metadata in %q", s)
		}
		build = strings.Split(buildStr, ".")
		for _, id := range build {
			if id == "" {
				return nil, fmt.Errorf("semver: empty build identifier in %q", s)
			}
			if !isIdentChars(id) {
				return nil, fmt.Errorf("semver: invalid build identifier %q in %q", id, s)
			}
		}
	}
	var pre []string
	if i := strings.IndexByte(body, '-'); i >= 0 {
		preStr := body[i+1:]
		body = body[:i]
		if preStr == "" {
			return nil, fmt.Errorf("semver: empty pre-release in %q", s)
		}
		pre = strings.Split(preStr, ".")
		for _, id := range pre {
			if id == "" {
				return nil, fmt.Errorf("semver: empty pre-release identifier in %q", s)
			}
			if !isIdentChars(id) {
				return nil, fmt.Errorf("semver: invalid pre-release identifier %q in %q", id, s)
			}
			if isAllDigits(id) && len(id) > 1 && id[0] == '0' {
				return nil, fmt.Errorf("semver: numeric pre-release id %q has leading zero in %q", id, s)
			}
		}
	}
	parts := strings.Split(body, ".")
	if len(parts) != 3 {
		return nil, fmt.Errorf("semver: %q is not MAJOR.MINOR.PATCH", s)
	}
	nums := [3]int{}
	for i, part := range parts {
		if part == "" {
			return nil, fmt.Errorf("semver: empty version component in %q", s)
		}
		if len(part) > 1 && part[0] == '0' {
			return nil, fmt.Errorf("semver: leading zero in %q", s)
		}
		n, err := strconv.Atoi(part)
		if err != nil || n < 0 {
			return nil, fmt.Errorf("semver: non-integer %q in %q", part, s)
		}
		nums[i] = n
	}
	return &Version{
		Raw:   s,
		Major: nums[0],
		Minor: nums[1],
		Patch: nums[2],
		Pre:   pre,
		Build: build,
	}, nil
}

// IsValid reports whether s is a parseable module-semver string.
func IsValid(s string) bool {
	_, err := Parse(s)
	return err == nil
}

// Compare returns -1, 0, +1 comparing a and b under Go's module
// semver ordering. The "+build" suffix is ignored for comparison.
// Pre-release versions are less than the corresponding release.
//
// Pseudo-versions sort against tagged versions using their numeric
// "v0.0.0-..." form, so a pseudo-version derived from a commit on
// `main` typically sorts *before* any tag that begins with v1+.
func Compare(a, b *Version) int {
	if a == nil || b == nil {
		switch {
		case a == nil && b == nil:
			return 0
		case a == nil:
			return -1
		default:
			return +1
		}
	}
	if c := cmpInt(a.Major, b.Major); c != 0 {
		return c
	}
	if c := cmpInt(a.Minor, b.Minor); c != 0 {
		return c
	}
	if c := cmpInt(a.Patch, b.Patch); c != 0 {
		return c
	}
	return comparePre(a.Pre, b.Pre)
}

// CompareStrings is a convenience that parses both sides and falls
// back to lexicographic comparison if either fails to parse. Sort
// helpers built on this never panic on malformed inputs.
func CompareStrings(a, b string) int {
	va, errA := Parse(a)
	vb, errB := Parse(b)
	switch {
	case errA == nil && errB == nil:
		return Compare(va, vb)
	case errA != nil && errB == nil:
		return +1
	case errA == nil && errB != nil:
		return -1
	default:
		switch {
		case a < b:
			return -1
		case a > b:
			return +1
		default:
			return 0
		}
	}
}

// Sort sorts versions in ascending order. Invalid versions sort after
// valid ones, then lexicographically among themselves.
func Sort(versions []string) {
	sort.Slice(versions, func(i, j int) bool {
		return CompareStrings(versions[i], versions[j]) < 0
	})
}

// Max returns the highest version in the slice, or "" if empty. Ties
// (same precedence) resolve to whichever appears first.
func Max(versions []string) string {
	if len(versions) == 0 {
		return ""
	}
	best := versions[0]
	for _, v := range versions[1:] {
		if CompareStrings(v, best) > 0 {
			best = v
		}
	}
	return best
}

// IsPrerelease reports whether v has a non-empty pre-release tag.
func (v *Version) IsPrerelease() bool {
	return v != nil && len(v.Pre) > 0
}

// IsPseudoVersion reports whether v looks like a "v0.0.0-YYYYMMDDHHMMSS-abcdef"
// pseudo-version (14-digit timestamp + 12-hex-char commit prefix).
//
// The pre-release portion of a pseudo-version is a single SemVer
// identifier "<timestamp>-<sha>" (since '-' is a valid identifier
// character); we recognise that shape rather than two distinct
// dot-separated pre-release components.
func (v *Version) IsPseudoVersion() bool {
	if v == nil || len(v.Pre) != 1 {
		return false
	}
	id := v.Pre[0]
	// timestamp(14) + '-' + sha(12) = 27 characters
	if len(id) != 27 || id[14] != '-' {
		return false
	}
	if !isAllDigits(id[:14]) {
		return false
	}
	for _, r := range id[15:] {
		if !((r >= '0' && r <= '9') || (r >= 'a' && r <= 'f')) {
			return false
		}
	}
	return true
}

// MajorString returns just the major version selector, e.g. "v1". This
// is the form that appears in module paths past the v2 boundary
// ("example.com/foo/v3").
func (v *Version) MajorString() string {
	if v == nil {
		return ""
	}
	return fmt.Sprintf("v%d", v.Major)
}

// String renders the canonical form of v. For successful Parse output
// this is the same as Raw, but reconstructing from fields lets callers
// build a version programmatically.
func (v *Version) String() string {
	if v == nil {
		return ""
	}
	out := fmt.Sprintf("v%d.%d.%d", v.Major, v.Minor, v.Patch)
	if len(v.Pre) > 0 {
		out += "-" + strings.Join(v.Pre, ".")
	}
	if len(v.Build) > 0 {
		out += "+" + strings.Join(v.Build, ".")
	}
	return out
}

func comparePre(a, b []string) int {
	switch {
	case len(a) == 0 && len(b) == 0:
		return 0
	case len(a) == 0:
		// Release (no pre) sorts after pre-release.
		return +1
	case len(b) == 0:
		return -1
	}
	for i := 0; i < len(a) && i < len(b); i++ {
		if c := compareIdent(a[i], b[i]); c != 0 {
			return c
		}
	}
	return cmpInt(len(a), len(b))
}

func compareIdent(a, b string) int {
	aNum, aIsNum := isNumericIdent(a)
	bNum, bIsNum := isNumericIdent(b)
	switch {
	case aIsNum && bIsNum:
		return cmpInt(aNum, bNum)
	case aIsNum:
		return -1
	case bIsNum:
		return +1
	default:
		switch {
		case a < b:
			return -1
		case a > b:
			return +1
		default:
			return 0
		}
	}
}

func isNumericIdent(s string) (int, bool) {
	if s == "" {
		return 0, false
	}
	if !isAllDigits(s) {
		return 0, false
	}
	n, err := strconv.Atoi(s)
	if err != nil {
		return 0, false
	}
	return n, true
}

func isAllDigits(s string) bool {
	if s == "" {
		return false
	}
	for _, r := range s {
		if r < '0' || r > '9' {
			return false
		}
	}
	return true
}

func isIdentChars(s string) bool {
	for _, r := range s {
		switch {
		case r >= '0' && r <= '9':
		case r >= 'a' && r <= 'z':
		case r >= 'A' && r <= 'Z':
		case r == '-':
		default:
			return false
		}
	}
	return true
}

func cmpInt(a, b int) int {
	switch {
	case a < b:
		return -1
	case a > b:
		return +1
	default:
		return 0
	}
}
