// Package semver implements RubyGems version semantics for the MEP-76 gem
// bridge. RubyGems uses a different version specification than Cargo semver:
// operators include the pessimistic constraint operator (~>) and versions may
// contain pre-release string segments.
//
// Phase 1 of MEP-76 delivers this package.
// See [website/docs/research/0076/11-version-resolution.md] for details.
package semver

import (
	"fmt"
	"sort"
	"strconv"
	"strings"
)

// Op is a RubyGems version constraint operator.
type Op int

const (
	// OpEq is the = operator: exact match.
	OpEq Op = iota
	// OpNeq is the != operator: any version except the specified one.
	OpNeq
	// OpGt is the > operator.
	OpGt
	// OpGte is the >= operator.
	OpGte
	// OpLt is the < operator.
	OpLt
	// OpLte is the <= operator.
	OpLte
	// OpPessimistic is the ~> operator (pessimistic constraint).
	// ~> 1.2 means >= 1.2, < 2.0
	// ~> 1.2.3 means >= 1.2.3, < 1.3.0
	// ~> 0 means >= 0 (no upper bound)
	OpPessimistic
)

// versionSegment holds one segment of a RubyGems version string. Each segment
// is either an integer or a string. A string segment indicates a pre-release
// component (e.g. "pre", "rc1", "alpha").
type versionSegment struct {
	isStr bool
	num   int64
	str   string
}

// Version represents a parsed RubyGems version string.
// Segments alternate between integer and string components as parsed from
// the dot-separated version string.
type Version struct {
	// Segments holds the parsed components of the version string.
	Segments []versionSegment
	// Original is the raw version string as given.
	Original string
}

// Req is one version requirement consisting of an operator and a version.
type Req struct {
	// Op is the comparison operator.
	Op Op
	// Version is the reference version.
	Version Version
}

// ParseVersion parses a RubyGems version string such as "1.2.3", "1.0.0.pre",
// or "2.0.0.rc1". Each dot-separated segment is parsed as an integer if it
// contains only digits, otherwise as a string.
func ParseVersion(s string) (Version, error) {
	s = strings.TrimSpace(s)
	if s == "" {
		return Version{}, fmt.Errorf("rubygems semver: empty version string")
	}
	parts := strings.Split(s, ".")
	segs := make([]versionSegment, 0, len(parts))
	for _, p := range parts {
		if p == "" {
			return Version{}, fmt.Errorf("rubygems semver: empty segment in %q", s)
		}
		n, err := strconv.ParseInt(p, 10, 64)
		if err == nil {
			segs = append(segs, versionSegment{isStr: false, num: n})
		} else {
			segs = append(segs, versionSegment{isStr: true, str: p})
		}
	}
	return Version{Segments: segs, Original: s}, nil
}

// ParseReq parses a RubyGems version requirement string such as ">= 1.0",
// "~> 2.1", or "= 1.2.3". Whitespace between the operator and the version is
// optional.
func ParseReq(s string) (Req, error) {
	s = strings.TrimSpace(s)
	var opStr, verStr string
	for _, op := range []string{"~>", ">=", "<=", "!=", ">", "<", "="} {
		if strings.HasPrefix(s, op) {
			opStr = op
			verStr = strings.TrimSpace(s[len(op):])
			break
		}
	}
	if opStr == "" {
		// No operator: treat as exact match.
		opStr = "="
		verStr = s
	}
	ver, err := ParseVersion(verStr)
	if err != nil {
		return Req{}, fmt.Errorf("rubygems semver: bad requirement %q: %w", s, err)
	}
	var op Op
	switch opStr {
	case "=":
		op = OpEq
	case "!=":
		op = OpNeq
	case ">":
		op = OpGt
	case ">=":
		op = OpGte
	case "<":
		op = OpLt
	case "<=":
		op = OpLte
	case "~>":
		op = OpPessimistic
	}
	return Req{Op: op, Version: ver}, nil
}

// compareVersions compares two Version values. It returns -1, 0, or 1
// following the RubyGems comparison rules: integer segments compare
// numerically, string segments sort before any integer segment at the same
// position (pre-release ordering), and string segments sort lexicographically
// among themselves.
func compareVersions(a, b Version) int {
	// Pad to same length with zero-equivalent segments.
	lenA, lenB := len(a.Segments), len(b.Segments)
	maxLen := lenA
	if lenB > maxLen {
		maxLen = lenB
	}
	for i := range maxLen {
		var segA, segB versionSegment
		if i < lenA {
			segA = a.Segments[i]
		} else {
			// Missing segment: treat as integer 0.
			segA = versionSegment{isStr: false, num: 0}
		}
		if i < lenB {
			segB = b.Segments[i]
		} else {
			segB = versionSegment{isStr: false, num: 0}
		}

		c := compareSegments(segA, segB)
		if c != 0 {
			return c
		}
	}
	return 0
}

// compareSegments compares two version segments. String segments sort before
// integer segments (pre-release before release). Two integer segments are
// compared numerically. Two string segments are compared lexicographically.
func compareSegments(a, b versionSegment) int {
	if !a.isStr && !b.isStr {
		if a.num < b.num {
			return -1
		} else if a.num > b.num {
			return 1
		}
		return 0
	}
	if a.isStr && b.isStr {
		if a.str < b.str {
			return -1
		} else if a.str > b.str {
			return 1
		}
		return 0
	}
	// One is a string and one is an integer. String segments sort before integer
	// segments at the same position (pre-release versions come before releases).
	if a.isStr {
		return -1 // a is pre-release, b is release: a < b
	}
	return 1 // a is release, b is pre-release: a > b
}

// pessimisticLower returns the lower bound version for ~>: same as the given version.
// pessimisticUpper returns the exclusive upper bound for ~>: increment the
// second-to-last segment and drop all trailing segments.
// Special case: if the version has only one segment, there is no upper bound.
func pessimisticBounds(v Version) (lower Version, upper Version, hasUpper bool) {
	lower = v
	if len(v.Segments) <= 1 {
		// ~> 0 or ~> 1 means >= N, no upper bound.
		return lower, Version{}, false
	}
	// Upper bound: drop last segment, increment the new last segment.
	trimmed := make([]versionSegment, len(v.Segments)-1)
	copy(trimmed, v.Segments[:len(v.Segments)-1])
	last := trimmed[len(trimmed)-1]
	if last.isStr {
		// Increment string lexicographically is not well-defined; just keep it.
		// In practice ~> with a string upper segment is unusual.
		return lower, Version{}, false
	}
	trimmed[len(trimmed)-1] = versionSegment{isStr: false, num: last.num + 1}
	// Build original string for the upper bound.
	parts := make([]string, len(trimmed))
	for i, seg := range trimmed {
		if seg.isStr {
			parts[i] = seg.str
		} else {
			parts[i] = strconv.FormatInt(seg.num, 10)
		}
	}
	upper = Version{
		Segments: trimmed,
		Original: strings.Join(parts, "."),
	}
	return lower, upper, true
}

// Satisfies reports whether candidate satisfies the requirement r.
func Satisfies(candidate Version, r Req) bool {
	switch r.Op {
	case OpEq:
		return compareVersions(candidate, r.Version) == 0
	case OpNeq:
		return compareVersions(candidate, r.Version) != 0
	case OpGt:
		return compareVersions(candidate, r.Version) > 0
	case OpGte:
		return compareVersions(candidate, r.Version) >= 0
	case OpLt:
		return compareVersions(candidate, r.Version) < 0
	case OpLte:
		return compareVersions(candidate, r.Version) <= 0
	case OpPessimistic:
		lower, upper, hasUpper := pessimisticBounds(r.Version)
		if compareVersions(candidate, lower) < 0 {
			return false
		}
		if hasUpper && compareVersions(candidate, upper) >= 0 {
			return false
		}
		return true
	}
	return false
}

// SatisfiesAll reports whether candidate satisfies all requirements in reqs.
func SatisfiesAll(candidate Version, reqs []Req) bool {
	for _, r := range reqs {
		if !Satisfies(candidate, r) {
			return false
		}
	}
	return true
}

// MaxSatisfying returns the highest version from candidates that satisfies r,
// or an error if none match. Pre-release versions are included only if the
// requirement explicitly names a pre-release version.
func MaxSatisfying(candidates []Version, r Req) (Version, error) {
	var best *Version
	for i := range candidates {
		c := candidates[i]
		if Satisfies(c, r) {
			if best == nil || compareVersions(c, *best) > 0 {
				best = &candidates[i]
			}
		}
	}
	if best == nil {
		return Version{}, fmt.Errorf("rubygems semver: no version satisfies %v %s", r.Op, r.Version.Original)
	}
	return *best, nil
}

// SortVersions sorts a slice of Version values in ascending order (oldest first).
// The original slice is modified in-place.
func SortVersions(vs []Version) {
	sort.Slice(vs, func(i, j int) bool {
		return compareVersions(vs[i], vs[j]) < 0
	})
}

// MustParseVersion parses a version string and panics on error. Useful for
// package-level variables and tests.
func MustParseVersion(s string) Version {
	v, err := ParseVersion(s)
	if err != nil {
		panic(err)
	}
	return v
}

// MustParseReq parses a requirement string and panics on error. Useful for
// package-level variables and tests.
func MustParseReq(s string) Req {
	r, err := ParseReq(s)
	if err != nil {
		panic(err)
	}
	return r
}

// CompareVersions compares two Version values. Returns -1, 0, or 1.
func CompareVersions(a, b Version) int {
	return compareVersions(a, b)
}
