// Package semver parses .NET four-part assembly versions and evaluates NuGet
// SemVer-2 version range expressions used in [dotnet-dependencies].
// The NuGet range grammar is documented at:
// https://learn.microsoft.com/en-us/nuget/concepts/package-versioning
package semver

import (
	"fmt"
	"strconv"
	"strings"
)

// Version represents a NuGet / .NET assembly version. .NET assembly versions
// are four-part (Major.Minor.Patch.Revision); NuGet normalises them to
// three-part SemVer-2 by dropping a trailing zero revision.
type Version struct {
	Major    int
	Minor    int
	Patch    int
	Revision int    // always 0 for normalised NuGet versions
	Pre      string // prerelease label, e.g. "alpha.1", "rc.2"
	Build    string // build metadata, e.g. "build.1"
}

// Parse parses a NuGet version string such as "13.0.3", "9.0.0-preview.1",
// or "1.0.0+build.42". Four-part assembly versions like "13.0.3.0" are also
// accepted; the trailing zero is preserved in Revision.
func Parse(s string) (Version, error) {
	// Strip build metadata (+...)
	v := Version{}
	if idx := strings.IndexByte(s, '+'); idx >= 0 {
		v.Build = s[idx+1:]
		s = s[:idx]
	}
	// Strip prerelease (-...)
	if idx := strings.IndexByte(s, '-'); idx >= 0 {
		v.Pre = s[idx+1:]
		s = s[:idx]
	}
	parts := strings.Split(s, ".")
	if len(parts) < 1 || len(parts) > 4 {
		return Version{}, fmt.Errorf("semver: invalid version %q", s)
	}
	nums := [4]int{}
	for i, p := range parts {
		n, err := strconv.Atoi(p)
		if err != nil || n < 0 {
			return Version{}, fmt.Errorf("semver: invalid part %q in version %q", p, s)
		}
		nums[i] = n
	}
	v.Major, v.Minor, v.Patch, v.Revision = nums[0], nums[1], nums[2], nums[3]
	return v, nil
}

// String returns the normalised NuGet string form (three-part for stable
// versions, three-part with prerelease for pre-releases).
func (v Version) String() string {
	s := fmt.Sprintf("%d.%d.%d", v.Major, v.Minor, v.Patch)
	if v.Revision != 0 {
		s += fmt.Sprintf(".%d", v.Revision)
	}
	if v.Pre != "" {
		s += "-" + v.Pre
	}
	if v.Build != "" {
		s += "+" + v.Build
	}
	return s
}

// Compare returns -1, 0, or 1. Prerelease versions sort before the
// corresponding stable version (SemVer-2 rule).
func (v Version) Compare(other Version) int {
	for _, pair := range [][2]int{
		{v.Major, other.Major},
		{v.Minor, other.Minor},
		{v.Patch, other.Patch},
		{v.Revision, other.Revision},
	} {
		if pair[0] < pair[1] {
			return -1
		}
		if pair[0] > pair[1] {
			return 1
		}
	}
	// Both have the same numeric part; compare prerelease.
	if v.Pre == "" && other.Pre != "" {
		return 1 // stable > prerelease
	}
	if v.Pre != "" && other.Pre == "" {
		return -1 // prerelease < stable
	}
	if v.Pre < other.Pre {
		return -1
	}
	if v.Pre > other.Pre {
		return 1
	}
	return 0
}

// Range represents a NuGet version range constraint as described in the
// NuGet versioning documentation. The bridge supports a subset of the full
// grammar: exact ([x.y.z]), caret (^x.y), tilde (~x.y), inequality (>=x.y),
// and bounded ranges ([x.y.z,x.y.z)).
type Range struct {
	raw string
}

// ParseRange parses a NuGet version range expression. Supported forms:
//   - "13.0.3"       bare version: >= 13.0.3 (NuGet minimum version)
//   - "[13.0.3]"     exact: == 13.0.3
//   - "^13.0"        caret: >= 13.0.0, < 14.0.0 (MEP-68 shorthand)
//   - "~3.1"         tilde: >= 3.1.0, < 3.2.0 (MEP-68 shorthand)
//   - ">=9.0.0"      inequality
//   - "[8.0.0,10.0)" half-open range
//   - "(,14.0)"      upper-bounded (any version < 14.0)
func ParseRange(s string) (Range, error) {
	s = strings.TrimSpace(s)
	if s == "" {
		return Range{}, fmt.Errorf("semver: empty version range")
	}
	return Range{raw: s}, nil
}

// Satisfies reports whether v satisfies the range constraint.
func (r Range) Satisfies(v Version) bool {
	s := r.raw
	switch {
	case strings.HasPrefix(s, "^"):
		lo, err := Parse(strings.TrimPrefix(s, "^"))
		if err != nil {
			return false
		}
		hi := Version{Major: lo.Major + 1}
		return v.Compare(lo) >= 0 && v.Compare(hi) < 0
	case strings.HasPrefix(s, "~"):
		lo, err := Parse(strings.TrimPrefix(s, "~"))
		if err != nil {
			return false
		}
		hi := Version{Major: lo.Major, Minor: lo.Minor + 1}
		return v.Compare(lo) >= 0 && v.Compare(hi) < 0
	case strings.HasPrefix(s, ">="):
		lo, err := Parse(strings.TrimPrefix(s, ">="))
		if err != nil {
			return false
		}
		return v.Compare(lo) >= 0
	case strings.HasPrefix(s, ">"):
		lo, err := Parse(strings.TrimPrefix(s, ">"))
		if err != nil {
			return false
		}
		return v.Compare(lo) > 0
	case strings.HasPrefix(s, "<="):
		hi, err := Parse(strings.TrimPrefix(s, "<="))
		if err != nil {
			return false
		}
		return v.Compare(hi) <= 0
	case strings.HasPrefix(s, "<"):
		hi, err := Parse(strings.TrimPrefix(s, "<"))
		if err != nil {
			return false
		}
		return v.Compare(hi) < 0
	case strings.HasPrefix(s, "[") && strings.HasSuffix(s, "]"):
		exact, err := Parse(s[1 : len(s)-1])
		if err != nil {
			return false
		}
		return v.Compare(exact) == 0
	case strings.HasPrefix(s, "[") || strings.HasPrefix(s, "("):
		return r.satisfiesBounded(v)
	default:
		// Bare version: NuGet minimum version semantics (>= bare).
		lo, err := Parse(s)
		if err != nil {
			return false
		}
		return v.Compare(lo) >= 0
	}
}

func (r Range) satisfiesBounded(v Version) bool {
	s := r.raw
	loInclusive := s[0] == '['
	hiInclusive := s[len(s)-1] == ']'
	inner := s[1 : len(s)-1]
	parts := strings.SplitN(inner, ",", 2)
	if len(parts) != 2 {
		return false
	}
	loStr := strings.TrimSpace(parts[0])
	hiStr := strings.TrimSpace(parts[1])
	if loStr != "" {
		lo, err := Parse(loStr)
		if err != nil {
			return false
		}
		cmp := v.Compare(lo)
		if loInclusive && cmp < 0 {
			return false
		}
		if !loInclusive && cmp <= 0 {
			return false
		}
	}
	if hiStr != "" {
		hi, err := Parse(hiStr)
		if err != nil {
			return false
		}
		cmp := v.Compare(hi)
		if hiInclusive && cmp > 0 {
			return false
		}
		if !hiInclusive && cmp >= 0 {
			return false
		}
	}
	return true
}

func (r Range) String() string { return r.raw }
