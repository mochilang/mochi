// Package semver parses Maven version range expressions.
package semver

import (
	"fmt"
	"strconv"
	"strings"
)

// Version represents a parsed Maven version number.
type Version struct {
	Major int
	Minor int
	Patch int
	// Qualifier holds pre-release / build metadata (e.g. "SNAPSHOT", "RC1").
	Qualifier string
}

func (v Version) String() string {
	s := fmt.Sprintf("%d.%d.%d", v.Major, v.Minor, v.Patch)
	if v.Qualifier != "" {
		s += "-" + v.Qualifier
	}
	return s
}

// Parse parses a version string such as "1.7.3", "1.9.23-SNAPSHOT", or "2.0".
func Parse(s string) (Version, error) {
	s = strings.TrimSpace(s)
	qualifier := ""
	if idx := strings.IndexByte(s, '-'); idx >= 0 {
		qualifier = s[idx+1:]
		s = s[:idx]
	}
	parts := strings.SplitN(s, ".", 3)
	v := Version{Qualifier: qualifier}
	var err error
	if len(parts) > 0 {
		v.Major, err = strconv.Atoi(parts[0])
		if err != nil {
			return Version{}, fmt.Errorf("semver: invalid major in %q: %w", s, err)
		}
	}
	if len(parts) > 1 {
		v.Minor, err = strconv.Atoi(parts[1])
		if err != nil {
			return Version{}, fmt.Errorf("semver: invalid minor in %q: %w", s, err)
		}
	}
	if len(parts) > 2 {
		v.Patch, err = strconv.Atoi(parts[2])
		if err != nil {
			return Version{}, fmt.Errorf("semver: invalid patch in %q: %w", s, err)
		}
	}
	return v, nil
}

// Compare returns -1, 0, or 1 for a < b, a == b, a > b (qualifiers ignored).
func Compare(a, b Version) int {
	if a.Major != b.Major {
		if a.Major < b.Major {
			return -1
		}
		return 1
	}
	if a.Minor != b.Minor {
		if a.Minor < b.Minor {
			return -1
		}
		return 1
	}
	if a.Patch != b.Patch {
		if a.Patch < b.Patch {
			return -1
		}
		return 1
	}
	return 0
}

// RangeKind identifies the boundary type of a Maven version range endpoint.
type RangeKind int

const (
	RangeExact      RangeKind = iota // e.g. "1.7.3"
	RangeInclusive                   // [ or ]
	RangeExclusive                   // ( or )
	RangeUnbounded                   // open end
	RangeLatest                      // "LATEST"
	RangeRelease                     // "RELEASE"
	RangePrefix                      // "1.7.+"
)

// Range is a parsed Maven version constraint.
type Range struct {
	Kind     RangeKind
	Lo       Version
	Hi       Version
	LoIncl   bool
	HiIncl   bool
	LoOpen   bool // true = no lower bound
	HiOpen   bool // true = no upper bound
	Prefix   Version // for RangePrefix
}

// ParseRange parses a Maven version range expression.
// Supported forms:
//
//	"1.7.3"      - exact
//	"[1.0,2.0)"  - >=1.0 <2.0
//	"[1.0,]"     - >=1.0
//	"(,2.0]"     - <=2.0
//	"1.7.+"      - >=1.7.0 <1.8.0
//	"LATEST"     - latest available
//	"RELEASE"    - latest release
func ParseRange(s string) (Range, error) {
	s = strings.TrimSpace(s)
	switch s {
	case "LATEST":
		return Range{Kind: RangeLatest}, nil
	case "RELEASE":
		return Range{Kind: RangeRelease}, nil
	}
	if base, ok := strings.CutSuffix(s, ".+"); ok {
		v, err := Parse(base)
		if err != nil {
			return Range{}, fmt.Errorf("semver: invalid prefix range %q: %w", s, err)
		}
		return Range{Kind: RangePrefix, Prefix: v}, nil
	}
	if len(s) > 0 && (s[0] == '[' || s[0] == '(') {
		return parseInterval(s)
	}
	v, err := Parse(s)
	if err != nil {
		return Range{}, err
	}
	return Range{Kind: RangeExact, Lo: v, Hi: v, LoIncl: true, HiIncl: true}, nil
}

func parseInterval(s string) (Range, error) {
	if len(s) < 2 {
		return Range{}, fmt.Errorf("semver: empty interval %q", s)
	}
	r := Range{Kind: RangeInclusive}
	r.LoIncl = s[0] == '['
	r.HiIncl = s[len(s)-1] == ']'
	inner := s[1 : len(s)-1]
	parts := strings.SplitN(inner, ",", 2)
	if len(parts) != 2 {
		return Range{}, fmt.Errorf("semver: interval %q missing comma", s)
	}
	lo, hi := strings.TrimSpace(parts[0]), strings.TrimSpace(parts[1])
	if lo == "" {
		r.LoOpen = true
	} else {
		v, err := Parse(lo)
		if err != nil {
			return Range{}, fmt.Errorf("semver: bad lower bound in %q: %w", s, err)
		}
		r.Lo = v
	}
	if hi == "" {
		r.HiOpen = true
	} else {
		v, err := Parse(hi)
		if err != nil {
			return Range{}, fmt.Errorf("semver: bad upper bound in %q: %w", s, err)
		}
		r.Hi = v
	}
	return r, nil
}

// Matches reports whether v satisfies the range constraint.
func (r Range) Matches(v Version) bool {
	switch r.Kind {
	case RangeLatest, RangeRelease:
		return true
	case RangeExact:
		return Compare(v, r.Lo) == 0
	case RangePrefix:
		return v.Major == r.Prefix.Major && v.Minor == r.Prefix.Minor && v.Patch >= r.Prefix.Patch
	}
	// interval
	if !r.LoOpen {
		cmp := Compare(v, r.Lo)
		if r.LoIncl && cmp < 0 {
			return false
		}
		if !r.LoIncl && cmp <= 0 {
			return false
		}
	}
	if !r.HiOpen {
		cmp := Compare(v, r.Hi)
		if r.HiIncl && cmp > 0 {
			return false
		}
		if !r.HiIncl && cmp >= 0 {
			return false
		}
	}
	return true
}
