package semver

import (
	"fmt"
	"strings"
)

// Req is a NuGet version range. NuGet supports interval notation as
// documented at https://docs.microsoft.com/en-us/nuget/concepts/package-versioning#version-ranges.
//
// Accepted shapes:
//
//	1.0.0          minimum version (>= 1.0.0, no upper bound)
//	[1.0.0]        exact version
//	[1.0.0, 2.0.0) min inclusive, max exclusive
//	[1.0.0, 2.0.0] both bounds inclusive
//	(1.0.0,)       exclusive minimum, no upper bound
//	(, 2.0.0)      exclusive maximum, no lower bound
//	*              any version
type Req struct {
	// Min is the minimum version bound. nil means no lower bound.
	Min *Version
	// Max is the maximum version bound. nil means no upper bound.
	Max *Version
	// MinInclusive is true when the lower bound uses '['.
	MinInclusive bool
	// MaxInclusive is true when the upper bound uses ']'.
	MaxInclusive bool

	raw string
}

// ParseReq parses a NuGet version range string.
func ParseReq(s string) (Req, error) {
	s = strings.TrimSpace(s)
	if s == "" {
		return Req{}, fmt.Errorf("semver req: empty string")
	}
	if s == "*" {
		return Req{raw: s}, nil
	}
	// Interval notation starts with '[' or '('.
	if s[0] == '[' || s[0] == '(' {
		return parseInterval(s)
	}
	// Bare version string: minimum version (>= s, no upper bound).
	v, err := Parse(s)
	if err != nil {
		return Req{}, fmt.Errorf("semver req: %w", err)
	}
	return Req{Min: &v, MinInclusive: true, raw: s}, nil
}

// MustParseReq is ParseReq but panics on error.
func MustParseReq(s string) Req {
	r, err := ParseReq(s)
	if err != nil {
		panic(err)
	}
	return r
}

// String renders the original range notation.
func (r Req) String() string {
	if r.raw != "" {
		return r.raw
	}
	return "*"
}

// Satisfies reports whether v satisfies the version range r.
func (r Req) Satisfies(v Version) bool {
	if r.Min != nil {
		cmp := v.Compare(*r.Min)
		if r.MinInclusive {
			if cmp < 0 {
				return false
			}
		} else {
			if cmp <= 0 {
				return false
			}
		}
	}
	if r.Max != nil {
		cmp := v.Compare(*r.Max)
		if r.MaxInclusive {
			if cmp > 0 {
				return false
			}
		} else {
			if cmp >= 0 {
				return false
			}
		}
	}
	return true
}

// parseInterval handles ranges that begin with '[' or '(' and end with ']' or ')'.
func parseInterval(s string) (Req, error) {
	if len(s) < 2 {
		return Req{}, fmt.Errorf("semver req: interval %q too short", s)
	}
	var minInc, maxInc bool
	switch s[0] {
	case '[':
		minInc = true
	case '(':
		minInc = false
	default:
		return Req{}, fmt.Errorf("semver req: interval %q must start with '[' or '('", s)
	}
	switch s[len(s)-1] {
	case ']':
		maxInc = true
	case ')':
		maxInc = false
	default:
		return Req{}, fmt.Errorf("semver req: interval %q must end with ']' or ')'", s)
	}
	inner := strings.TrimSpace(s[1 : len(s)-1])

	// Check for comma indicating two-bound range.
	loPart, hiPart, hasComma := strings.Cut(inner, ",")
	if !hasComma {
		// Single-value bracket form: [1.0.0] means exact.
		if s[0] != '[' || s[len(s)-1] != ']' {
			return Req{}, fmt.Errorf("semver req: single-value interval must use '[' and ']'")
		}
		v, err := Parse(strings.TrimSpace(inner))
		if err != nil {
			return Req{}, fmt.Errorf("semver req: %w", err)
		}
		return Req{Min: &v, Max: &v, MinInclusive: true, MaxInclusive: true, raw: s}, nil
	}

	minStr := strings.TrimSpace(loPart)
	maxStr := strings.TrimSpace(hiPart)

	var minVer, maxVer *Version
	if minStr != "" {
		v, err := Parse(minStr)
		if err != nil {
			return Req{}, fmt.Errorf("semver req: lower bound: %w", err)
		}
		minVer = &v
	}
	if maxStr != "" {
		v, err := Parse(maxStr)
		if err != nil {
			return Req{}, fmt.Errorf("semver req: upper bound: %w", err)
		}
		maxVer = &v
	}
	return Req{
		Min:          minVer,
		Max:          maxVer,
		MinInclusive: minInc,
		MaxInclusive: maxInc,
		raw:          s,
	}, nil
}

// MaxSatisfying returns the highest Version in versions that satisfies r.
// Returns false in the second return value when no version satisfies.
func MaxSatisfying(r Req, versions []Version) (Version, bool) {
	var best Version
	found := false
	for _, v := range versions {
		if !r.Satisfies(v) {
			continue
		}
		if !found || best.Compare(v) < 0 {
			best = v
			found = true
		}
	}
	return best, found
}
