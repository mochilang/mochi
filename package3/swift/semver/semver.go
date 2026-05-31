// Package semver parses Swift Package Manager version expressions.
// It supports the SPM requirement forms: from, exact, upToNextMajor,
// upToNextMinor, and range (..<).
package semver

import (
	"fmt"
	"strconv"
	"strings"
)

// Version is a parsed semantic version number.
type Version struct {
	Major int
	Minor int
	Patch int
}

// Parse parses a version string such as "1.0.0" or "v2.3.1".
// A leading "v" is stripped before parsing.
func Parse(s string) (Version, error) {
	s = strings.TrimSpace(s)
	s = strings.TrimPrefix(s, "v")
	parts := strings.SplitN(s, ".", 3)
	if len(parts) != 3 {
		return Version{}, fmt.Errorf("semver: %q must have major.minor.patch", s)
	}
	v := Version{}
	var err error
	v.Major, err = strconv.Atoi(parts[0])
	if err != nil {
		return Version{}, fmt.Errorf("semver: invalid major in %q: %w", s, err)
	}
	v.Minor, err = strconv.Atoi(parts[1])
	if err != nil {
		return Version{}, fmt.Errorf("semver: invalid minor in %q: %w", s, err)
	}
	v.Patch, err = strconv.Atoi(parts[2])
	if err != nil {
		return Version{}, fmt.Errorf("semver: invalid patch in %q: %w", s, err)
	}
	return v, nil
}

// String returns "major.minor.patch".
func (v Version) String() string {
	return fmt.Sprintf("%d.%d.%d", v.Major, v.Minor, v.Patch)
}

// Less reports whether v is strictly less than other.
func (v Version) Less(other Version) bool {
	if v.Major != other.Major {
		return v.Major < other.Major
	}
	if v.Minor != other.Minor {
		return v.Minor < other.Minor
	}
	return v.Patch < other.Patch
}

// ReqKind identifies the kind of version requirement.
type ReqKind int

const (
	// ReqFrom means >= From and < next major (SPM "from:" form).
	ReqFrom ReqKind = iota
	// ReqExact means exactly equal to From.
	ReqExact
	// ReqUpToNextMajor means >= From and < next major.
	ReqUpToNextMajor
	// ReqUpToNextMinor means >= From and < next minor.
	ReqUpToNextMinor
	// ReqRange means >= From and < To.
	ReqRange
)

// Req is a parsed version requirement.
type Req struct {
	Kind ReqKind
	From Version
	To   Version // upper bound for ReqRange (exclusive)
}

// ParseReq parses an SPM version requirement string. Supported forms:
//
//	from: "1.0.0"
//	exact: "2.3.1"
//	upToNextMajor(from: "1.0.0")
//	upToNextMinor(from: "1.0.0")
//	"1.0.0"..<"2.0.0"
func ParseReq(s string) (Req, error) {
	s = strings.TrimSpace(s)

	// Range form: "X.Y.Z"..<"A.B.C"
	if idx := strings.Index(s, "..<"); idx >= 0 {
		fromStr := strings.Trim(s[:idx], `"`)
		toStr := strings.Trim(s[idx+3:], `"`)
		from, err := Parse(fromStr)
		if err != nil {
			return Req{}, fmt.Errorf("semver: range lower bound: %w", err)
		}
		to, err := Parse(toStr)
		if err != nil {
			return Req{}, fmt.Errorf("semver: range upper bound: %w", err)
		}
		return Req{Kind: ReqRange, From: from, To: to}, nil
	}

	// upToNextMajor(from: "X.Y.Z")
	if strings.HasPrefix(s, "upToNextMajor(from:") {
		inner := extractInnerVersion(s)
		v, err := Parse(inner)
		if err != nil {
			return Req{}, fmt.Errorf("semver: upToNextMajor: %w", err)
		}
		return Req{Kind: ReqUpToNextMajor, From: v}, nil
	}

	// upToNextMinor(from: "X.Y.Z")
	if strings.HasPrefix(s, "upToNextMinor(from:") {
		inner := extractInnerVersion(s)
		v, err := Parse(inner)
		if err != nil {
			return Req{}, fmt.Errorf("semver: upToNextMinor: %w", err)
		}
		return Req{Kind: ReqUpToNextMinor, From: v}, nil
	}

	// from: "X.Y.Z"
	if after, ok := strings.CutPrefix(s, "from:"); ok {
		inner := strings.Trim(strings.TrimSpace(after), `"`)
		v, err := Parse(inner)
		if err != nil {
			return Req{}, fmt.Errorf("semver: from: %w", err)
		}
		return Req{Kind: ReqFrom, From: v}, nil
	}

	// exact: "X.Y.Z"
	if after, ok := strings.CutPrefix(s, "exact:"); ok {
		inner := strings.Trim(strings.TrimSpace(after), `"`)
		v, err := Parse(inner)
		if err != nil {
			return Req{}, fmt.Errorf("semver: exact: %w", err)
		}
		return Req{Kind: ReqExact, From: v}, nil
	}

	return Req{}, fmt.Errorf("semver: unrecognised requirement form %q", s)
}

// extractInnerVersion extracts the quoted version string from
// upToNextMajor(from: "X.Y.Z") or upToNextMinor(from: "X.Y.Z").
func extractInnerVersion(s string) string {
	start := strings.Index(s, `"`)
	end := strings.LastIndex(s, `"`)
	if start < 0 || end <= start {
		return s
	}
	return s[start+1 : end]
}

// Satisfies reports whether v satisfies the requirement r.
func (r Req) Satisfies(v Version) bool {
	switch r.Kind {
	case ReqExact:
		return v == r.From
	case ReqFrom, ReqUpToNextMajor:
		// >= From and < next major
		nextMajor := Version{Major: r.From.Major + 1}
		return !v.Less(r.From) && v.Less(nextMajor)
	case ReqUpToNextMinor:
		// >= From and < next minor
		nextMinor := Version{Major: r.From.Major, Minor: r.From.Minor + 1}
		return !v.Less(r.From) && v.Less(nextMinor)
	case ReqRange:
		// >= From and < To
		return !v.Less(r.From) && v.Less(r.To)
	}
	return false
}
