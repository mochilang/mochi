package semver

import (
	"fmt"
	"strings"
)

// Req is a cargo-flavoured version requirement. A Req is satisfied by a
// Version when every Comparator in every And-group is satisfied, and at
// least one Or-group matches. (The comma operator is "AND"; multiple Reqs
// passed to MaxSatisfying are "OR".)
//
// Cargo accepts these requirement shapes (with whitespace ignored):
//
//	1.2.3       => caret: >=1.2.3, <2.0.0  (with the 0.x.y carve-out)
//	^1.2.3      => same as bare 1.2.3
//	~1.2.3      => >=1.2.3, <1.3.0
//	~1.2        => >=1.2.0, <1.3.0
//	~1          => >=1.0.0, <2.0.0
//	=1.2.3      => exactly 1.2.3
//	>=1.2.3     => any version >= 1.2.3
//	>1.2.3      => strictly greater
//	<=1.2.3, <1.2.3
//	*           => any version
//	1.2.*       => >=1.2.0, <1.3.0
//	1.*         => >=1.0.0, <2.0.0
//	1.2.3, <2.0 => intersection
type Req struct {
	comparators []comparator
	raw         string
}

type comparator struct {
	op  comparatorOp
	ver Version
}

type comparatorOp int

const (
	opExact comparatorOp = iota
	opGT
	opGE
	opLT
	opLE
)

// ParseReq parses a single cargo requirement string.
func ParseReq(s string) (Req, error) {
	s = strings.TrimSpace(s)
	if s == "" {
		return Req{}, fmt.Errorf("semver req: empty string")
	}
	if s == "*" {
		return Req{raw: s, comparators: nil}, nil
	}
	parts := strings.Split(s, ",")
	var cmps []comparator
	for _, p := range parts {
		p = strings.TrimSpace(p)
		expanded, err := parseSinglePart(p)
		if err != nil {
			return Req{}, fmt.Errorf("semver req %q: %w", s, err)
		}
		cmps = append(cmps, expanded...)
	}
	return Req{raw: s, comparators: cmps}, nil
}

// MustParseReq is ParseReq with a panic on error.
func MustParseReq(s string) Req {
	r, err := ParseReq(s)
	if err != nil {
		panic(err)
	}
	return r
}

// String renders the original raw requirement.
func (r Req) String() string {
	if r.raw == "" {
		return "*"
	}
	return r.raw
}

// Matches reports whether v satisfies r. Pre-release versions match only
// when the requirement explicitly references the same Major.Minor.Patch as
// the pre-release version (cargo's pre-release carve-out).
func (r Req) Matches(v Version) bool {
	if len(r.comparators) == 0 {
		// "*" matches any non-prerelease.
		return !v.IsPrerelease()
	}
	for _, c := range r.comparators {
		if !c.matches(v) {
			return false
		}
	}
	if v.IsPrerelease() {
		// At least one comparator must be on the same (major, minor, patch)
		// for a pre-release version to satisfy a requirement. This matches
		// cargo's "pre-releases are opt-in" rule.
		for _, c := range r.comparators {
			if c.ver.Major == v.Major && c.ver.Minor == v.Minor && c.ver.Patch == v.Patch && c.ver.IsPrerelease() {
				return true
			}
		}
		return false
	}
	return true
}

func (c comparator) matches(v Version) bool {
	cmp := v.Compare(c.ver)
	switch c.op {
	case opExact:
		return cmp == 0
	case opGT:
		return cmp > 0
	case opGE:
		return cmp >= 0
	case opLT:
		return cmp < 0
	case opLE:
		return cmp <= 0
	}
	return false
}

// parseSinglePart parses one comma-separated chunk and returns the
// comparators it expands into. Caret and tilde and wildcard each expand to
// two comparators; exact and inequality each produce one.
func parseSinglePart(s string) ([]comparator, error) {
	switch {
	case strings.HasPrefix(s, "^"):
		return expandCaret(strings.TrimSpace(s[1:]))
	case strings.HasPrefix(s, "~"):
		return expandTilde(strings.TrimSpace(s[1:]))
	case strings.HasPrefix(s, ">="):
		v, err := ParseRelaxed(strings.TrimSpace(s[2:]))
		if err != nil {
			return nil, err
		}
		return []comparator{{op: opGE, ver: v}}, nil
	case strings.HasPrefix(s, "<="):
		v, err := ParseRelaxed(strings.TrimSpace(s[2:]))
		if err != nil {
			return nil, err
		}
		return []comparator{{op: opLE, ver: v}}, nil
	case strings.HasPrefix(s, ">"):
		v, err := ParseRelaxed(strings.TrimSpace(s[1:]))
		if err != nil {
			return nil, err
		}
		return []comparator{{op: opGT, ver: v}}, nil
	case strings.HasPrefix(s, "<"):
		v, err := ParseRelaxed(strings.TrimSpace(s[1:]))
		if err != nil {
			return nil, err
		}
		return []comparator{{op: opLT, ver: v}}, nil
	case strings.HasPrefix(s, "="):
		v, err := ParseRelaxed(strings.TrimSpace(s[1:]))
		if err != nil {
			return nil, err
		}
		return []comparator{{op: opExact, ver: v}}, nil
	default:
		// Bare or wildcard.
		if strings.Contains(s, "*") {
			return expandWildcard(s)
		}
		return expandCaret(s)
	}
}

// expandCaret implements cargo's caret semantics with the leading-zero
// carve-out.
//
//	^1.2.3  => >=1.2.3, <2.0.0
//	^1.2    => >=1.2.0, <2.0.0
//	^1      => >=1.0.0, <2.0.0
//	^0.2.3  => >=0.2.3, <0.3.0
//	^0.2    => >=0.2.0, <0.3.0
//	^0.0.3  => >=0.0.3, <0.0.4
//	^0.0    => >=0.0.0, <0.1.0
//	^0      => >=0.0.0, <1.0.0
func expandCaret(s string) ([]comparator, error) {
	majorPart, minorPart, patchPart, pre, build, depth, err := splitComponents(s)
	if err != nil {
		return nil, err
	}
	low := Version{Major: majorPart, Minor: minorPart, Patch: patchPart, Pre: pre, Build: build}
	var high Version
	switch {
	case majorPart > 0 || depth == 1:
		// ^1.2.3 or ^1.2 or ^1 or ^2 => upper at next major
		high = Version{Major: majorPart + 1}
	case minorPart > 0 || depth == 2:
		// ^0.x.y or ^0.x => upper at next minor
		high = Version{Major: 0, Minor: minorPart + 1}
	default:
		// ^0.0.z => upper at next patch
		high = Version{Major: 0, Minor: 0, Patch: patchPart + 1}
	}
	return []comparator{{op: opGE, ver: low}, {op: opLT, ver: high}}, nil
}

// expandTilde implements cargo's tilde semantics.
//
//	~1.2.3  => >=1.2.3, <1.3.0
//	~1.2    => >=1.2.0, <1.3.0
//	~1      => >=1.0.0, <2.0.0
func expandTilde(s string) ([]comparator, error) {
	majorPart, minorPart, patchPart, pre, build, depth, err := splitComponents(s)
	if err != nil {
		return nil, err
	}
	low := Version{Major: majorPart, Minor: minorPart, Patch: patchPart, Pre: pre, Build: build}
	var high Version
	switch depth {
	case 1:
		high = Version{Major: majorPart + 1}
	default: // 2 or 3
		high = Version{Major: majorPart, Minor: minorPart + 1}
	}
	return []comparator{{op: opGE, ver: low}, {op: opLT, ver: high}}, nil
}

// expandWildcard implements cargo wildcard semantics.
//
//	*      => any
//	1.*    => >=1.0.0, <2.0.0
//	1.2.*  => >=1.2.0, <1.3.0
func expandWildcard(s string) ([]comparator, error) {
	if s == "*" {
		return nil, nil
	}
	parts := strings.Split(s, ".")
	if len(parts) < 2 || len(parts) > 3 {
		return nil, fmt.Errorf("wildcard %q: want N.* or N.M.*", s)
	}
	last := parts[len(parts)-1]
	if last != "*" {
		return nil, fmt.Errorf("wildcard %q: last component must be *", s)
	}
	majorPart, err := parseNumeric(parts[0])
	if err != nil {
		return nil, err
	}
	var minorPart uint64
	if len(parts) == 3 {
		minorPart, err = parseNumeric(parts[1])
		if err != nil {
			return nil, err
		}
	}
	low := Version{Major: majorPart, Minor: minorPart}
	var high Version
	switch len(parts) {
	case 2:
		high = Version{Major: majorPart + 1}
	default:
		high = Version{Major: majorPart, Minor: minorPart + 1}
	}
	return []comparator{{op: opGE, ver: low}, {op: opLT, ver: high}}, nil
}

// splitComponents parses a relaxed N or N.M or N.M.P with optional
// pre-release and build, returning each component and the depth (how many
// dotted parts were supplied).
func splitComponents(s string) (uint64, uint64, uint64, string, string, int, error) {
	rest := s
	var pre, build string
	if i := strings.Index(rest, "+"); i >= 0 {
		build = rest[i+1:]
		rest = rest[:i]
	}
	if i := strings.Index(rest, "-"); i >= 0 {
		pre = rest[i+1:]
		rest = rest[:i]
	}
	parts := strings.Split(rest, ".")
	if len(parts) < 1 || len(parts) > 3 {
		return 0, 0, 0, "", "", 0, fmt.Errorf("want 1-3 components in %q, got %d", s, len(parts))
	}
	mj, err := parseNumeric(parts[0])
	if err != nil {
		return 0, 0, 0, "", "", 0, err
	}
	var mn, pt uint64
	if len(parts) >= 2 {
		mn, err = parseNumeric(parts[1])
		if err != nil {
			return 0, 0, 0, "", "", 0, err
		}
	}
	if len(parts) >= 3 {
		pt, err = parseNumeric(parts[2])
		if err != nil {
			return 0, 0, 0, "", "", 0, err
		}
	}
	if pre != "" {
		if err := validateIdentifier(pre); err != nil {
			return 0, 0, 0, "", "", 0, err
		}
	}
	if build != "" {
		if err := validateIdentifier(build); err != nil {
			return 0, 0, 0, "", "", 0, err
		}
	}
	return mj, mn, pt, pre, build, len(parts), nil
}

// MaxSatisfying returns the highest Version in versions that matches r,
// preferring non-prerelease over prerelease. Returns false in the second
// return when no version matches.
func MaxSatisfying(r Req, versions []Version) (Version, bool) {
	var best Version
	found := false
	for _, v := range versions {
		if !r.Matches(v) {
			continue
		}
		if !found || best.Compare(v) < 0 {
			best = v
			found = true
		}
	}
	return best, found
}
