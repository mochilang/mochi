// Package hexsemver implements the Hex.pm version constraint language.
//
// Hex.pm uses a subset of SemVer 2.0.0 with its own constraint syntax:
//
//	"~> 2.1"    — patch-compatible: >= 2.1.0 and < 3.0.0
//	"~> 2.1.3"  — patch-compatible: >= 2.1.3 and < 2.2.0
//	">= 2.0.0"  — range lower bound (inclusive)
//	"> 1.9.9"   — range lower bound (exclusive)
//	"<= 3.0.0"  — range upper bound (inclusive)
//	"< 3.0.0"   — range upper bound (exclusive)
//	"== 2.1.0"  — exact match
//	"2.1.0"     — bare version string, treated as == 2.1.0
//
// Multiple constraints may be combined via AND by passing a slice to Satisfies.
// Hex.pm does not support OR constraints within a single dependency declaration.
//
// Pre-release versions (e.g. "2.1.0-rc.1") are only considered if the
// constraint itself references a pre-release version of the same major.minor.patch,
// matching the semantics documented in hex.pm/docs/publish.
package hexsemver

import (
	"fmt"
	"strconv"
	"strings"
)

// Version is a parsed Hex.pm/SemVer version.
type Version struct {
	Major, Minor, Patch int
	Pre                 string // empty for release versions
}

// ParseVersion parses a version string of the form "MAJOR.MINOR.PATCH[-pre]".
// The patch component is optional; it defaults to 0.
func ParseVersion(s string) (Version, error) {
	s = strings.TrimSpace(s)
	pre := ""
	if idx := strings.IndexByte(s, '-'); idx >= 0 {
		pre = s[idx+1:]
		s = s[:idx]
	}
	parts := strings.Split(s, ".")
	if len(parts) < 2 || len(parts) > 3 {
		return Version{}, fmt.Errorf("hexsemver: invalid version %q", s)
	}
	major, err := strconv.Atoi(parts[0])
	if err != nil {
		return Version{}, fmt.Errorf("hexsemver: invalid major in %q: %w", s, err)
	}
	minor, err := strconv.Atoi(parts[1])
	if err != nil {
		return Version{}, fmt.Errorf("hexsemver: invalid minor in %q: %w", s, err)
	}
	patch := 0
	if len(parts) == 3 {
		patch, err = strconv.Atoi(parts[2])
		if err != nil {
			return Version{}, fmt.Errorf("hexsemver: invalid patch in %q: %w", s, err)
		}
	}
	return Version{Major: major, Minor: minor, Patch: patch, Pre: pre}, nil
}

// String renders a Version back to canonical string form.
func (v Version) String() string {
	base := fmt.Sprintf("%d.%d.%d", v.Major, v.Minor, v.Patch)
	if v.Pre != "" {
		return base + "-" + v.Pre
	}
	return base
}

// Compare returns -1, 0, or +1 for v < other, v == other, or v > other.
// Pre-release versions sort before the release: 2.0.0-rc.1 < 2.0.0.
func (v Version) Compare(other Version) int {
	if v.Major != other.Major {
		return cmpInt(v.Major, other.Major)
	}
	if v.Minor != other.Minor {
		return cmpInt(v.Minor, other.Minor)
	}
	if v.Patch != other.Patch {
		return cmpInt(v.Patch, other.Patch)
	}
	// Pre-release handling: release > pre-release.
	switch {
	case v.Pre == "" && other.Pre == "":
		return 0
	case v.Pre != "" && other.Pre == "":
		return -1
	case v.Pre == "" && other.Pre != "":
		return 1
	default:
		return strings.Compare(v.Pre, other.Pre)
	}
}

func cmpInt(a, b int) int {
	if a < b {
		return -1
	}
	if a > b {
		return 1
	}
	return 0
}

// Constraint is a single parsed Hex.pm version constraint (e.g. "~> 2.1.3").
type Constraint struct {
	op      string  // "~>", ">=", ">", "<=", "<", "=="
	version Version // the version on the right-hand side
	hasPatch bool   // whether the version string had an explicit patch component
}

// ParseConstraint parses a single version constraint.
func ParseConstraint(s string) (Constraint, error) {
	s = strings.TrimSpace(s)
	var op, verStr string
	for _, candidate := range []string{"~>", ">=", "<=", ">", "<", "=="} {
		if strings.HasPrefix(s, candidate) {
			op = candidate
			verStr = strings.TrimSpace(s[len(candidate):])
			break
		}
	}
	if op == "" {
		// Bare version — treat as ==.
		op = "=="
		verStr = s
	}
	// Detect whether a patch component is present (before pre-release strip).
	rawVer := verStr
	if idx := strings.IndexByte(rawVer, '-'); idx >= 0 {
		rawVer = rawVer[:idx]
	}
	hasPatch := strings.Count(rawVer, ".") >= 2

	ver, err := ParseVersion(verStr)
	if err != nil {
		return Constraint{}, fmt.Errorf("hexsemver: parse constraint %q: %w", s, err)
	}
	return Constraint{op: op, version: ver, hasPatch: hasPatch}, nil
}

// Matches reports whether v satisfies c.
func (c Constraint) Matches(v Version) bool {
	// Pre-release filtering: Hex.pm only resolves pre-releases when the
	// constraint itself references a pre-release version or the operator is ==.
	// All other range operators treat pre-releases as invisible.
	if v.Pre != "" {
		switch c.op {
		case "==":
			// Exact match bypasses the filter.
		default:
			if c.version.Pre == "" {
				return false
			}
		}
	}
	cmp := v.Compare(c.version)
	switch c.op {
	case "==":
		return cmp == 0
	case ">":
		return cmp > 0
	case ">=":
		return cmp >= 0
	case "<":
		return cmp < 0
	case "<=":
		return cmp <= 0
	case "~>":
		// Pessimistic constraint:
		//   ~> 2.1   -> >= 2.1.0 and < 3.0.0  (minor-compatible, no patch given)
		//   ~> 2.1.3 -> >= 2.1.3 and < 2.2.0  (patch-compatible, patch given)
		if cmp < 0 {
			return false // v < constraint version
		}
		if !c.hasPatch {
			// Minor-compatible: same major, any minor >= constraint.minor.
			return v.Major == c.version.Major
		}
		// Patch-compatible: same major.minor, any patch >= constraint.patch.
		return v.Major == c.version.Major && v.Minor == c.version.Minor
	}
	return false
}

// String renders the constraint back to its canonical form.
func (c Constraint) String() string {
	return c.op + " " + c.version.String()
}

// Requirements is a list of constraints that must all be satisfied (logical AND).
type Requirements []Constraint

// ParseRequirements parses a comma-separated list of constraints.
// Example: ">= 2.0.0, < 3.0.0"
func ParseRequirements(s string) (Requirements, error) {
	parts := strings.Split(s, ",")
	var reqs Requirements
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		c, err := ParseConstraint(p)
		if err != nil {
			return nil, err
		}
		reqs = append(reqs, c)
	}
	return reqs, nil
}

// Satisfies reports whether v satisfies all constraints in r.
func (r Requirements) Satisfies(v Version) bool {
	for _, c := range r {
		if !c.Matches(v) {
			return false
		}
	}
	return true
}

// BestMatch returns the highest version from candidates that satisfies r.
// Returns the zero Version and false if no candidate satisfies r.
func (r Requirements) BestMatch(candidates []Version) (Version, bool) {
	var best Version
	found := false
	for _, v := range candidates {
		if r.Satisfies(v) {
			if !found || v.Compare(best) > 0 {
				best = v
				found = true
			}
		}
	}
	return best, found
}
