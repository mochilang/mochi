// Package semver implements the cargo flavour of Semantic Versioning 2.0.0.
//
// Cargo's semver differs from semver.org and from npm-semver in a few
// load-bearing ways:
//   - The caret operator (^) treats the leading 0 specially: ^0.x.y allows
//     0.x.* but not 0.(x+1).0, and ^0.0.z allows only 0.0.z exactly.
//   - The tilde operator (~) is "patch within a minor".
//   - The wildcard operator (*) is "any".
//   - Bare version strings ("1.2.3") behave as "^1.2.3" (cargo's default).
//   - Pre-release versions (1.0.0-alpha.1) have ordering rules from semver.org
//     but cargo only matches them when the requirement explicitly mentions a
//     pre-release.
//
// The reference for cargo's semver is the cargo book at
// https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html
// plus the semver crate at https://docs.rs/semver/.
package semver

import (
	"fmt"
	"strconv"
	"strings"
)

// Version is a parsed semver 2.0.0 version. Pre-release and build metadata
// are preserved verbatim from the input.
type Version struct {
	Major uint64
	Minor uint64
	Patch uint64
	// Pre is the pre-release identifier (without the leading '-'), e.g.
	// "alpha.1". Empty for non-prerelease versions.
	Pre string
	// Build is the build-metadata string (without the leading '+'), e.g.
	// "001". Build metadata does not affect ordering per semver.org §10.
	Build string
}

// Parse converts a string into a Version. Invalid inputs return an error.
//
// Accepted shapes:
//
//	1.2.3
//	1.2.3-alpha.1
//	1.2.3+build.5
//	1.2.3-alpha.1+build.5
//
// The bare-form short version (1.0 or 1) is not accepted by Parse; use
// ParseRelaxed for shapes that fill missing components with zero.
func Parse(s string) (Version, error) {
	if s == "" {
		return Version{}, fmt.Errorf("semver: empty version string")
	}
	rest := s
	var pre, build string
	var hasPre, hasBuild bool
	if i := strings.Index(rest, "+"); i >= 0 {
		hasBuild = true
		build = rest[i+1:]
		rest = rest[:i]
	}
	if i := strings.Index(rest, "-"); i >= 0 {
		hasPre = true
		pre = rest[i+1:]
		rest = rest[:i]
	}
	parts := strings.Split(rest, ".")
	if len(parts) != 3 {
		return Version{}, fmt.Errorf("semver: %q has %d dot-separated components; want 3", s, len(parts))
	}
	mj, err := parseNumeric(parts[0])
	if err != nil {
		return Version{}, fmt.Errorf("semver: %q major: %w", s, err)
	}
	mn, err := parseNumeric(parts[1])
	if err != nil {
		return Version{}, fmt.Errorf("semver: %q minor: %w", s, err)
	}
	pt, err := parseNumeric(parts[2])
	if err != nil {
		return Version{}, fmt.Errorf("semver: %q patch: %w", s, err)
	}
	if hasPre {
		if err := validateIdentifier(pre); err != nil {
			return Version{}, fmt.Errorf("semver: %q pre-release: %w", s, err)
		}
	}
	if hasBuild {
		if err := validateIdentifier(build); err != nil {
			return Version{}, fmt.Errorf("semver: %q build: %w", s, err)
		}
	}
	return Version{Major: mj, Minor: mn, Patch: pt, Pre: pre, Build: build}, nil
}

// ParseRelaxed accepts the same shapes as Parse plus the cargo short forms:
//
//	1       -> 1.0.0
//	1.2     -> 1.2.0
//
// Used when reading a [dependencies] table value that omits trailing zeros.
func ParseRelaxed(s string) (Version, error) {
	if s == "" {
		return Version{}, fmt.Errorf("semver: empty version string")
	}
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
		return Version{}, fmt.Errorf("semver: %q has %d components; want 1, 2, or 3", s, len(parts))
	}
	pad := make([]string, 3)
	for i := range pad {
		if i < len(parts) {
			pad[i] = parts[i]
		} else {
			pad[i] = "0"
		}
	}
	return Parse(strings.Join(pad, ".") + suffix(pre, build))
}

func suffix(pre, build string) string {
	out := ""
	if pre != "" {
		out += "-" + pre
	}
	if build != "" {
		out += "+" + build
	}
	return out
}

// MustParse is Parse but panics on error. Useful for tests and for in-source
// constants.
func MustParse(s string) Version {
	v, err := Parse(s)
	if err != nil {
		panic(err)
	}
	return v
}

// String renders the Version in canonical form.
func (v Version) String() string {
	out := fmt.Sprintf("%d.%d.%d", v.Major, v.Minor, v.Patch)
	if v.Pre != "" {
		out += "-" + v.Pre
	}
	if v.Build != "" {
		out += "+" + v.Build
	}
	return out
}

// IsPrerelease reports whether the version has a pre-release identifier.
func (v Version) IsPrerelease() bool { return v.Pre != "" }

// Compare returns -1 if v < other, 0 if equal, +1 if v > other. Build
// metadata does not affect ordering (semver.org §10).
func (v Version) Compare(other Version) int {
	if c := cmpUint(v.Major, other.Major); c != 0 {
		return c
	}
	if c := cmpUint(v.Minor, other.Minor); c != 0 {
		return c
	}
	if c := cmpUint(v.Patch, other.Patch); c != 0 {
		return c
	}
	return comparePre(v.Pre, other.Pre)
}

// Equal reports whether v and other have the same Major.Minor.Patch and Pre.
// Build metadata is intentionally ignored.
func (v Version) Equal(other Version) bool { return v.Compare(other) == 0 }

// cmpUint compares two uint64.
func cmpUint(a, b uint64) int {
	switch {
	case a < b:
		return -1
	case a > b:
		return 1
	default:
		return 0
	}
}

// comparePre implements semver.org §11 pre-release ordering. A version
// without a pre-release sorts higher than one with a pre-release.
func comparePre(a, b string) int {
	switch {
	case a == "" && b == "":
		return 0
	case a == "":
		return 1
	case b == "":
		return -1
	}
	aIds := strings.Split(a, ".")
	bIds := strings.Split(b, ".")
	for i := 0; i < len(aIds) && i < len(bIds); i++ {
		aN, aIsNum := tryParseNumeric(aIds[i])
		bN, bIsNum := tryParseNumeric(bIds[i])
		switch {
		case aIsNum && bIsNum:
			if c := cmpUint(aN, bN); c != 0 {
				return c
			}
		case aIsNum:
			return -1
		case bIsNum:
			return 1
		default:
			if c := strings.Compare(aIds[i], bIds[i]); c != 0 {
				return c
			}
		}
	}
	switch {
	case len(aIds) < len(bIds):
		return -1
	case len(aIds) > len(bIds):
		return 1
	default:
		return 0
	}
}

func parseNumeric(s string) (uint64, error) {
	if s == "" {
		return 0, fmt.Errorf("empty numeric component")
	}
	if len(s) > 1 && s[0] == '0' {
		return 0, fmt.Errorf("leading zero in %q", s)
	}
	return strconv.ParseUint(s, 10, 64)
}

func tryParseNumeric(s string) (uint64, bool) {
	v, err := strconv.ParseUint(s, 10, 64)
	if err != nil {
		return 0, false
	}
	if len(s) > 1 && s[0] == '0' {
		return 0, false
	}
	return v, true
}

func validateIdentifier(s string) error {
	if s == "" {
		return fmt.Errorf("empty identifier")
	}
	for _, part := range strings.Split(s, ".") {
		if part == "" {
			return fmt.Errorf("empty dot-separated part")
		}
		for _, r := range part {
			ok := (r >= 'A' && r <= 'Z') || (r >= 'a' && r <= 'z') ||
				(r >= '0' && r <= '9') || r == '-'
			if !ok {
				return fmt.Errorf("invalid character %q in %q", r, part)
			}
		}
	}
	return nil
}
