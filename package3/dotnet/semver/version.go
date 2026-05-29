// Package semver implements NuGet's 4-part version scheme (Major.Minor.Patch.Revision)
// with optional pre-release suffix. NuGet version ordering follows SemVer 2.0.0
// with the extension that a Revision component may appear between Patch and the
// pre-release separator.
//
// Reference: https://docs.microsoft.com/en-us/nuget/concepts/package-versioning
package semver

import (
	"fmt"
	"strconv"
	"strings"
)

// Version is a parsed NuGet version. Pre-release is preserved verbatim.
// Revision is zero for versions that omit the fourth component.
type Version struct {
	Major, Minor, Patch, Revision int
	// PreRelease is the pre-release label without the leading '-', e.g.
	// "alpha.1". Empty for stable releases.
	PreRelease string
}

// Parse converts a NuGet version string into a Version. Invalid inputs
// return an error.
//
// Accepted shapes:
//
//	1.2.3
//	1.2.3.4
//	1.2.3-beta.1
//	1.2.3.4-rc.2
func Parse(s string) (Version, error) {
	if s == "" {
		return Version{}, fmt.Errorf("semver: empty version string")
	}
	rest := s
	var pre string
	if i := strings.Index(rest, "-"); i >= 0 {
		pre = rest[i+1:]
		rest = rest[:i]
	}
	parts := strings.Split(rest, ".")
	if len(parts) < 3 || len(parts) > 4 {
		return Version{}, fmt.Errorf("semver: %q has %d components; want 3 or 4", s, len(parts))
	}
	mj, err := parseComponent(parts[0])
	if err != nil {
		return Version{}, fmt.Errorf("semver: %q major: %w", s, err)
	}
	mn, err := parseComponent(parts[1])
	if err != nil {
		return Version{}, fmt.Errorf("semver: %q minor: %w", s, err)
	}
	pt, err := parseComponent(parts[2])
	if err != nil {
		return Version{}, fmt.Errorf("semver: %q patch: %w", s, err)
	}
	var rev int
	if len(parts) == 4 {
		rev, err = parseComponent(parts[3])
		if err != nil {
			return Version{}, fmt.Errorf("semver: %q revision: %w", s, err)
		}
	}
	if pre != "" {
		if err := validatePreRelease(pre); err != nil {
			return Version{}, fmt.Errorf("semver: %q pre-release: %w", s, err)
		}
	}
	return Version{Major: mj, Minor: mn, Patch: pt, Revision: rev, PreRelease: pre}, nil
}

// MustParse is Parse but panics on error. Useful in tests and package-level
// constants.
func MustParse(s string) Version {
	v, err := Parse(s)
	if err != nil {
		panic(err)
	}
	return v
}

// String renders the Version in NuGet canonical form.
func (v Version) String() string {
	var b strings.Builder
	fmt.Fprintf(&b, "%d.%d.%d", v.Major, v.Minor, v.Patch)
	if v.Revision != 0 {
		fmt.Fprintf(&b, ".%d", v.Revision)
	}
	if v.PreRelease != "" {
		b.WriteByte('-')
		b.WriteString(v.PreRelease)
	}
	return b.String()
}

// Compare returns -1 if v < other, 0 if equal, +1 if v > other.
// Pre-release versions sort lower than their stable counterpart per NuGet
// semantics.
func (v Version) Compare(other Version) int {
	if c := cmpInt(v.Major, other.Major); c != 0 {
		return c
	}
	if c := cmpInt(v.Minor, other.Minor); c != 0 {
		return c
	}
	if c := cmpInt(v.Patch, other.Patch); c != 0 {
		return c
	}
	if c := cmpInt(v.Revision, other.Revision); c != 0 {
		return c
	}
	return comparePreRelease(v.PreRelease, other.PreRelease)
}

// IsPreRelease reports whether the version has a pre-release label.
func (v Version) IsPreRelease() bool { return v.PreRelease != "" }

// Equal reports whether v and other compare equal.
func (v Version) Equal(other Version) bool { return v.Compare(other) == 0 }

// cmpInt compares two ints in standard signum fashion.
func cmpInt(a, b int) int {
	switch {
	case a < b:
		return -1
	case a > b:
		return 1
	default:
		return 0
	}
}

// comparePreRelease sorts pre-release labels. A stable release (empty pre)
// sorts higher than any pre-release, mirroring NuGet ordering.
func comparePreRelease(a, b string) int {
	switch {
	case a == "" && b == "":
		return 0
	case a == "":
		return 1 // stable > pre-release
	case b == "":
		return -1
	}
	// Both non-empty: compare dot-delimited identifiers.
	aIds := strings.Split(a, ".")
	bIds := strings.Split(b, ".")
	for i := 0; i < len(aIds) && i < len(bIds); i++ {
		aN, aIsNum := tryParseInt(aIds[i])
		bN, bIsNum := tryParseInt(bIds[i])
		switch {
		case aIsNum && bIsNum:
			if c := cmpInt(aN, bN); c != 0 {
				return c
			}
		case aIsNum:
			return -1 // numeric < alphanumeric
		case bIsNum:
			return 1
		default:
			if c := strings.Compare(strings.ToLower(aIds[i]), strings.ToLower(bIds[i])); c != 0 {
				return c
			}
		}
	}
	return cmpInt(len(aIds), len(bIds))
}

// parseComponent parses a single non-negative integer version component.
func parseComponent(s string) (int, error) {
	if s == "" {
		return 0, fmt.Errorf("empty component")
	}
	n, err := strconv.Atoi(s)
	if err != nil {
		return 0, fmt.Errorf("non-numeric component %q", s)
	}
	if n < 0 {
		return 0, fmt.Errorf("negative component %q", s)
	}
	return n, nil
}

// tryParseInt attempts to parse s as a non-negative integer.
func tryParseInt(s string) (int, bool) {
	n, err := strconv.Atoi(s)
	if err != nil || n < 0 {
		return 0, false
	}
	return n, true
}

// validatePreRelease ensures the pre-release label is non-empty and contains
// only alphanumerics, dots, and hyphens.
func validatePreRelease(s string) error {
	if s == "" {
		return fmt.Errorf("empty pre-release label")
	}
	for _, r := range s {
		ok := (r >= 'A' && r <= 'Z') || (r >= 'a' && r <= 'z') ||
			(r >= '0' && r <= '9') || r == '-' || r == '.'
		if !ok {
			return fmt.Errorf("invalid character %q in pre-release label", r)
		}
	}
	return nil
}
