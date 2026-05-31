// Package semver implements node-semver-compatible range parsing and SemVer 2.0
// version ordering. Lands in MEP-72 Phase 0.
package semver

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

// Version is a parsed SemVer 2.0 version.
type Version struct {
	Major      int
	Minor      int
	Patch      int
	PreRelease string
}

// Range is a parsed node-semver range expression.
type Range struct {
	raw    string
	groups [][]comparator // OR groups of AND comparators
}

func (r Range) String() string { return r.raw }

type comparator struct {
	op    string // "=", "<", "<=", ">", ">="
	major int
	minor int // -1 = wildcard (only used in upper bounds)
	patch int // -1 = wildcard
	pre   string
}

var versionRe = regexp.MustCompile(`^v?(\d+)\.(\d+)\.(\d+)(?:-([0-9A-Za-z.-]+))?(?:\+[0-9A-Za-z.-]+)?$`)
var rangePartRe = regexp.MustCompile(`^([=<>^~]{0,2})(v?\d+|[xX*])(?:\.(v?\d+|[xX*])(?:\.(v?\d+|[xX*]))?)?(?:-([0-9A-Za-z.-]+))?$`)

// Parse parses a SemVer 2.0 version string.
func Parse(s string) (Version, error) {
	m := versionRe.FindStringSubmatch(strings.TrimSpace(s))
	if m == nil {
		return Version{}, fmt.Errorf("semver: invalid version %q", s)
	}
	major, _ := strconv.Atoi(m[1])
	minor, _ := strconv.Atoi(m[2])
	patch, _ := strconv.Atoi(m[3])
	return Version{Major: major, Minor: minor, Patch: patch, PreRelease: m[4]}, nil
}

func (v Version) String() string {
	s := fmt.Sprintf("%d.%d.%d", v.Major, v.Minor, v.Patch)
	if v.PreRelease != "" {
		s += "-" + v.PreRelease
	}
	return s
}

func (v Version) compare(other Version) int {
	if d := intCmp(v.Major, other.Major); d != 0 {
		return d
	}
	if d := intCmp(v.Minor, other.Minor); d != 0 {
		return d
	}
	if d := intCmp(v.Patch, other.Patch); d != 0 {
		return d
	}
	if v.PreRelease == "" && other.PreRelease != "" {
		return 1
	}
	if v.PreRelease != "" && other.PreRelease == "" {
		return -1
	}
	return strings.Compare(v.PreRelease, other.PreRelease)
}

func intCmp(a, b int) int {
	if a < b {
		return -1
	}
	if a > b {
		return 1
	}
	return 0
}

func isWild(s string) bool {
	return s == "" || s == "x" || s == "X" || s == "*"
}

func toNum(x string) int {
	x = strings.TrimPrefix(x, "v")
	n, _ := strconv.Atoi(x)
	return n
}

// ParseRange parses a node-semver range expression.
// Supports: =, <, <=, >, >=, ^, ~, x-ranges, hyphen ranges, || unions.
func ParseRange(s string) (Range, error) {
	s = strings.TrimSpace(s)
	if s == "" || s == "*" || s == "x" || s == "X" {
		return Range{raw: s, groups: [][]comparator{{{op: ">=", major: 0, minor: 0, patch: 0}}}}, nil
	}
	var groups [][]comparator
	for _, part := range strings.Split(s, "||") {
		comps, err := parseAndGroup(strings.TrimSpace(part))
		if err != nil {
			return Range{}, err
		}
		groups = append(groups, comps)
	}
	return Range{raw: s, groups: groups}, nil
}

func parseAndGroup(s string) ([]comparator, error) {
	if idx := strings.Index(s, " - "); idx != -1 {
		lo, err := parseSingleComp(">=" + strings.TrimSpace(s[:idx]))
		if err != nil {
			return nil, err
		}
		hi, err := parseSingleComp("<=" + strings.TrimSpace(s[idx+3:]))
		if err != nil {
			return nil, err
		}
		return []comparator{lo, hi}, nil
	}
	var comps []comparator
	for _, tok := range strings.Fields(s) {
		pair, err := parseToken(tok)
		if err != nil {
			return nil, err
		}
		comps = append(comps, pair...)
	}
	return comps, nil
}

// parseToken handles one token and returns 1-2 comparators (^ and ~ produce pairs).
func parseToken(tok string) ([]comparator, error) {
	tok = strings.TrimSpace(tok)
	if tok == "" || tok == "*" || tok == "x" || tok == "X" {
		return []comparator{{op: ">=", major: 0, minor: 0, patch: 0}}, nil
	}
	m := rangePartRe.FindStringSubmatch(tok)
	if m == nil {
		return nil, fmt.Errorf("semver: invalid token %q", tok)
	}
	op, maj, min, pat, pre := m[1], m[2], m[3], m[4], m[5]
	major := toNum(maj)

	switch op {
	case "^":
		return expandCaret(major, min, pat, pre)
	case "~":
		return expandTilde(major, min, pat, pre)
	case "", "=":
		if isWild(min) {
			return []comparator{
				{op: ">=", major: major, minor: 0, patch: 0},
				{op: "<", major: major + 1, minor: 0, patch: 0},
			}, nil
		}
		minor := toNum(min)
		if isWild(pat) {
			return []comparator{
				{op: ">=", major: major, minor: minor, patch: 0},
				{op: "<", major: major, minor: minor + 1, patch: 0},
			}, nil
		}
		patch := toNum(pat)
		return []comparator{{op: "=", major: major, minor: minor, patch: patch, pre: pre}}, nil
	default:
		c, err := parseSingleComp(tok)
		if err != nil {
			return nil, err
		}
		return []comparator{c}, nil
	}
}

func expandCaret(major int, min, pat, pre string) ([]comparator, error) {
	if isWild(min) {
		return []comparator{
			{op: ">=", major: major, minor: 0, patch: 0},
			{op: "<", major: major + 1, minor: 0, patch: 0},
		}, nil
	}
	minor := toNum(min)
	if isWild(pat) {
		return []comparator{
			{op: ">=", major: major, minor: minor, patch: 0},
			{op: "<", major: major + 1, minor: 0, patch: 0},
		}, nil
	}
	patch := toNum(pat)
	lo := comparator{op: ">=", major: major, minor: minor, patch: patch, pre: pre}
	if major != 0 {
		return []comparator{lo, {op: "<", major: major + 1, minor: 0, patch: 0}}, nil
	}
	if minor != 0 {
		return []comparator{lo, {op: "<", major: 0, minor: minor + 1, patch: 0}}, nil
	}
	return []comparator{lo, {op: "<", major: 0, minor: 0, patch: patch + 1}}, nil
}

func expandTilde(major int, min, pat, pre string) ([]comparator, error) {
	if isWild(min) {
		return []comparator{
			{op: ">=", major: major, minor: 0, patch: 0},
			{op: "<", major: major + 1, minor: 0, patch: 0},
		}, nil
	}
	minor := toNum(min)
	if isWild(pat) {
		return []comparator{
			{op: ">=", major: major, minor: minor, patch: 0},
			{op: "<", major: major, minor: minor + 1, patch: 0},
		}, nil
	}
	patch := toNum(pat)
	return []comparator{
		{op: ">=", major: major, minor: minor, patch: patch, pre: pre},
		{op: "<", major: major, minor: minor + 1, patch: 0},
	}, nil
}

func parseSingleComp(tok string) (comparator, error) {
	tok = strings.TrimSpace(tok)
	m := rangePartRe.FindStringSubmatch(tok)
	if m == nil {
		return comparator{}, fmt.Errorf("semver: invalid comparator %q", tok)
	}
	op, maj, min, pat, pre := m[1], m[2], m[3], m[4], m[5]
	if op == "" {
		op = "="
	}
	major := toNum(maj)
	minor := 0
	if !isWild(min) {
		minor = toNum(min)
	}
	patch := 0
	if !isWild(pat) {
		patch = toNum(pat)
	}
	return comparator{op: op, major: major, minor: minor, patch: patch, pre: pre}, nil
}

// Satisfies returns true if v satisfies the range.
func (r Range) Satisfies(v Version) bool {
	for _, group := range r.groups {
		if satisfiesGroup(v, group) {
			return true
		}
	}
	return false
}

func satisfiesGroup(v Version, comps []comparator) bool {
	for _, c := range comps {
		if !satisfiesOne(v, c) {
			return false
		}
	}
	return true
}

func satisfiesOne(v Version, c comparator) bool {
	cv := Version{Major: c.major, Minor: c.minor, Patch: c.patch, PreRelease: c.pre}
	if c.minor < 0 {
		cv.Minor = 0
	}
	if c.patch < 0 {
		cv.Patch = 0
	}
	d := v.compare(cv)
	switch c.op {
	case "=", "":
		return d == 0
	case "<":
		return d < 0
	case "<=":
		return d <= 0
	case ">":
		return d > 0
	case ">=":
		return d >= 0
	}
	return false
}

// MaxSatisfying finds the highest version in versions that satisfies the range.
func (r Range) MaxSatisfying(versions []string) (string, bool) {
	var best *Version
	var bestStr string
	for _, vs := range versions {
		v, err := Parse(vs)
		if err != nil {
			continue
		}
		if !r.Satisfies(v) {
			continue
		}
		if best == nil || v.compare(*best) > 0 {
			cp := v
			best = &cp
			bestStr = vs
		}
	}
	return bestStr, best != nil
}
