package freethread

import (
	"fmt"
	"strconv"
	"strings"
)

// ABITag is a parsed CPython ABI tag with the version + free-threaded
// bit pulled out. The wheel resolver branches on FreeThreaded so a
// 3.13t install does not accept a `cp313`-tagged extension that
// expects the GIL to serialise its refcounts.
type ABITag struct {
	Raw          string
	Interpreter  string // "cp" today; "pp" / "pypy" arrive in sub-phase 17.x
	Major        int
	Minor        int
	FreeThreaded bool
}

// ParseABITag accepts `cp3XY` or `cp3XYt`. Anything else returns
// an error: the install orchestrator surfaces it through a
// SkipReason so an unparseable tag never silently falls through
// the resolver.
func ParseABITag(raw string) (ABITag, error) {
	t := ABITag{Raw: raw}
	if !strings.HasPrefix(raw, "cp") {
		return t, fmt.Errorf("freethread: ABI tag %q: only cp<ver>[t] is supported in phase 17", raw)
	}
	t.Interpreter = "cp"
	rest := strings.TrimPrefix(raw, "cp")
	if rest == "" {
		return t, fmt.Errorf("freethread: ABI tag %q: missing version", raw)
	}
	if strings.HasSuffix(rest, "t") {
		t.FreeThreaded = true
		rest = strings.TrimSuffix(rest, "t")
	}
	if len(rest) < 2 {
		return t, fmt.Errorf("freethread: ABI tag %q: version must be at least 2 digits", raw)
	}
	major, err := strconv.Atoi(rest[:1])
	if err != nil || major <= 0 {
		return t, fmt.Errorf("freethread: ABI tag %q: bad major %q", raw, rest[:1])
	}
	minor, err := strconv.Atoi(rest[1:])
	if err != nil || minor < 0 {
		return t, fmt.Errorf("freethread: ABI tag %q: bad minor %q", raw, rest[1:])
	}
	if t.FreeThreaded && !(major == 3 && minor >= 13) {
		return t, fmt.Errorf("freethread: ABI tag %q: free-threaded build requires Python >=3.13", raw)
	}
	t.Major = major
	t.Minor = minor
	return t, nil
}

// String renders the canonical ABI tag form.
func (t ABITag) String() string {
	if t.Interpreter == "" {
		return ""
	}
	suffix := ""
	if t.FreeThreaded {
		suffix = "t"
	}
	return fmt.Sprintf("%s%d%d%s", t.Interpreter, t.Major, t.Minor, suffix)
}

// Compatible reports whether installing a wheel built for ABI tag w
// into an interpreter described by t is sound. A free-threaded
// interpreter (3.13t) accepts free-threaded wheels of the same minor.
// It also accepts pure-Python "none" / "abi3" tags (which the caller
// passes by leaving Interpreter == ""). A legacy interpreter (cp313)
// only accepts non-free-threaded wheels of the same minor.
func (t ABITag) Compatible(w ABITag) bool {
	if w.Interpreter == "" {
		// none / abi3 / pure-python is always compatible.
		return true
	}
	if t.Interpreter != w.Interpreter {
		return false
	}
	if t.Major != w.Major || t.Minor != w.Minor {
		return false
	}
	return t.FreeThreaded == w.FreeThreaded
}
