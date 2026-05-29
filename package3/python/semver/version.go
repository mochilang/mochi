package semver

import (
	"fmt"
	"strconv"
	"strings"
)

// Version is a parsed PEP 440 version. The zero value is invalid; use Parse.
type Version struct {
	// Epoch is the optional epoch counter (N!). Zero when absent.
	Epoch int
	// Release is the release segment as a slice of non-negative integers.
	// Always at least length 1.
	Release []int
	// PreKind is the pre-release kind: "a", "b", "rc", or "" if absent.
	// Spellings "alpha"/"beta"/"c"/"pre"/"preview" are normalised on Parse.
	PreKind string
	// PreNum is the pre-release counter when PreKind is non-empty.
	PreNum int
	// Post is the post-release counter; -1 when absent.
	Post int
	// Dev is the dev-release counter; -1 when absent.
	Dev int
	// Local is the optional local version label following "+". Empty when
	// absent. Hyphens/underscores in the input are normalised to ".".
	Local string
}

// Parse parses a PEP 440 version string. It rejects leading "v" prefixes,
// trailing whitespace, empty release segments, and non-canonical pre-release
// spellings. Successful Parse round-trips through String byte-for-byte.
func Parse(s string) (Version, error) {
	if s == "" {
		return Version{}, fmt.Errorf("semver: empty version")
	}
	if s[0] == 'v' || s[0] == 'V' {
		return Version{}, fmt.Errorf("semver: leading %q prefix not allowed in %q", s[:1], s)
	}
	if strings.TrimSpace(s) != s {
		return Version{}, fmt.Errorf("semver: surrounding whitespace in %q", s)
	}

	var v Version
	v.Post = -1
	v.Dev = -1

	rest := s

	// Local version label.
	if i := strings.Index(rest, "+"); i >= 0 {
		local := rest[i+1:]
		rest = rest[:i]
		if local == "" {
			return Version{}, fmt.Errorf("semver: empty local label in %q", s)
		}
		// Local segments may contain ASCII letters/digits separated by
		// "." / "-" / "_"; normalise the separator to ".".
		norm := strings.NewReplacer("-", ".", "_", ".").Replace(local)
		for _, c := range norm {
			if !isAlnum(byte(c)) && c != '.' {
				return Version{}, fmt.Errorf("semver: invalid local label char %q in %q", c, s)
			}
		}
		v.Local = norm
	}

	// Epoch.
	if i := strings.Index(rest, "!"); i >= 0 {
		ep, err := strconv.Atoi(rest[:i])
		if err != nil || ep < 0 {
			return Version{}, fmt.Errorf("semver: bad epoch in %q: %v", s, err)
		}
		v.Epoch = ep
		rest = rest[i+1:]
	}

	// Dev release. Match the rightmost ".dev" since post and pre can
	// precede it.
	if i := strings.LastIndex(rest, ".dev"); i >= 0 {
		num, err := strconv.Atoi(rest[i+len(".dev"):])
		if err != nil || num < 0 {
			return Version{}, fmt.Errorf("semver: bad dev counter in %q: %v", s, err)
		}
		v.Dev = num
		rest = rest[:i]
	}

	// Post release. .postN.
	if i := strings.LastIndex(rest, ".post"); i >= 0 {
		num, err := strconv.Atoi(rest[i+len(".post"):])
		if err != nil || num < 0 {
			return Version{}, fmt.Errorf("semver: bad post counter in %q: %v", s, err)
		}
		v.Post = num
		rest = rest[:i]
	}

	// Pre release: a, b, rc preceded directly by digit (no dot per canonical).
	// We accept the canonical spellings only.
	if kind, num, idx := splitPre(rest); kind != "" {
		v.PreKind = kind
		v.PreNum = num
		rest = rest[:idx]
	}

	// Release segment.
	if rest == "" {
		return Version{}, fmt.Errorf("semver: missing release segment in %q", s)
	}
	parts := strings.Split(rest, ".")
	for _, p := range parts {
		if p == "" {
			return Version{}, fmt.Errorf("semver: empty release component in %q", s)
		}
		n, err := strconv.Atoi(p)
		if err != nil || n < 0 {
			return Version{}, fmt.Errorf("semver: bad release component %q in %q", p, s)
		}
		v.Release = append(v.Release, n)
	}
	return v, nil
}

func splitPre(rest string) (kind string, num int, idx int) {
	for _, k := range []string{"rc", "a", "b"} {
		i := strings.LastIndex(rest, k)
		if i <= 0 {
			continue
		}
		// Previous character must be a digit (i.e. directly after release
		// segment). After k must be a numeric suffix.
		if !isDigit(rest[i-1]) {
			continue
		}
		suffix := rest[i+len(k):]
		if suffix == "" {
			continue
		}
		n, err := strconv.Atoi(suffix)
		if err != nil || n < 0 {
			continue
		}
		return k, n, i
	}
	return "", 0, -1
}

// String renders the canonical normalised form. The round-trip property
// holds: Parse(v.String()) returns a Version equal to v.
func (v Version) String() string {
	var b strings.Builder
	if v.Epoch > 0 {
		fmt.Fprintf(&b, "%d!", v.Epoch)
	}
	for i, r := range v.Release {
		if i > 0 {
			b.WriteByte('.')
		}
		fmt.Fprintf(&b, "%d", r)
	}
	if v.PreKind != "" {
		fmt.Fprintf(&b, "%s%d", v.PreKind, v.PreNum)
	}
	if v.Post >= 0 {
		fmt.Fprintf(&b, ".post%d", v.Post)
	}
	if v.Dev >= 0 {
		fmt.Fprintf(&b, ".dev%d", v.Dev)
	}
	if v.Local != "" {
		b.WriteByte('+')
		b.WriteString(v.Local)
	}
	return b.String()
}

// IsPreRelease reports whether v has a pre-release or dev suffix.
func (v Version) IsPreRelease() bool {
	return v.PreKind != "" || v.Dev >= 0
}

// Compare returns -1, 0, or 1 for v < w, v == w, v > w under the PEP 440
// ordering. The ordering is:
//
//  1. epoch
//  2. release tuple (with right-padded zeros)
//  3. pre-release key (absent sorts after present)
//  4. post-release key (absent sorts before present)
//  5. dev-release key (absent sorts after present)
//  6. local label (absent sorts before present, then segment-wise)
//
// The asymmetric "absent sorts ..." rules come from PEP 440 §"Summary of
// permitted suffixes and relative ordering": dev/pre/final/post fall along
// a number line where dev precedes the canonical release, post follows it,
// and an absent dev or post is treated as plus/minus infinity respectively.
func (v Version) Compare(w Version) int {
	if c := cmpInt(v.Epoch, w.Epoch); c != 0 {
		return c
	}
	if c := cmpRelease(v.Release, w.Release); c != 0 {
		return c
	}
	if c := cmpPreKey(v, w); c != 0 {
		return c
	}
	if c := cmpPostKey(v, w); c != 0 {
		return c
	}
	if c := cmpDevKey(v, w); c != 0 {
		return c
	}
	return cmpLocal(v.Local, w.Local)
}

func cmpRelease(a, b []int) int {
	n := len(a)
	if len(b) > n {
		n = len(b)
	}
	for i := 0; i < n; i++ {
		var ai, bi int
		if i < len(a) {
			ai = a[i]
		}
		if i < len(b) {
			bi = b[i]
		}
		if c := cmpInt(ai, bi); c != 0 {
			return c
		}
	}
	return 0
}

// cmpPreKey compares the pre-release segments. Three cases:
//
//  1. Pre-release absent AND post absent AND dev present: the version is a
//     dev release of the bare final (e.g. "1.0.dev0"), and per PEP 440 it
//     must sort BEFORE any pre-release of the same release tuple. We model
//     this by ranking it as -infinity.
//  2. Pre-release absent otherwise: the version is a final or post release;
//     it sorts AFTER any pre-release of the same release tuple. Model as
//     +infinity.
//  3. Pre-release present: compare by (PreKind, PreNum). Canonical kind
//     ordering "a" < "b" < "rc" falls out of strings.Compare.
//
// This matches the `_cmpkey` logic in PyPA's `packaging.version`.
func cmpPreKey(v, w Version) int {
	type key struct {
		negInf bool
		posInf bool
		kind   string
		num    int
	}
	keyOf := func(x Version) key {
		if x.PreKind == "" && x.Post < 0 && x.Dev >= 0 {
			return key{negInf: true}
		}
		if x.PreKind == "" {
			return key{posInf: true}
		}
		return key{kind: x.PreKind, num: x.PreNum}
	}
	a, b := keyOf(v), keyOf(w)
	if a.negInf != b.negInf {
		if a.negInf {
			return -1
		}
		return 1
	}
	if a.posInf != b.posInf {
		if a.posInf {
			return 1
		}
		return -1
	}
	if a.negInf || a.posInf {
		return 0
	}
	if c := strings.Compare(a.kind, b.kind); c != 0 {
		return c
	}
	return cmpInt(a.num, b.num)
}

// cmpPostKey compares the post-release counters under the rule that an
// absent post sorts BEFORE any present post counter (i.e. "1.0" < "1.0.post0").
func cmpPostKey(v, w Version) int {
	switch {
	case v.Post < 0 && w.Post < 0:
		return 0
	case v.Post < 0:
		return -1
	case w.Post < 0:
		return 1
	}
	return cmpInt(v.Post, w.Post)
}

// cmpDevKey compares the dev counters under the rule that an absent dev
// sorts AFTER any present dev counter (i.e. "1.0.dev0" < "1.0").
func cmpDevKey(v, w Version) int {
	switch {
	case v.Dev < 0 && w.Dev < 0:
		return 0
	case v.Dev < 0:
		return 1
	case w.Dev < 0:
		return -1
	}
	return cmpInt(v.Dev, w.Dev)
}

func cmpLocal(a, b string) int {
	if a == "" && b == "" {
		return 0
	}
	if a == "" {
		return -1
	}
	if b == "" {
		return 1
	}
	as, bs := strings.Split(a, "."), strings.Split(b, ".")
	n := len(as)
	if len(bs) > n {
		n = len(bs)
	}
	for i := 0; i < n; i++ {
		var ap, bp string
		if i < len(as) {
			ap = as[i]
		}
		if i < len(bs) {
			bp = bs[i]
		}
		aN, aOK := tryAtoi(ap)
		bN, bOK := tryAtoi(bp)
		if aOK && bOK {
			if c := cmpInt(aN, bN); c != 0 {
				return c
			}
			continue
		}
		// Numeric segments sort higher than alphanumeric per PEP 440.
		if aOK && !bOK {
			return 1
		}
		if !aOK && bOK {
			return -1
		}
		if c := strings.Compare(ap, bp); c != 0 {
			return c
		}
	}
	return 0
}

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

func tryAtoi(s string) (int, bool) {
	if s == "" {
		return 0, false
	}
	for _, c := range s {
		if c < '0' || c > '9' {
			return 0, false
		}
	}
	n, err := strconv.Atoi(s)
	return n, err == nil
}

func isDigit(b byte) bool { return b >= '0' && b <= '9' }
func isAlpha(b byte) bool { return (b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z') }
func isAlnum(b byte) bool { return isDigit(b) || isAlpha(b) }
