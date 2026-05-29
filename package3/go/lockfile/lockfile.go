// Package lockfile is the MEP-74 lockfile integration layer. It
// owns the `[[go-package]]` table that MEP-74 spec §3 adds to
// `mochi.lock`: schema, encoder, decoder, and drift checker
// (`mochi pkg lock --check`).
//
// The package is layering-conservative: it imports no other
// package3/go/* module. Callers in the build pipeline compose a
// []GoPackage from their own state (resolved-module proxy fetch +
// sum.golang.org cross-check + ApiSurface hash + wrapper hash +
// capability decls) and hand it to Encode; the inverse Decode reads
// back the same shape. CheckDrift compares a fresh slice against
// the lockfile slice and returns the closed set of drift kinds.
package lockfile

import (
	"fmt"
	"io"
	"sort"
	"strings"
)

// SourceKind classifies where a go-package was sourced from. The
// spec admits three: module-proxy (the default proxy.golang.org
// HTTP API), git (a git URL with an optional rev for replaced or
// private modules), and path (a local relative directory used
// during development).
type SourceKind string

const (
	// SourceModuleProxy is a proxy.golang.org-style HTTP source.
	SourceModuleProxy SourceKind = "module-proxy"
	// SourceGit is a git URL source. Used for `GOPRIVATE` modules
	// or `[go.private]` overrides that bypass the public proxy.
	SourceGit SourceKind = "git"
	// SourcePath is a local path source for path = "..."
	// replacements.
	SourcePath SourceKind = "path"
)

// Source describes the origin of a go-package. Exactly one of the
// optional fields is non-empty depending on the Kind.
type Source struct {
	// Kind is one of module-proxy / git / path.
	Kind SourceKind
	// Proxy is the proxy URL when Kind == SourceModuleProxy.
	Proxy string
	// URL is the git repo URL when Kind == SourceGit.
	URL string
	// Rev is the git revision when Kind == SourceGit. Optional;
	// empty pins to the default branch HEAD at lock time.
	Rev string
	// Path is the local directory (relative to mochi.toml) when
	// Kind == SourcePath.
	Path string
}

// GoPackage is one [[go-package]] table entry. The field names in
// the rendered TOML use the kebab-case the MEP-74 spec shows
// (zip-blake3, zip-h1, sumdb-verified, etc.).
type GoPackage struct {
	// Module is the resolved Go module path
	// (e.g. github.com/spf13/cobra).
	Module string
	// Version is the resolved version: a semver tag (v1.8.0), a
	// pseudo-version (v0.0.0-20260520150000-abcdef012345), or a
	// git rev. Never a requirement form.
	Version string
	// Source classifies the origin (module-proxy / git / path).
	Source Source

	// ZipBlake3 is the lowercase hex BLAKE3-256 of the module .zip
	// artefact. MEP-57's primary verification hash.
	ZipBlake3 string
	// ZipH1 is the Go ecosystem's `h1:` hash:
	//   "h1:" + base64(sha256(zip-content))
	// This is what sum.golang.org publishes.
	ZipH1 string

	// SumdbVerified records whether the sum.golang.org cross-check
	// succeeded at lock time. true is the only acceptable value for
	// public modules; [go.private] modules record false plus a
	// comment naming the override.
	SumdbVerified bool
	// SumdbTreeSize is the global checksum-DB tree size at lock
	// time (leaf count when this module was looked up). Monotonic;
	// future `--check` can request a consistency proof against the
	// current tree.
	SumdbTreeSize int64
	// SumdbRecordHash records the SHA-256 of the specific log
	// record for this module version. Canonical body:
	//   "<module> <version> <h1:hash>\n"
	SumdbRecordHash string

	// ApiSurfaceSha256 is the lowercase hex SHA-256 of the JSON
	// ApiSurface the bridge ingested. A drift at --check is a hard
	// error.
	ApiSurfaceSha256 string
	// WrapperSha256 is the lowercase hex SHA-256 of the synthesised
	// cgo wrapper package's source tree (file-concatenation). A
	// drift at --check is a hard error.
	WrapperSha256 string

	// CapabilitiesDeclared is the capability set the manifest
	// declared at lock time. Subset of net / fs / proc / cgo /
	// unsafe.
	CapabilitiesDeclared []string
	// Dependencies is the resolved transitive dep graph as
	// "<module>@<version>" strings.
	Dependencies []string
	// BuildTags is the build-tag set the lock was taken under.
	// Different tag sets produce different ApiSurface trees; the
	// lockfile pins the tag set.
	BuildTags []string
}

// Encode renders a slice of GoPackage entries as the TOML body that
// the lockfile inserts into mochi.lock. Entries are sorted by
// module (ascending) then version (ascending) for deterministic
// byte output.
func Encode(packages []GoPackage) string {
	cp := append([]GoPackage{}, packages...)
	sort.Slice(cp, func(i, j int) bool {
		if cp[i].Module != cp[j].Module {
			return cp[i].Module < cp[j].Module
		}
		return cp[i].Version < cp[j].Version
	})
	var b strings.Builder
	for i, p := range cp {
		if i > 0 {
			b.WriteString("\n")
		}
		writeEntry(&b, p)
	}
	return b.String()
}

func writeEntry(b *strings.Builder, p GoPackage) {
	b.WriteString("[[go-package]]\n")
	fmt.Fprintf(b, "module = %q\n", p.Module)
	fmt.Fprintf(b, "version = %q\n", p.Version)
	b.WriteString("source = ")
	writeSource(b, p.Source)
	b.WriteString("\n")
	if p.ZipBlake3 != "" {
		fmt.Fprintf(b, "zip-blake3 = %q\n", p.ZipBlake3)
	}
	if p.ZipH1 != "" {
		fmt.Fprintf(b, "zip-h1 = %q\n", p.ZipH1)
	}
	fmt.Fprintf(b, "sumdb-verified = %v\n", p.SumdbVerified)
	if p.SumdbTreeSize != 0 {
		fmt.Fprintf(b, "sumdb-tree-size = %d\n", p.SumdbTreeSize)
	}
	if p.SumdbRecordHash != "" {
		fmt.Fprintf(b, "sumdb-record-hash = %q\n", p.SumdbRecordHash)
	}
	if p.ApiSurfaceSha256 != "" {
		fmt.Fprintf(b, "api-surface-sha256 = %q\n", p.ApiSurfaceSha256)
	}
	if p.WrapperSha256 != "" {
		fmt.Fprintf(b, "wrapper-sha256 = %q\n", p.WrapperSha256)
	}
	writeStringArray(b, "capabilities-declared", p.CapabilitiesDeclared)
	writeStringArray(b, "dependencies", p.Dependencies)
	writeStringArray(b, "build-tags", p.BuildTags)
}

func writeSource(b *strings.Builder, s Source) {
	b.WriteString("{ ")
	fmt.Fprintf(b, "kind = %q", string(s.Kind))
	switch s.Kind {
	case SourceModuleProxy:
		if s.Proxy != "" {
			fmt.Fprintf(b, ", proxy = %q", s.Proxy)
		}
	case SourceGit:
		if s.URL != "" {
			fmt.Fprintf(b, ", url = %q", s.URL)
		}
		if s.Rev != "" {
			fmt.Fprintf(b, ", rev = %q", s.Rev)
		}
	case SourcePath:
		if s.Path != "" {
			fmt.Fprintf(b, ", path = %q", s.Path)
		}
	}
	b.WriteString(" }")
}

func writeStringArray(b *strings.Builder, key string, vs []string) {
	if len(vs) == 0 {
		return
	}
	fmt.Fprintf(b, "%s = [", key)
	for i, v := range vs {
		if i > 0 {
			b.WriteString(", ")
		}
		fmt.Fprintf(b, "%q", v)
	}
	b.WriteString("]\n")
}

// Decode parses the TOML body produced by Encode (or a hand-edited
// mochi.lock containing the same shape). The reader is consumed
// entirely; unknown keys are tolerated for forward-compat but
// silently dropped.
//
// The decoder is line-oriented and rejects any line that is not
// empty, a comment, a `[[go-package]]` header, a flat `key = value`
// assignment, or a string-array / inline-table assignment. This
// keeps the schema closed and the parser small (~150 lines).
func Decode(r io.Reader) ([]GoPackage, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, fmt.Errorf("lockfile: read: %w", err)
	}
	return decodeBytes(data)
}

// DecodeString is the string-input form of Decode.
func DecodeString(s string) ([]GoPackage, error) {
	return decodeBytes([]byte(s))
}

func decodeBytes(data []byte) ([]GoPackage, error) {
	lines := strings.Split(string(data), "\n")
	var out []GoPackage
	var cur *GoPackage
	flush := func() {
		if cur != nil {
			out = append(out, *cur)
		}
		cur = nil
	}
	for lineno, raw := range lines {
		line := strings.TrimSpace(raw)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		if line == "[[go-package]]" {
			flush()
			cur = &GoPackage{}
			continue
		}
		if cur == nil {
			// Lines outside a [[go-package]] block are tolerated
			// (they may belong to the surrounding mochi.lock
			// document).
			continue
		}
		eq := strings.IndexByte(line, '=')
		if eq < 0 {
			return nil, fmt.Errorf("lockfile: line %d: missing '=': %q", lineno+1, line)
		}
		key := strings.TrimSpace(line[:eq])
		val := strings.TrimSpace(line[eq+1:])
		if err := setField(cur, key, val); err != nil {
			return nil, fmt.Errorf("lockfile: line %d (%s): %w", lineno+1, key, err)
		}
	}
	flush()
	return out, nil
}

func setField(p *GoPackage, key, val string) error {
	switch key {
	case "module":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.Module = s
	case "version":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.Version = s
	case "source":
		src, err := parseSource(val)
		if err != nil {
			return err
		}
		p.Source = src
	case "zip-blake3":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.ZipBlake3 = s
	case "zip-h1":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.ZipH1 = s
	case "sumdb-verified":
		b, err := parseBool(val)
		if err != nil {
			return err
		}
		p.SumdbVerified = b
	case "sumdb-tree-size":
		n, err := parseInt(val)
		if err != nil {
			return err
		}
		p.SumdbTreeSize = n
	case "sumdb-record-hash":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.SumdbRecordHash = s
	case "api-surface-sha256":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.ApiSurfaceSha256 = s
	case "wrapper-sha256":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.WrapperSha256 = s
	case "capabilities-declared":
		arr, err := parseStringArray(val)
		if err != nil {
			return err
		}
		p.CapabilitiesDeclared = arr
	case "dependencies":
		arr, err := parseStringArray(val)
		if err != nil {
			return err
		}
		p.Dependencies = arr
	case "build-tags":
		arr, err := parseStringArray(val)
		if err != nil {
			return err
		}
		p.BuildTags = arr
	default:
		// Unknown key: forward-compat tolerance.
	}
	return nil
}

func parseString(val string) (string, error) {
	val = strings.TrimSpace(val)
	if len(val) < 2 || val[0] != '"' || val[len(val)-1] != '"' {
		return "", fmt.Errorf("expected quoted string, got %q", val)
	}
	return val[1 : len(val)-1], nil
}

func parseBool(val string) (bool, error) {
	switch strings.TrimSpace(val) {
	case "true":
		return true, nil
	case "false":
		return false, nil
	}
	return false, fmt.Errorf("expected true / false, got %q", val)
}

func parseInt(val string) (int64, error) {
	val = strings.TrimSpace(val)
	if val == "" {
		return 0, fmt.Errorf("expected integer, got empty")
	}
	var n int64
	neg := false
	i := 0
	if val[0] == '-' {
		neg = true
		i = 1
	}
	if i == len(val) {
		return 0, fmt.Errorf("expected integer, got %q", val)
	}
	for ; i < len(val); i++ {
		c := val[i]
		if c < '0' || c > '9' {
			return 0, fmt.Errorf("expected integer, got %q", val)
		}
		n = n*10 + int64(c-'0')
	}
	if neg {
		n = -n
	}
	return n, nil
}

func parseStringArray(val string) ([]string, error) {
	val = strings.TrimSpace(val)
	if !strings.HasPrefix(val, "[") || !strings.HasSuffix(val, "]") {
		return nil, fmt.Errorf("expected [..], got %q", val)
	}
	inner := strings.TrimSpace(val[1 : len(val)-1])
	if inner == "" {
		return nil, nil
	}
	parts := splitTopLevel(inner, ',')
	out := make([]string, 0, len(parts))
	for _, p := range parts {
		s, err := parseString(p)
		if err != nil {
			return nil, fmt.Errorf("array element: %w", err)
		}
		out = append(out, s)
	}
	return out, nil
}

func parseSource(val string) (Source, error) {
	val = strings.TrimSpace(val)
	if !strings.HasPrefix(val, "{") || !strings.HasSuffix(val, "}") {
		return Source{}, fmt.Errorf("expected inline table { ... }, got %q", val)
	}
	inner := strings.TrimSpace(val[1 : len(val)-1])
	parts := splitTopLevel(inner, ',')
	src := Source{}
	for _, kv := range parts {
		eq := strings.IndexByte(kv, '=')
		if eq < 0 {
			return Source{}, fmt.Errorf("source key without '=': %q", kv)
		}
		k := strings.TrimSpace(kv[:eq])
		v := strings.TrimSpace(kv[eq+1:])
		s, err := parseString(v)
		if err != nil {
			return Source{}, fmt.Errorf("source[%s]: %w", k, err)
		}
		switch k {
		case "kind":
			src.Kind = SourceKind(s)
		case "proxy":
			src.Proxy = s
		case "url":
			src.URL = s
		case "rev":
			src.Rev = s
		case "path":
			src.Path = s
		}
	}
	if src.Kind == "" {
		return Source{}, fmt.Errorf("source missing kind: %q", val)
	}
	return src, nil
}

// splitTopLevel splits s on sep, ignoring sep when it appears inside
// matched braces, brackets, or double-quoted strings. Used so that
// inline-table values like `source = { ... }` and string arrays
// `["a", "b"]` are not split on their interior commas.
func splitTopLevel(s string, sep byte) []string {
	var out []string
	depth := 0
	inStr := false
	last := 0
	for i := 0; i < len(s); i++ {
		c := s[i]
		switch {
		case inStr:
			if c == '"' && (i == 0 || s[i-1] != '\\') {
				inStr = false
			}
		case c == '"':
			inStr = true
		case c == '{' || c == '[':
			depth++
		case c == '}' || c == ']':
			depth--
		case c == sep && depth == 0:
			out = append(out, strings.TrimSpace(s[last:i]))
			last = i + 1
		}
	}
	out = append(out, strings.TrimSpace(s[last:]))
	return out
}

// DriftKind classifies a lockfile / live-state mismatch the
// `--check` mode surfaces. The set is closed; new kinds require
// MEP-74 spec amendment.
type DriftKind string

const (
	// DriftMissing flags a live package the lockfile does not
	// record. A new `import go` line was added without re-running
	// `mochi pkg lock`.
	DriftMissing DriftKind = "missing"
	// DriftStale flags a lockfile entry that is no longer claimed
	// by any live `import go` line.
	DriftStale DriftKind = "stale"
	// DriftVersion flags a (module, version) pair where the
	// version drifted.
	DriftVersion DriftKind = "version"
	// DriftZip flags a module whose .zip BLAKE3 / h1 drifted at
	// fetch time. The hard-fail case from the spec.
	DriftZip DriftKind = "zip-hash"
	// DriftSumdb flags a module whose sum.golang.org log record
	// hash or tree size drifted.
	DriftSumdb DriftKind = "sumdb"
	// DriftAPI flags a module whose ApiSurface SHA-256 drifted.
	DriftAPI DriftKind = "api-surface"
	// DriftWrapper flags a module whose synthesised wrapper SHA-256
	// drifted.
	DriftWrapper DriftKind = "wrapper"
	// DriftCapabilities flags a module whose capability set drifted
	// (additions, in particular, require user re-acknowledgement
	// per MEP-57 monotonicity).
	DriftCapabilities DriftKind = "capabilities"
)

// Drift describes a single drift between a lockfile entry and the
// live state.
type Drift struct {
	Module   string
	Version  string
	Kind     DriftKind
	Detail   string
}

// String renders a Drift as a one-line human-readable summary, used
// in `mochi pkg lock --check` diagnostics.
func (d Drift) String() string {
	if d.Detail != "" {
		return fmt.Sprintf("%s@%s: %s drift (%s)", d.Module, d.Version, d.Kind, d.Detail)
	}
	return fmt.Sprintf("%s@%s: %s drift", d.Module, d.Version, d.Kind)
}

// CheckDrift compares a lockfile slice (`locked`) against a fresh
// live state (`live`) and returns the union of drifts. The check
// is symmetric: missing-from-lockfile entries surface as
// DriftMissing, stale-in-lockfile entries surface as DriftStale,
// and shape-mismatches surface as the field-specific drift kind.
//
// The returned slice is sorted by (module, version, kind) so the
// output is deterministic across runs.
func CheckDrift(locked, live []GoPackage) []Drift {
	lockedByKey := map[string]GoPackage{}
	lockedByMod := map[string]GoPackage{}
	for _, p := range locked {
		lockedByKey[p.Module+"@"+p.Version] = p
		lockedByMod[p.Module] = p
	}
	liveByKey := map[string]GoPackage{}
	liveByMod := map[string]GoPackage{}
	for _, p := range live {
		liveByKey[p.Module+"@"+p.Version] = p
		liveByMod[p.Module] = p
	}

	var out []Drift
	for _, p := range live {
		if _, ok := lockedByMod[p.Module]; !ok {
			out = append(out, Drift{Module: p.Module, Version: p.Version, Kind: DriftMissing})
			continue
		}
		lp := lockedByMod[p.Module]
		if lp.Version != p.Version {
			out = append(out, Drift{
				Module: p.Module, Version: p.Version, Kind: DriftVersion,
				Detail: fmt.Sprintf("locked %s -> live %s", lp.Version, p.Version),
			})
			continue
		}
		out = append(out, compareSame(lp, p)...)
	}
	for _, p := range locked {
		if _, ok := liveByMod[p.Module]; !ok {
			out = append(out, Drift{Module: p.Module, Version: p.Version, Kind: DriftStale})
		}
	}

	sort.SliceStable(out, func(i, j int) bool {
		if out[i].Module != out[j].Module {
			return out[i].Module < out[j].Module
		}
		if out[i].Version != out[j].Version {
			return out[i].Version < out[j].Version
		}
		return out[i].Kind < out[j].Kind
	})
	return out
}

// compareSame compares two GoPackage entries for the same
// (module, version) tuple and surfaces every field-level drift.
func compareSame(locked, live GoPackage) []Drift {
	var out []Drift
	add := func(kind DriftKind, detail string) {
		out = append(out, Drift{Module: locked.Module, Version: locked.Version, Kind: kind, Detail: detail})
	}
	if locked.ZipBlake3 != live.ZipBlake3 || locked.ZipH1 != live.ZipH1 {
		add(DriftZip, fmt.Sprintf("zip-blake3 %s -> %s; zip-h1 %s -> %s",
			locked.ZipBlake3, live.ZipBlake3, locked.ZipH1, live.ZipH1))
	}
	if locked.SumdbVerified != live.SumdbVerified ||
		locked.SumdbTreeSize != live.SumdbTreeSize ||
		locked.SumdbRecordHash != live.SumdbRecordHash {
		add(DriftSumdb, fmt.Sprintf("verified %v -> %v; tree-size %d -> %d; record-hash %s -> %s",
			locked.SumdbVerified, live.SumdbVerified,
			locked.SumdbTreeSize, live.SumdbTreeSize,
			locked.SumdbRecordHash, live.SumdbRecordHash))
	}
	if locked.ApiSurfaceSha256 != live.ApiSurfaceSha256 {
		add(DriftAPI, fmt.Sprintf("%s -> %s", locked.ApiSurfaceSha256, live.ApiSurfaceSha256))
	}
	if locked.WrapperSha256 != live.WrapperSha256 {
		add(DriftWrapper, fmt.Sprintf("%s -> %s", locked.WrapperSha256, live.WrapperSha256))
	}
	if !equalStringSet(locked.CapabilitiesDeclared, live.CapabilitiesDeclared) {
		add(DriftCapabilities, fmt.Sprintf("locked %v -> live %v",
			locked.CapabilitiesDeclared, live.CapabilitiesDeclared))
	}
	return out
}

// equalStringSet reports whether two []string are equal as multi-sets
// (order-insensitive, duplicate-sensitive). Used to compare capability
// arrays where order is not significant.
func equalStringSet(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	count := map[string]int{}
	for _, s := range a {
		count[s]++
	}
	for _, s := range b {
		count[s]--
		if count[s] < 0 {
			return false
		}
	}
	for _, c := range count {
		if c != 0 {
			return false
		}
	}
	return true
}
