// Package lockfile is the MEP-73 lockfile integration layer. It
// owns the `[[rust-package]]` table that MEP-73 spec §3 adds to
// `mochi.lock`: schema, encoder, decoder, and drift checker
// (`mochi pkg lock --check`).
//
// The package is layering-conservative: it imports no other
// package3/rust/* module. Callers in the build pipeline compose a
// []RustPackage from their own state (ResolvedCrate +
// source-tarball hashes + capability decls) and hand it to Encode;
// the inverse Decode reads back the same shape.
package lockfile

import (
	"fmt"
	"io"
	"sort"
	"strings"
)

// SourceKind classifies where a rust-package was sourced from. The
// spec admits three: registry (the default crates.io sparse index),
// git (a git URL with an optional rev), and path (a local relative
// directory used during development).
type SourceKind string

const (
	// SourceRegistry is a crates.io sparse-index source.
	SourceRegistry SourceKind = "registry"
	// SourceGit is a git URL source.
	SourceGit SourceKind = "git"
	// SourcePath is a local path source.
	SourcePath SourceKind = "path"
)

// Source describes the origin of a rust-package. Exactly one of the
// optional fields is non-empty depending on the Kind.
type Source struct {
	// Kind is one of registry / git / path.
	Kind SourceKind
	// Registry is the sparse-index URL when Kind == SourceRegistry.
	Registry string
	// URL is the git repo URL when Kind == SourceGit.
	URL string
	// Rev is the git revision (commit / tag / branch) when Kind ==
	// SourceGit. Optional; empty pins to the default branch HEAD at
	// lock time.
	Rev string
	// Path is the local directory (relative to the manifest) when
	// Kind == SourcePath.
	Path string
}

// RustPackage is one [[rust-package]] table entry. The field names
// in the rendered TOML use the cargo-flavoured kebab-case the spec
// shows (crate-blake3, rustdoc-types-version, etc.).
type RustPackage struct {
	// Name is the published crate name as it appears on crates.io.
	Name string
	// Version is the resolved version (full N.N.N triple, never a
	// requirement form). Pre-release versions render as N.N.N-pre.
	Version string
	// Source classifies the origin (registry / git / path).
	Source Source

	// CrateBlake3 is the lowercase hex BLAKE3-256 of the .crate
	// tarball. MEP-57's primary verification hash.
	CrateBlake3 string
	// CrateSHA256 is the lowercase hex SHA-256 of the .crate
	// tarball. Matches what crates.io itself publishes.
	CrateSHA256 string

	// RustdocTypesVersion is the rustdoc-types schema version the
	// bridge ingested at lock time. A drift here is a hard error.
	RustdocTypesVersion string
	// RustdocSHA256 is the lowercase hex SHA-256 of the raw rustdoc
	// JSON document the bridge parsed. A drift here means the
	// upstream API surface changed.
	RustdocSHA256 string
	// WrapperSHA256 is the lowercase hex SHA-256 of the synthesised
	// wrapper crate's src/lib.rs. A drift here means the bridge
	// would regenerate the wrapper with different bytes.
	WrapperSHA256 string

	// CapabilitiesDeclared is the capability set the manifest
	// declared for this crate at lock time. New capabilities at
	// --check time require user re-acknowledgement (the MEP-57
	// monotonicity rule).
	CapabilitiesDeclared []string
	// Dependencies is the resolved transitive dependency tree as
	// "<crate>@<version-req>" strings.
	Dependencies []string
	// Features is the feature set the manifest enabled at lock time.
	Features []string
}

// Encode renders a slice of RustPackage entries as the TOML body
// that the lockfile inserts into mochi.lock. Entries are sorted by
// name (ascending) for deterministic byte output.
func Encode(packages []RustPackage) string {
	cp := append([]RustPackage{}, packages...)
	sort.Slice(cp, func(i, j int) bool {
		if cp[i].Name != cp[j].Name {
			return cp[i].Name < cp[j].Name
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

func writeEntry(b *strings.Builder, p RustPackage) {
	b.WriteString("[[rust-package]]\n")
	fmt.Fprintf(b, "name = %q\n", p.Name)
	fmt.Fprintf(b, "version = %q\n", p.Version)
	b.WriteString("source = ")
	writeSource(b, p.Source)
	b.WriteString("\n")
	if p.CrateBlake3 != "" {
		fmt.Fprintf(b, "crate-blake3 = %q\n", p.CrateBlake3)
	}
	if p.CrateSHA256 != "" {
		fmt.Fprintf(b, "crate-sha256 = %q\n", p.CrateSHA256)
	}
	if p.RustdocTypesVersion != "" {
		fmt.Fprintf(b, "rustdoc-types-version = %q\n", p.RustdocTypesVersion)
	}
	if p.RustdocSHA256 != "" {
		fmt.Fprintf(b, "rustdoc-sha256 = %q\n", p.RustdocSHA256)
	}
	if p.WrapperSHA256 != "" {
		fmt.Fprintf(b, "wrapper-sha256 = %q\n", p.WrapperSHA256)
	}
	writeStringArray(b, "capabilities-declared", p.CapabilitiesDeclared)
	writeStringArray(b, "dependencies", p.Dependencies)
	writeStringArray(b, "features", p.Features)
}

func writeSource(b *strings.Builder, s Source) {
	b.WriteString("{ ")
	fmt.Fprintf(b, "kind = %q", string(s.Kind))
	switch s.Kind {
	case SourceRegistry:
		if s.Registry != "" {
			fmt.Fprintf(b, ", registry = %q", s.Registry)
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
// either empty, a comment, an `[[rust-package]]` header, a flat
// `key = value` assignment, or a string-array assignment. This
// keeps the schema closed and the parser small (~150 lines).
func Decode(r io.Reader) ([]RustPackage, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, fmt.Errorf("lockfile: read: %w", err)
	}
	return decodeBytes(data)
}

// DecodeString is the string-input form of Decode.
func DecodeString(s string) ([]RustPackage, error) {
	return decodeBytes([]byte(s))
}

func decodeBytes(data []byte) ([]RustPackage, error) {
	lines := strings.Split(string(data), "\n")
	var out []RustPackage
	var cur *RustPackage
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
		if line == "[[rust-package]]" {
			flush()
			cur = &RustPackage{}
			continue
		}
		if cur == nil {
			// Lines outside a [[rust-package]] block are
			// tolerated (they may be part of the surrounding
			// mochi.lock document).
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

func setField(p *RustPackage, key, val string) error {
	switch key {
	case "name":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.Name = s
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
	case "crate-blake3":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.CrateBlake3 = s
	case "crate-sha256":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.CrateSHA256 = s
	case "rustdoc-types-version":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.RustdocTypesVersion = s
	case "rustdoc-sha256":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.RustdocSHA256 = s
	case "wrapper-sha256":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.WrapperSHA256 = s
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
	case "features":
		arr, err := parseStringArray(val)
		if err != nil {
			return err
		}
		p.Features = arr
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
		case "registry":
			src.Registry = s
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
