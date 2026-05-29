// Package lockfile is the MEP-68 lockfile integration layer. It owns the
// [[dotnet-package]] table added to mochi.lock: schema, encoder, decoder,
// and drift checker.
package lockfile

import (
	"fmt"
	"io"
	"sort"
	"strings"
)

// SourceKind classifies where a dotnet-package was sourced from.
type SourceKind string

const (
	// SourceRegistry is a NuGet registry source.
	SourceRegistry SourceKind = "registry"
	// SourceGit is a git URL source.
	SourceGit SourceKind = "git"
	// SourcePath is a local path source.
	SourcePath SourceKind = "path"
)

// Source describes the origin of a dotnet-package.
type Source struct {
	// Kind is one of registry / git / path.
	Kind SourceKind
	// Registry is the NuGet feed URL when Kind == SourceRegistry.
	Registry string
	// URL is the git repo URL when Kind == SourceGit.
	URL string
	// Rev is the git revision when Kind == SourceGit.
	Rev string
	// Path is the local directory when Kind == SourcePath.
	Path string
}

// DotNetPackage is one [[dotnet-package]] table entry.
type DotNetPackage struct {
	// ID is the NuGet package id.
	ID string
	// Version is the resolved version.
	Version string
	// Source classifies the origin.
	Source Source
	// NupkgSHA512 is the SHA-512 of the .nupkg file.
	NupkgSHA512 string
	// MetadataSHA256 is the SHA-256 of the mochi-dotnet-meta JSON output.
	MetadataSHA256 string
	// ShimSHA256 is the SHA-256 of the generated Bridge.cs.
	ShimSHA256 string
	// CapabilitiesDeclared is the capability set declared at lock time.
	CapabilitiesDeclared []string
	// TargetFramework is the TFM (e.g. "net8.0").
	TargetFramework string
	// Dependencies is the resolved transitive dependency tree as "<id>@<version-req>" strings.
	Dependencies []string
}

// Encode renders a slice of DotNetPackage as TOML for mochi.lock.
// Entries are sorted by id (ascending) for deterministic byte output.
func Encode(packages []DotNetPackage) string {
	cp := append([]DotNetPackage{}, packages...)
	sort.Slice(cp, func(i, j int) bool {
		if cp[i].ID != cp[j].ID {
			return cp[i].ID < cp[j].ID
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

func writeEntry(b *strings.Builder, p DotNetPackage) {
	b.WriteString("[[dotnet-package]]\n")
	fmt.Fprintf(b, "id = %q\n", p.ID)
	fmt.Fprintf(b, "version = %q\n", p.Version)
	b.WriteString("source = ")
	writeSource(b, p.Source)
	b.WriteString("\n")
	if p.NupkgSHA512 != "" {
		fmt.Fprintf(b, "nupkg-sha512 = %q\n", p.NupkgSHA512)
	}
	if p.MetadataSHA256 != "" {
		fmt.Fprintf(b, "metadata-sha256 = %q\n", p.MetadataSHA256)
	}
	if p.ShimSHA256 != "" {
		fmt.Fprintf(b, "shim-sha256 = %q\n", p.ShimSHA256)
	}
	writeStringArray(b, "capabilities-declared", p.CapabilitiesDeclared)
	if p.TargetFramework != "" {
		fmt.Fprintf(b, "target-framework = %q\n", p.TargetFramework)
	}
	writeStringArray(b, "dependencies", p.Dependencies)
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

// Decode parses the TOML body produced by Encode.
// Unknown keys are tolerated for forward-compat.
func Decode(r io.Reader) ([]DotNetPackage, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, fmt.Errorf("lockfile: read: %w", err)
	}
	return decodeBytes(data)
}

// DecodeString is the string-input form of Decode.
func DecodeString(s string) ([]DotNetPackage, error) {
	return decodeBytes([]byte(s))
}

func decodeBytes(data []byte) ([]DotNetPackage, error) {
	lines := strings.Split(string(data), "\n")
	var out []DotNetPackage
	var cur *DotNetPackage
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
		if line == "[[dotnet-package]]" {
			flush()
			cur = &DotNetPackage{}
			continue
		}
		if cur == nil {
			// Lines outside a [[dotnet-package]] block are tolerated.
			continue
		}
		rawKey, rawVal, hasEq := strings.Cut(line, "=")
		if !hasEq {
			return nil, fmt.Errorf("lockfile: line %d: missing '=': %q", lineno+1, line)
		}
		key := strings.TrimSpace(rawKey)
		val := strings.TrimSpace(rawVal)
		if err := setField(cur, key, val); err != nil {
			return nil, fmt.Errorf("lockfile: line %d (%s): %w", lineno+1, key, err)
		}
	}
	flush()
	return out, nil
}

func setField(p *DotNetPackage, key, val string) error {
	switch key {
	case "id":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.ID = s
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
	case "nupkg-sha512":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.NupkgSHA512 = s
	case "metadata-sha256":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.MetadataSHA256 = s
	case "shim-sha256":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.ShimSHA256 = s
	case "capabilities-declared":
		arr, err := parseStringArray(val)
		if err != nil {
			return err
		}
		p.CapabilitiesDeclared = arr
	case "target-framework":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.TargetFramework = s
	case "dependencies":
		arr, err := parseStringArray(val)
		if err != nil {
			return err
		}
		p.Dependencies = arr
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
		rawK, rawV, hasEq := strings.Cut(kv, "=")
		if !hasEq {
			return Source{}, fmt.Errorf("source key without '=': %q", kv)
		}
		k := strings.TrimSpace(rawK)
		v := strings.TrimSpace(rawV)
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

// splitTopLevel splits s on sep, ignoring sep inside braces, brackets, or strings.
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
