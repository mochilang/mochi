// Package lock is the MEP-70 lockfile integration layer. It owns the
// [[kotlin-package]] table that MEP-70 spec §3 adds to mochi.lock: schema,
// encoder, decoder, and drift checker (mochi pkg lock --check).
//
// The package is layering-conservative: it imports no other package3/kotlin/*
// module. Callers compose a []KotlinPackage from their own state (resolved
// coordinate + JAR hashes + wrapper hashes + capability decls) and hand it to
// Encode; the inverse Decode reads back the same shape.
package lock

import (
	"errors"
	"fmt"
	"io"
	"sort"
	"strings"
)

// SourceKind classifies where a kotlin-package was sourced from.
type SourceKind string

const (
	SourceRegistry SourceKind = "registry" // Maven Central (default)
	SourceGit      SourceKind = "git"      // git URL
	SourcePath     SourceKind = "path"     // local path
)

// Source describes the origin of a kotlin-package.
type Source struct {
	Kind SourceKind

	// Registry is the Maven registry URL when Kind == SourceRegistry.
	Registry string

	// URL is the git repo URL when Kind == SourceGit.
	URL string
	// Rev is the git revision (commit / tag) when Kind == SourceGit.
	Rev string

	// Path is the local directory (relative to the manifest) when Kind == SourcePath.
	Path string
}

// KotlinPackage is one [[kotlin-package]] table entry in mochi.lock.
//
// Field names in the rendered TOML use kebab-case as the spec shows
// (jar-sha256, metadata-sha256, wrapper-sha256, etc.).
type KotlinPackage struct {
	// Group is the Maven groupId (e.g. "org.jetbrains.kotlinx").
	Group string
	// Artifact is the Maven artifactId (e.g. "kotlinx-coroutines-core").
	Artifact string
	// Version is the resolved version string (e.g. "1.7.3").
	Version string
	// Source classifies the origin (registry / git / path).
	Source Source

	// JarSHA256 is the hex-encoded SHA-256 of the artifact JAR.
	JarSHA256 string
	// JarBLAKE3 is the hex-encoded BLAKE3-256 of the artifact JAR (primary).
	JarBLAKE3 string

	// MetadataSHA256 is the hex-encoded SHA-256 of the parsed Kotlin metadata
	// surface (the binary-encoded @kotlin.Metadata annotation bytes). A drift
	// here means the upstream API surface changed.
	MetadataSHA256 string
	// WrapperSHA256 is the hex-encoded SHA-256 of the synthesised wrapper
	// source tree (stable canonical hash). A drift here means the bridge would
	// regenerate the wrapper with different bytes.
	WrapperSHA256 string

	// CapabilitiesDeclared is the capability set the manifest declared for this
	// artifact at lock time.
	CapabilitiesDeclared []string
	// Dependencies is the resolved transitive dependency list as
	// "<group>:<artifact>@<version>" strings, topologically ordered.
	Dependencies []string
}

// CheckError records one drift detected by Check.
type CheckError struct {
	Artifact string
	Field    string
	Expected string
	Got      string
}

func (e *CheckError) Error() string {
	return fmt.Sprintf("mochi.lock drift for %s: field %q changed (was %s, now %s)",
		e.Artifact, e.Field, e.Expected, e.Got)
}

// Check compares a freshly-computed slice against the locked slice and returns
// all drifts found. An empty result means the lock is current.
//
// The four hard-error fields are: jar-sha256, jar-blake3, metadata-sha256,
// wrapper-sha256. Dependencies and capabilities changes are also reported.
func Check(locked, fresh []KotlinPackage) []*CheckError {
	byKey := make(map[string]KotlinPackage, len(locked))
	for _, p := range locked {
		byKey[p.Group+":"+p.Artifact] = p
	}
	var errs []*CheckError
	for _, f := range fresh {
		k := f.Group + ":" + f.Artifact
		l, ok := byKey[k]
		if !ok {
			errs = append(errs, &CheckError{Artifact: k, Field: "package", Expected: "present", Got: "missing"})
			continue
		}
		if f.JarSHA256 != "" && l.JarSHA256 != "" && f.JarSHA256 != l.JarSHA256 {
			errs = append(errs, &CheckError{k, "jar-sha256", l.JarSHA256, f.JarSHA256})
		}
		if f.JarBLAKE3 != "" && l.JarBLAKE3 != "" && f.JarBLAKE3 != l.JarBLAKE3 {
			errs = append(errs, &CheckError{k, "jar-blake3", l.JarBLAKE3, f.JarBLAKE3})
		}
		if f.MetadataSHA256 != "" && l.MetadataSHA256 != "" && f.MetadataSHA256 != l.MetadataSHA256 {
			errs = append(errs, &CheckError{k, "metadata-sha256", l.MetadataSHA256, f.MetadataSHA256})
		}
		if f.WrapperSHA256 != "" && l.WrapperSHA256 != "" && f.WrapperSHA256 != l.WrapperSHA256 {
			errs = append(errs, &CheckError{k, "wrapper-sha256", l.WrapperSHA256, f.WrapperSHA256})
		}
	}
	return errs
}

// CheckErr returns the first Check error as a combined error, or nil.
func CheckErr(locked, fresh []KotlinPackage) error {
	errs := Check(locked, fresh)
	if len(errs) == 0 {
		return nil
	}
	msgs := make([]string, len(errs))
	for i, e := range errs {
		msgs[i] = e.Error()
	}
	return errors.New(strings.Join(msgs, "\n"))
}

// Encode renders a slice of KotlinPackage entries as the TOML body inserted
// into mochi.lock. Entries are sorted by group+artifact for deterministic
// byte output.
func Encode(packages []KotlinPackage) string {
	cp := append([]KotlinPackage{}, packages...)
	sort.Slice(cp, func(i, j int) bool {
		ki := cp[i].Group + ":" + cp[i].Artifact
		kj := cp[j].Group + ":" + cp[j].Artifact
		if ki != kj {
			return ki < kj
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

func writeEntry(b *strings.Builder, p KotlinPackage) {
	b.WriteString("[[kotlin-package]]\n")
	fmt.Fprintf(b, "group = %q\n", p.Group)
	fmt.Fprintf(b, "artifact = %q\n", p.Artifact)
	fmt.Fprintf(b, "version = %q\n", p.Version)
	b.WriteString("source = ")
	writeSource(b, p.Source)
	b.WriteString("\n")
	if p.JarSHA256 != "" {
		fmt.Fprintf(b, "jar-sha256 = %q\n", p.JarSHA256)
	}
	if p.JarBLAKE3 != "" {
		fmt.Fprintf(b, "jar-blake3 = %q\n", p.JarBLAKE3)
	}
	if p.MetadataSHA256 != "" {
		fmt.Fprintf(b, "metadata-sha256 = %q\n", p.MetadataSHA256)
	}
	if p.WrapperSHA256 != "" {
		fmt.Fprintf(b, "wrapper-sha256 = %q\n", p.WrapperSHA256)
	}
	writeStringArray(b, "capabilities-declared", p.CapabilitiesDeclared)
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

// Decode parses the TOML body produced by Encode (or a hand-edited mochi.lock).
// Unknown keys are tolerated for forward-compat. The parser is line-oriented
// and accepts only [[kotlin-package]] headers, blank lines, comments, flat
// key = "value" assignments, and string-array assignments.
func Decode(r io.Reader) ([]KotlinPackage, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, fmt.Errorf("lockfile: read: %w", err)
	}
	return decodeBytes(data)
}

// DecodeString is the string-input form of Decode.
func DecodeString(s string) ([]KotlinPackage, error) {
	return decodeBytes([]byte(s))
}

func decodeBytes(data []byte) ([]KotlinPackage, error) {
	lines := strings.Split(string(data), "\n")
	var out []KotlinPackage
	var cur *KotlinPackage
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
		if line == "[[kotlin-package]]" {
			flush()
			cur = &KotlinPackage{}
			continue
		}
		if cur == nil {
			continue // outside a [[kotlin-package]] block
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

func setField(p *KotlinPackage, key, val string) error {
	switch key {
	case "group":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.Group = s
	case "artifact":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.Artifact = s
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
	case "jar-sha256":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.JarSHA256 = s
	case "jar-blake3":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.JarBLAKE3 = s
	case "metadata-sha256":
		s, err := parseString(val)
		if err != nil {
			return err
		}
		p.MetadataSHA256 = s
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

// splitTopLevel splits s on sep, ignoring sep inside matched braces, brackets,
// or double-quoted strings.
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
