// Package lockfile manages [[npm-package]] and [[jsr-package]] entries in mochi.lock.
// Lands in MEP-72 Phase 9.
package lockfile

import (
	"fmt"
	"os"
	"strings"
)

// NpmEntry is one [[npm-package]] lock table entry.
type NpmEntry struct {
	Name    string
	Version string
	SHA512  string // SRI integrity: "sha512-<base64>"
}

// JsrEntry is one [[jsr-package]] lock table entry.
type JsrEntry struct {
	Scope   string
	Package string
	Version string
	SHA256  string
}

// File holds the parsed lock file contents for npm and JSR packages.
type File struct {
	NpmPackages []NpmEntry
	JsrPackages []JsrEntry
}

// Read parses [[npm-package]] and [[jsr-package]] tables from a TOML lock file.
func Read(lockPath string) (*File, error) {
	data, err := os.ReadFile(lockPath)
	if err != nil {
		return nil, fmt.Errorf("lockfile: read %s: %w", lockPath, err)
	}
	return parse(string(data))
}

func parse(content string) (*File, error) {
	f := &File{}
	lines := strings.Split(content, "\n")
	var section string
	var cur map[string]string

	flush := func() {
		if cur == nil {
			return
		}
		switch section {
		case "npm-package":
			e := NpmEntry{
				Name:    cur["name"],
				Version: cur["version"],
				SHA512:  cur["sha512"],
			}
			f.NpmPackages = append(f.NpmPackages, e)
		case "jsr-package":
			e := JsrEntry{
				Scope:   cur["scope"],
				Package: cur["package"],
				Version: cur["version"],
				SHA256:  cur["sha256"],
			}
			f.JsrPackages = append(f.JsrPackages, e)
		}
		cur = nil
	}

	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		if line == "[[npm-package]]" {
			flush()
			section = "npm-package"
			cur = make(map[string]string)
			continue
		}
		if line == "[[jsr-package]]" {
			flush()
			section = "jsr-package"
			cur = make(map[string]string)
			continue
		}
		if strings.HasPrefix(line, "[[") {
			flush()
			section = ""
			cur = nil
			continue
		}
		if cur != nil {
			k, v, ok := parseKeyValue(line)
			if ok {
				cur[k] = v
			}
		}
	}
	flush()
	return f, nil
}

func parseKeyValue(line string) (string, string, bool) {
	idx := strings.IndexByte(line, '=')
	if idx < 0 {
		return "", "", false
	}
	k := strings.TrimSpace(line[:idx])
	v := strings.TrimSpace(line[idx+1:])
	v = strings.Trim(v, `"`)
	return k, v, true
}

// Write serialises the file, replacing existing [[npm-package]] and [[jsr-package]] sections.
func Write(lockPath string, f *File) error {
	existing := ""
	if data, err := os.ReadFile(lockPath); err == nil {
		existing = stripTSSections(string(data))
	}

	var b strings.Builder
	b.WriteString(strings.TrimRight(existing, "\n"))
	if b.Len() > 0 {
		b.WriteString("\n\n")
	}

	for _, e := range f.NpmPackages {
		b.WriteString("[[npm-package]]\n")
		fmt.Fprintf(&b, "name = %q\n", e.Name)
		fmt.Fprintf(&b, "version = %q\n", e.Version)
		if e.SHA512 != "" {
			fmt.Fprintf(&b, "sha512 = %q\n", e.SHA512)
		}
		b.WriteByte('\n')
	}
	for _, e := range f.JsrPackages {
		b.WriteString("[[jsr-package]]\n")
		fmt.Fprintf(&b, "scope = %q\n", e.Scope)
		fmt.Fprintf(&b, "package = %q\n", e.Package)
		fmt.Fprintf(&b, "version = %q\n", e.Version)
		if e.SHA256 != "" {
			fmt.Fprintf(&b, "sha256 = %q\n", e.SHA256)
		}
		b.WriteByte('\n')
	}

	return os.WriteFile(lockPath, []byte(b.String()), 0o644)
}

func stripTSSections(content string) string {
	var out []string
	inSection := false
	for _, line := range strings.Split(content, "\n") {
		trimmed := strings.TrimSpace(line)
		if trimmed == "[[npm-package]]" || trimmed == "[[jsr-package]]" {
			inSection = true
			continue
		}
		if strings.HasPrefix(trimmed, "[[") && inSection {
			inSection = false
		}
		if !inSection {
			out = append(out, line)
		}
	}
	return strings.Join(out, "\n")
}

// AddNpm adds or updates an npm entry.
func (f *File) AddNpm(e NpmEntry) {
	for i, existing := range f.NpmPackages {
		if existing.Name == e.Name {
			f.NpmPackages[i] = e
			return
		}
	}
	f.NpmPackages = append(f.NpmPackages, e)
}

// AddJsr adds or updates a JSR entry.
func (f *File) AddJsr(e JsrEntry) {
	for i, existing := range f.JsrPackages {
		if existing.Scope == e.Scope && existing.Package == e.Package {
			f.JsrPackages[i] = e
			return
		}
	}
	f.JsrPackages = append(f.JsrPackages, e)
}
