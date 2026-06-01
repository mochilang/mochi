package lockfile

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// Entry is the simplified [[dotnet-package]] entry as written to mochi.lock.
// It carries just the fields needed for version pinning and integrity.
type Entry struct {
	ID      string
	Version string
	RIDs    []string
	SHA256  string
}

// Read parses all [[dotnet-package]] tables from a mochi.lock file.
// Other TOML tables in the file are ignored.
func Read(lockPath string) ([]Entry, error) {
	data, err := os.ReadFile(lockPath)
	if err != nil {
		return nil, fmt.Errorf("lockfile.Read: %w", err)
	}
	return parseEntries(string(data))
}

// Write writes [[dotnet-package]] tables to the lock file.
// If the file already contains a dotnet-package section, it is replaced.
// Other sections are preserved.
func Write(lockPath string, entries []Entry) error {
	// Read existing content.
	existing := ""
	if data, err := os.ReadFile(lockPath); err == nil {
		existing = string(data)
	}

	// Strip existing [[dotnet-package]] sections.
	stripped := stripDotnetSection(existing)
	newSection := encodeEntries(entries)

	var out strings.Builder
	trimmed := strings.TrimRight(stripped, "\n")
	if trimmed != "" {
		out.WriteString(trimmed)
		out.WriteString("\n\n")
	}
	out.WriteString(newSection)

	if err := os.MkdirAll(filepath.Dir(lockPath), 0o755); err != nil {
		return fmt.Errorf("lockfile.Write: mkdir: %w", err)
	}
	return os.WriteFile(lockPath, []byte(out.String()), 0o644)
}

// Check verifies that all [[dotnet-package]] entries match installed packages
// in cacheDir. Returns a list of DriftReports for any mismatches.
// cacheDir layout: <cacheDir>/<id>/<version>/<id>.nupkg
func Check(lockPath, cacheDir string) ([]DriftReport, error) {
	entries, err := Read(lockPath)
	if err != nil {
		return nil, fmt.Errorf("lockfile.Check: read: %w", err)
	}

	var reports []DriftReport
	for _, e := range entries {
		// Look for installed version directory.
		installedVersion, installedSHA256, found := findInstalled(cacheDir, e.ID)
		if !found {
			reports = append(reports, DriftReport{
				ID:      e.ID,
				Field:   "version",
				Stored:  e.Version,
				Current: "(not installed)",
			})
			continue
		}
		if installedVersion != e.Version {
			reports = append(reports, DriftReport{
				ID:      e.ID,
				Field:   "version",
				Stored:  e.Version,
				Current: installedVersion,
			})
		}
		if e.SHA256 != "" && installedSHA256 != "" && installedSHA256 != e.SHA256 {
			reports = append(reports, DriftReport{
				ID:      e.ID,
				Field:   "sha256",
				Stored:  e.SHA256,
				Current: installedSHA256,
			})
		}
	}
	return reports, nil
}

// findInstalled looks for <cacheDir>/<id>/<version>/ directories.
// Returns the installed version and sha256 hash of the .nupkg file.
func findInstalled(cacheDir, id string) (version, sha256 string, found bool) {
	pkgDir := filepath.Join(cacheDir, id)
	entries, err := os.ReadDir(pkgDir)
	if err != nil {
		return "", "", false
	}
	// Return the first version directory found (latest lexicographically).
	for i := len(entries) - 1; i >= 0; i-- {
		e := entries[i]
		if !e.IsDir() {
			continue
		}
		ver := e.Name()
		nupkgPath := filepath.Join(pkgDir, ver, id+".nupkg")
		if _, statErr := os.Stat(nupkgPath); statErr == nil {
			// Compute hash.
			h, hashErr := hashFileSHA256(nupkgPath)
			if hashErr != nil {
				h = ""
			}
			return ver, h, true
		}
		// Also accept the version directory itself as "installed" even without .nupkg.
		return ver, "", true
	}
	return "", "", false
}

// parseEntries extracts Entry values from TOML content.
func parseEntries(content string) ([]Entry, error) {
	lines := strings.Split(content, "\n")
	var out []Entry
	var cur *Entry
	flush := func() {
		if cur != nil {
			out = append(out, *cur)
		}
		cur = nil
	}
	for _, raw := range lines {
		line := strings.TrimSpace(raw)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		if line == "[[dotnet-package]]" {
			flush()
			cur = &Entry{}
			continue
		}
		if cur == nil {
			continue
		}
		k, v, hasEq := strings.Cut(line, "=")
		if !hasEq {
			continue
		}
		key := strings.TrimSpace(k)
		val := strings.TrimSpace(v)
		switch key {
		case "id":
			s, err := parseEntryString(val)
			if err == nil {
				cur.ID = s
			}
		case "version":
			s, err := parseEntryString(val)
			if err == nil {
				cur.Version = s
			}
		case "rids":
			arr, err := parseEntryArray(val)
			if err == nil {
				cur.RIDs = arr
			}
		case "sha256":
			s, err := parseEntryString(val)
			if err == nil {
				cur.SHA256 = s
			}
		}
	}
	flush()
	return out, nil
}

// encodeEntries renders entries as TOML [[dotnet-package]] tables.
func encodeEntries(entries []Entry) string {
	var b strings.Builder
	for i, e := range entries {
		if i > 0 {
			b.WriteString("\n")
		}
		b.WriteString("[[dotnet-package]]\n")
		fmt.Fprintf(&b, "id = %q\n", e.ID)
		fmt.Fprintf(&b, "version = %q\n", e.Version)
		if len(e.RIDs) > 0 {
			b.WriteString("rids = [")
			for j, r := range e.RIDs {
				if j > 0 {
					b.WriteString(", ")
				}
				fmt.Fprintf(&b, "%q", r)
			}
			b.WriteString("]\n")
		}
		if e.SHA256 != "" {
			fmt.Fprintf(&b, "sha256 = %q\n", e.SHA256)
		}
	}
	return b.String()
}

// stripDotnetSection removes all [[dotnet-package]] blocks from content.
func stripDotnetSection(content string) string {
	lines := strings.Split(content, "\n")
	var out []string
	inBlock := false
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		if trimmed == "[[dotnet-package]]" {
			inBlock = true
			continue
		}
		if inBlock {
			// End of block: next [[...]] header or blank line followed by [[
			if strings.HasPrefix(trimmed, "[[") {
				inBlock = false
				out = append(out, line)
			}
			// Skip lines inside the dotnet-package block
			continue
		}
		out = append(out, line)
	}
	return strings.Join(out, "\n")
}

func parseEntryString(val string) (string, error) {
	val = strings.TrimSpace(val)
	if len(val) < 2 || val[0] != '"' || val[len(val)-1] != '"' {
		return "", fmt.Errorf("expected quoted string")
	}
	return val[1 : len(val)-1], nil
}

func parseEntryArray(val string) ([]string, error) {
	val = strings.TrimSpace(val)
	if !strings.HasPrefix(val, "[") || !strings.HasSuffix(val, "]") {
		return nil, fmt.Errorf("expected array")
	}
	inner := strings.TrimSpace(val[1 : len(val)-1])
	if inner == "" {
		return nil, nil
	}
	parts := strings.Split(inner, ",")
	out := make([]string, 0, len(parts))
	for _, p := range parts {
		s, err := parseEntryString(strings.TrimSpace(p))
		if err != nil {
			return nil, err
		}
		out = append(out, s)
	}
	return out, nil
}
