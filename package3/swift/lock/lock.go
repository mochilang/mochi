// Package lock is the MEP-69 lockfile integration layer. It owns the
// [[swift-package]] table that MEP-69 spec §3 adds to mochi.lock: schema,
// encoder, decoder, and drift checker (mochi pkg lock --check).
//
// The package uses hand-written TOML serialization (no external library) and
// imports no other package3/swift/* module.
package lock

import (
	"crypto/sha256"
	"fmt"
	"io"
	"os"
	"sort"
	"strings"
)

// Entry is one [[swift-package]] table entry in mochi.lock.
type Entry struct {
	// URL is the git URL or SE-0292 registry URL of the package.
	URL string
	// Version is the resolved version string, e.g. "1.2.3".
	Version string
	// SHA256 is the hex-encoded SHA-256 of the downloaded source archive.
	SHA256 string
	// Platforms is the list of platform identifiers this entry applies to.
	Platforms []string
}

// DriftReport records a single field drift detected by Check.
type DriftReport struct {
	// URL is the package URL that drifted.
	URL string
	// Field is the field name that changed.
	Field string
	// Stored is the value in the lock file.
	Stored string
	// Current is the value computed now.
	Current string
}

// Read parses a mochi.lock (or standalone) TOML file and returns the
// [[swift-package]] entries. Unknown keys are tolerated for forward
// compatibility.
func Read(lockPath string) ([]Entry, error) {
	f, err := os.Open(lockPath)
	if err != nil {
		return nil, fmt.Errorf("lock: open %s: %w", lockPath, err)
	}
	defer f.Close()
	data, err := io.ReadAll(f)
	if err != nil {
		return nil, fmt.Errorf("lock: read %s: %w", lockPath, err)
	}
	return parse(string(data))
}

// Write writes entries as [[swift-package]] TOML tables to lockPath,
// overwriting any existing file. Entries are sorted by URL for a stable diff.
func Write(lockPath string, entries []Entry) error {
	// Sort for deterministic output
	sorted := make([]Entry, len(entries))
	copy(sorted, entries)
	sort.Slice(sorted, func(i, j int) bool {
		return sorted[i].URL < sorted[j].URL
	})

	var sb strings.Builder
	for _, e := range sorted {
		sb.WriteString("[[swift-package]]\n")
		sb.WriteString(fmt.Sprintf("url = %q\n", e.URL))
		sb.WriteString(fmt.Sprintf("version = %q\n", e.Version))
		sb.WriteString(fmt.Sprintf("sha256 = %q\n", e.SHA256))
		if len(e.Platforms) > 0 {
			sb.WriteString("platforms = [")
			for i, p := range e.Platforms {
				if i > 0 {
					sb.WriteString(", ")
				}
				sb.WriteString(fmt.Sprintf("%q", p))
			}
			sb.WriteString("]\n")
		} else {
			sb.WriteString("platforms = []\n")
		}
		sb.WriteByte('\n')
	}

	return os.WriteFile(lockPath, []byte(sb.String()), 0o644)
}

// Check compares the entries in lockPath against freshly computed hashes from
// cacheDir. Returns a DriftReport for each entry whose sha256 has changed.
// Only entries present in the lock file are checked; new entries are not
// reported as drift.
func Check(lockPath, cacheDir string) ([]DriftReport, error) {
	entries, err := Read(lockPath)
	if err != nil {
		return nil, err
	}

	var reports []DriftReport
	for _, e := range entries {
		// Look for the archive in cacheDir by URL+version key
		archiveName := archiveFileName(e.URL, e.Version)
		archivePath := fmt.Sprintf("%s/%s", cacheDir, archiveName)
		currentHash, hashErr := hashFile(archivePath)
		if hashErr != nil {
			// File not present in cache, cannot check drift
			continue
		}
		if currentHash != e.SHA256 {
			reports = append(reports, DriftReport{
				URL:     e.URL,
				Field:   "sha256",
				Stored:  e.SHA256,
				Current: currentHash,
			})
		}
	}
	return reports, nil
}

// archiveFileName returns the expected archive filename in cacheDir for a
// given URL and version.
func archiveFileName(url, version string) string {
	key := fmt.Sprintf("%s:%s", url, version)
	sum := sha256.Sum256([]byte(key))
	return fmt.Sprintf("%x", sum)
}

// hashFile computes the SHA-256 of the file at path and returns it as a hex string.
func hashFile(path string) (string, error) {
	f, err := os.Open(path)
	if err != nil {
		return "", err
	}
	defer f.Close()
	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		return "", err
	}
	return fmt.Sprintf("%x", h.Sum(nil)), nil
}

// --- TOML parser ---

// parse parses [[swift-package]] entries from TOML text.
func parse(text string) ([]Entry, error) {
	var entries []Entry
	var current *Entry

	lines := strings.Split(text, "\n")
	for _, rawLine := range lines {
		line := strings.TrimSpace(rawLine)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}

		if line == "[[swift-package]]" {
			if current != nil {
				entries = append(entries, *current)
			}
			current = &Entry{}
			continue
		}

		if current == nil {
			continue
		}

		eq := strings.IndexByte(line, '=')
		if eq < 0 {
			continue
		}
		key := strings.TrimSpace(line[:eq])
		val := strings.TrimSpace(line[eq+1:])

		switch key {
		case "url":
			s, err := parseString(val)
			if err != nil {
				return nil, fmt.Errorf("lock: url: %w", err)
			}
			current.URL = s
		case "version":
			s, err := parseString(val)
			if err != nil {
				return nil, fmt.Errorf("lock: version: %w", err)
			}
			current.Version = s
		case "sha256":
			s, err := parseString(val)
			if err != nil {
				return nil, fmt.Errorf("lock: sha256: %w", err)
			}
			current.SHA256 = s
		case "platforms":
			ss, err := parseStringArray(val)
			if err != nil {
				return nil, fmt.Errorf("lock: platforms: %w", err)
			}
			current.Platforms = ss
		}
	}
	if current != nil {
		entries = append(entries, *current)
	}
	return entries, nil
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
		return nil, fmt.Errorf("expected [...], got %q", val)
	}
	inner := strings.TrimSpace(val[1 : len(val)-1])
	if inner == "" {
		return nil, nil
	}
	parts := splitTopLevel(inner, ',')
	var out []string
	for _, p := range parts {
		s, err := parseString(strings.TrimSpace(p))
		if err != nil {
			return nil, fmt.Errorf("array element: %w", err)
		}
		out = append(out, s)
	}
	return out, nil
}

// splitTopLevel splits s on sep, ignoring sep inside matched brackets or strings.
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
		case c == '[' || c == '{':
			depth++
		case c == ']' || c == '}':
			depth--
		case c == sep && depth == 0:
			out = append(out, strings.TrimSpace(s[last:i]))
			last = i + 1
		}
	}
	out = append(out, strings.TrimSpace(s[last:]))
	return out
}
