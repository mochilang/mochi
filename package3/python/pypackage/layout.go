package pypackage

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
)

// Layout is the flat path -> bytes representation of a rendered artifact tree.
// Paths use forward slashes and are relative to the layout root.
type Layout struct {
	Files map[string]string
}

// NewLayout constructs an empty Layout.
func NewLayout() *Layout {
	return &Layout{Files: map[string]string{}}
}

// Add inserts or replaces a path -> body entry. Adding the same path twice
// overwrites the prior entry; this is intentional so a per-format renderer
// can layer additional files (e.g. a wheel METADATA on top of a shared
// __init__.py).
func (l *Layout) Add(path, body string) {
	l.Files[path] = body
}

// Paths returns the sorted file paths.
func (l *Layout) Paths() []string {
	out := make([]string, 0, len(l.Files))
	for p := range l.Files {
		out = append(out, p)
	}
	sort.Strings(out)
	return out
}

// Write flushes the layout to dst, creating intermediate directories as
// needed. Returns the list of absolute paths written, sorted.
func (l *Layout) Write(dst string) ([]string, error) {
	if dst == "" {
		return nil, fmt.Errorf("pypackage: empty Write destination")
	}
	written := make([]string, 0, len(l.Files))
	for _, p := range l.Paths() {
		full := filepath.Join(dst, filepath.FromSlash(p))
		if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
			return written, fmt.Errorf("pypackage: mkdir %s: %w", filepath.Dir(full), err)
		}
		if err := os.WriteFile(full, []byte(l.Files[p]), 0o644); err != nil {
			return written, fmt.Errorf("pypackage: write %s: %w", full, err)
		}
		written = append(written, full)
	}
	return written, nil
}
