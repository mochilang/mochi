package build

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
)

// copySiblingGoFiles copies every *.go file that lives in the same
// directory as src into workDir. This supports the phase-10.2 Go FFI
// pattern where `extern go fun NAME(...)` declarations in a .mochi
// source file resolve against hand-written Go files placed next to it.
// Non-*.go files and subdirectories are silently ignored.
func copySiblingGoFiles(src, workDir string) error {
	srcDir := filepath.Dir(src)
	entries, err := os.ReadDir(srcDir)
	if err != nil {
		return err
	}
	for _, e := range entries {
		if e.IsDir() {
			continue
		}
		name := e.Name()
		if filepath.Ext(name) != ".go" {
			continue
		}
		if err := copyFile(filepath.Join(srcDir, name), filepath.Join(workDir, name)); err != nil {
			return err
		}
	}
	return nil
}

// copyFile copies src to dst, stripping any leading `//go:build ignore` line
// so that companion Go FFI files (which carry that tag to prevent `go build`
// from compiling them in-place) are compiled normally inside the work dir.
func copyFile(src, dst string) error {
	data, err := os.ReadFile(src)
	if err != nil {
		return err
	}
	data = stripBuildIgnore(data)
	return os.WriteFile(dst, data, 0o644)
}

// stripBuildIgnore removes a leading `//go:build ignore` line (and any
// immediately following blank line) from Go source so the copy compiles.
func stripBuildIgnore(src []byte) []byte {
	const tag = "//go:build ignore"
	line, rest, found := bytes.Cut(src, []byte("\n"))
	if !found {
		return src
	}
	if strings.TrimSpace(string(line)) != tag {
		return src
	}
	// drop optional blank separator line
	if next, after, ok := bytes.Cut(rest, []byte("\n")); ok && strings.TrimSpace(string(next)) == "" {
		return after
	}
	return rest
}
