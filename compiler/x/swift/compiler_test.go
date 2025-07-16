//go:build slow

package swift_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/compiler/x/testutil"
)

// TestCompileValidPrograms was replaced by vm_golden_test.go which also
// executes the compiled code and verifies the output. The helper functions in
// this file remain for other tests.

func ensureSwift(t *testing.T) string {
	if env := os.Getenv("SWIFT"); env != "" {
		if p, err := exec.LookPath(env); err == nil {
			return p
		}
	}
	if p, err := exec.LookPath("swiftc"); err == nil {
		return p
	}
	if p, err := exec.LookPath("swift"); err == nil {
		return p
	}
	t.Skip("swift not found")
	return ""
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}

func updateReadme() {
	root := testutil.FindRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "swift")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s.mochi", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Machine-generated Swift Programs\n\n")
	buf.WriteString("This directory contains Swift code compiled from Mochi programs in `tests/vm/valid` using the experimental compiler.\n\n")
	fmt.Fprintf(&buf, "## Progress\n\nCompiled: %d/%d programs\n\n", compiled, total)
	buf.WriteString("## Checklist\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	os.WriteFile(filepath.Join(outDir, "README.md"), buf.Bytes(), 0644)
}
