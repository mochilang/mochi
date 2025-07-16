//go:build slow

package rbcode_test

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"testing"
)

func findRepoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("repo root not found")
	return ""
}

func writeError(dir, name string, code, stderr []byte) {
	re := regexp.MustCompile(`:(\d+):`)
	line := 0
	if m := re.FindSubmatch(stderr); m != nil {
		line, _ = strconv.Atoi(string(m[1]))
	}
	lines := bytes.Split(code, []byte{'\n'})
	start := line - 3
	if start < 0 {
		start = 0
	}
	end := line + 2
	if end > len(lines) {
		end = len(lines)
	}
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "error: %s\n", bytes.TrimSpace(stderr))
	for i := start; i < end; i++ {
		fmt.Fprintf(&buf, "%d: %s\n", i+1, lines[i])
	}
	os.WriteFile(filepath.Join(dir, name+".error"), buf.Bytes(), 0644)
}
