//go:build slow
// +build slow

package pas

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

// UpdateReadmeForTests regenerates README checklist from golden outputs.
func UpdateReadmeForTests() {
	root := repoRootDir()
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "pas")
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "pas")
	readme := filepath.Join(root, "tools", "a2mochi", "x", "pas", "README.md")
	vmDir := filepath.Join(root, "tests", "vm", "valid")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.pas"))
	sort.Strings(files)
	total := len(files)
	done := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".pas")
		mark := "[ ]"
		errPath := filepath.Join(outDir, name+".error")
		if _, err := os.Stat(errPath); err == nil {
			// keep mark empty
		} else {
			gotOut, err1 := os.ReadFile(filepath.Join(outDir, name+".out"))
			wantOut, err2 := os.ReadFile(filepath.Join(vmDir, name+".out"))
			if err1 == nil && err2 == nil && bytes.Equal(bytes.TrimSpace(gotOut), bytes.TrimSpace(wantOut)) {
				mark = "[x]"
				done++
			}
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	ts := time.Now().In(time.FixedZone("GMT+7", 7*3600)).Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# a2mochi Pascal Converter\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", done, total)
	fmt.Fprintf(&buf, "Updated: %s\n\n", ts)
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readme, buf.Bytes(), 0o644)
}

func repoRootDir() string {
	dir, _ := os.Getwd()
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
	return dir
}
