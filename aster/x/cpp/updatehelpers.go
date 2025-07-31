//go:build slow

package cpp

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

// UpdateReadme regenerates README checklist from golden outputs.
func UpdateReadme() {
	root := repoRootDir()
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "cpp")
	outDir := filepath.Join(root, "tests", "aster", "x", "cpp")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.cpp"))
	sort.Strings(files)
	if len(files) > 25 {
		files = files[:25]
	}
	total := len(files)
	done := 0
	var lines []string
	for i, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".cpp")
		mark := "[ ]"
		gotOut, err1 := os.ReadFile(filepath.Join(outDir, name+".out"))
		wantOut, err2 := os.ReadFile(filepath.Join(srcDir, name+".out"))
		if err1 == nil && err2 == nil {
			if bytes.Equal(bytes.TrimSpace(gotOut), bytes.TrimSpace(wantOut)) {
				mark = "[x]"
				done++
			}
		}
		lines = append(lines, fmt.Sprintf("%d. %s %s", i+1, mark, name))
	}
	tz := time.FixedZone("GMT+7", 7*3600)
	var b bytes.Buffer
	b.WriteString("# C++ AST Printer\n\n")
	b.WriteString("Golden files for the C++ inspector and printer live in this directory.\n\n")
	fmt.Fprintf(&b, "Completed programs: %d/%d\n", done, total)
	fmt.Fprintf(&b, "Date: %s\n\n", time.Now().In(tz).Format("2006-01-02 15:04 MST"))
	b.WriteString("## Checklist\n")
	b.WriteString(strings.Join(lines, "\n"))
	b.WriteByte('\n')
	_ = os.WriteFile(filepath.Join(root, "aster", "x", "cpp", "README.md"), b.Bytes(), 0o644)
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
