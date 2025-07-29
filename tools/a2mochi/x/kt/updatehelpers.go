package kt

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
	root := repoRoot()
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "kt")
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "kt")
	validDir := filepath.Join(root, "tests", "vm", "valid")
	readme := filepath.Join(root, "tools", "a2mochi", "x", "kt", "README.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.kt"))
	sort.Strings(files)
	total := len(files)
	converted := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".kt")
		mark := "[ ]"
		outPath := filepath.Join(outDir, name+".out")
		validPath := filepath.Join(validDir, name+".out")
		if outData, err1 := os.ReadFile(outPath); err1 == nil {
			if validData, err2 := os.ReadFile(validPath); err2 == nil {
				if bytes.Equal(bytes.TrimSpace(outData), bytes.TrimSpace(validData)) {
					converted++
					mark = "[x]"
				}
			}
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	ts := time.Now().In(time.FixedZone("GMT+7", 7*3600)).Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# a2mochi Kotlin Converter\n\n")
	percent := float64(converted) / float64(total) * 100
	fmt.Fprintf(&buf, "Completed programs: %d/%d (%.0f%%)\n", converted, total, percent)
	fmt.Fprintf(&buf, "Date: %s\n\n", ts)
	buf.WriteString("This directory holds golden outputs for the Kotlin to Mochi converter.\n\n")
	buf.WriteString("Implemented examples:\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readme, buf.Bytes(), 0o644)
}

func repoRoot() string {
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
