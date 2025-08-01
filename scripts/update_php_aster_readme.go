//go:build archive

package main

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

func main() {
	root := "."
	outDir := filepath.Join(root, "tests", "aster", "x", "php")
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "php")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.php"))
	sort.Strings(files)
	var names []string
	for _, f := range files {
		names = append(names, strings.TrimSuffix(filepath.Base(f), ".php"))
	}
	total := len(names)
	processed := 0
	var lines []string
	for i, name := range names {
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".php")); err == nil {
			if _, err2 := os.Stat(filepath.Join(outDir, name+".out")); err2 == nil {
				if _, err3 := os.Stat(filepath.Join(outDir, name+".php.json")); err3 == nil {
					mark = "[x]"
					processed++
				}
			}
		} else if _, err := os.Stat(filepath.Join(outDir, name+".php.json")); err == nil {
			mark = "[x]"
			processed++
		}
		lines = append(lines, fmt.Sprintf("%d. %s %s", i+1, mark, name))
	}

	now := time.Now().In(time.FixedZone("GMT+7", 7*3600))
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "# PHP AST Print Progress (%d/%d completed)\n\n", processed, total)
	buf.WriteString("This file is auto-generated by `update_php_aster_readme.go`.\n")
	buf.WriteString("The checklist tracks PHP programs used in golden tests.\n\n")
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	fmt.Fprintf(&buf, "\nUpdated: %s\n", now.Format("2006-01-02 15:04 MST"))

	os.WriteFile(filepath.Join("aster", "x", "php", "README.md"), buf.Bytes(), 0644)
}
