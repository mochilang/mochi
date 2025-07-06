package main

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	py "mochi/tools/any2mochi/py"
)

func main() {
	repo := findRepoRoot()
	testDir := filepath.Join(repo, "tests", "compiler", "py")
	outDir := filepath.Join(repo, "tests", "any2mochi", "py")
	os.MkdirAll(outDir, 0755)
	files, _ := filepath.Glob(filepath.Join(testDir, "*.py.out"))
	for _, pf := range files {
		base := strings.TrimSuffix(pf, ".py.out")
		mochiExp := base + ".mochi"
		name := filepath.Base(base)
		outFile := filepath.Join(outDir, name+".mochi")
		errFile := filepath.Join(outDir, name+".error")

		code, err := py.ConvertFile(pf)
		if err != nil {
			os.WriteFile(errFile, []byte(err.Error()+"\n"), 0644)
			os.Remove(outFile)
			continue
		}
		expected, err := os.ReadFile(mochiExp)
		if err != nil {
			continue
		}
		if normalize(code) == normalize(expected) {
			os.WriteFile(outFile, code, 0644)
			os.Remove(errFile)
		} else {
			diff := unifiedDiff(string(expected), string(code))
			os.WriteFile(errFile, []byte("generated code does not match expected\n"+diff), 0644)
			os.Remove(outFile)
		}
	}
}

func normalize(b []byte) string {
	lines := strings.Split(string(b), "\n")
	for i, l := range lines {
		if idx := strings.Index(l, "//"); idx != -1 {
			l = l[:idx]
		}
		lines[i] = strings.TrimSpace(l)
	}
	return strings.Join(lines, "")
}

func unifiedDiff(a, b string) string {
	var buf bytes.Buffer
	al := strings.Split(a, "\n")
	bl := strings.Split(b, "\n")
	max := len(al)
	if len(bl) > max {
		max = len(bl)
	}
	for i := 0; i < max; i++ {
		av := ""
		if i < len(al) {
			av = al[i]
		}
		bv := ""
		if i < len(bl) {
			bv = bl[i]
		}
		if av != bv {
			fmt.Fprintf(&buf, "-%3d| %s\n", i+1, av)
			fmt.Fprintf(&buf, "+%3d| %s\n", i+1, bv)
		}
	}
	return buf.String()
}

func findRepoRoot() string {
	dir, _ := os.Getwd()
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		p := filepath.Dir(dir)
		if p == dir {
			break
		}
		dir = p
	}
	return ""
}
