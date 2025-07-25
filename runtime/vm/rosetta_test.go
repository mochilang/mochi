//go:build slow

package vm_test

import (
	"bufio"
	"bytes"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func runRosettaCase(t *testing.T, name string) {
	t.Helper()
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".mochi")
	outPath := strings.TrimSuffix(src, ".mochi") + ".out"
	irDir := filepath.Join(root, "tests", "rosetta", "ir")
	os.MkdirAll(irDir, 0o755)
	irPath := filepath.Join(irDir, name+".ir")

	want, _ := os.ReadFile(outPath)
	want = bytes.TrimSpace(want)
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		t.Fatalf("compile: %v", err)
	}
	var out bytes.Buffer
	m := vm.New(p, &out)
	if err := m.Run(); err != nil {
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out.Bytes())
	_ = os.WriteFile(irPath, []byte(p.Disassemble("")), 0o644)
	if updating() || len(want) == 0 {
		return
	}
	if !bytes.Equal(got, want) {
		t.Fatalf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func readIndex(path string) ([]string, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	var names []string
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		if len(parts) == 2 {
			names = append(names, parts[1])
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return names, nil
}

func TestVM_Rosetta_Golden(t *testing.T) {
	t.Cleanup(updateRosettaChecklist)
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		t.Fatalf("read index: %v", err)
	}
	if len(names) == 0 {
		t.Fatal("no rosetta programs found")
	}

	start := 0
	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(names) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		start = idx - 1
		names = names[start : start+1]
	}

	for i, nameFile := range names {
		name := strings.TrimSuffix(nameFile, ".mochi")
		idx := start + i + 1
		if ok := t.Run(fmt.Sprintf("%03d_%s", idx, name), func(t *testing.T) { runRosettaCase(t, name) }); !ok {
			t.Fatalf("first failing program: %s", name)
		}
	}
}

func updateRosettaChecklist() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	irDir := filepath.Join(root, "tests", "rosetta", "ir")
	md := filepath.Join(root, "runtime", "vm", "ROSETTA.md")

	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		return
	}
	total := len(names)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status |")
	lines = append(lines, "|------:|------|:-----:|")
	for i, nf := range names {
		name := strings.TrimSuffix(nf, ".mochi")
		status := " "
		if _, err := os.Stat(filepath.Join(irDir, name+".ir")); err == nil {
			status = "✓"
			compiled++
		}
		lines = append(lines, fmt.Sprintf("| %d | %s | %s |", i+1, name, status))
	}
	ts := time.Now().UTC().Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# VM Rosetta Progress\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	buf.WriteString("Last updated: " + ts + "\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(md, buf.Bytes(), 0o644)
}

func repoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
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
	t.Fatal("go.mod not found (not in Go module)")
	return ""
}

func updating() bool {
	f := flag.Lookup("update")
	if f == nil {
		return false
	}
	if getter, ok := f.Value.(interface{ Get() any }); ok {
		if v, ok2 := getter.Get().(bool); ok2 {
			return v
		}
	}
	return false
}
