//go:build slow

package vm_test

import (
	"bufio"
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

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

func runRosettaCase(t *testing.T, name string) {
	t.Helper()
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "ir")
	os.MkdirAll(outDir, 0o755)
	irPath := filepath.Join(outDir, name+".ir")
	outPath := filepath.Join(outDir, name+".out")
	benchPath := filepath.Join(outDir, name+".bench")
	errPath := filepath.Join(outDir, name+".error")

	want, _ := os.ReadFile(outPath)
	want = bytes.TrimSpace(want)

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
		t.Fatalf("type: %v", errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("compile: "+err.Error()), 0o644)
		t.Fatalf("compile: %v", err)
	}

	srcBytes, _ := os.ReadFile(src)
	ir := p.Disassemble(string(srcBytes))
	if err := os.WriteFile(irPath, []byte(ir), 0o644); err != nil {
		t.Fatalf("write ir: %v", err)
	}

	bench := os.Getenv("MOCHI_BENCHMARK") == "true" || os.Getenv("MOCHI_BENCHMARK") == "1"
	inPath := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".in")
	var in io.Reader = os.Stdin
	if r, err := os.Open(inPath); err == nil {
		defer r.Close()
		in = r
	}
	var out bytes.Buffer
	m := vm.NewWithIO(p, in, &out)
	var start time.Time
	var startMem uint64
	if bench {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem = ms.Alloc
		start = time.Now()
	}
	if err := m.Run(); err != nil {
		_ = os.WriteFile(errPath, []byte("run: "+err.Error()), 0o644)
		t.Fatalf("run: %v", err)
	}
	if bench {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		dur := time.Since(start)
		mem := int64(ms.Alloc - startMem)
		data := map[string]any{"name": "main", "duration_us": dur.Microseconds(), "memory_bytes": mem}
		js, _ := json.MarshalIndent(data, "", "  ")
		fmt.Fprintln(&out, string(js))
	}
	_ = os.Remove(errPath)
	b := bytes.TrimSpace(out.Bytes())
	if bench {
		if idx := bytes.LastIndexByte(b, '{'); idx >= 0 {
			b = b[idx:]
		}
		_ = os.WriteFile(benchPath, b, 0o644)
		return
	}
	if idx := bytes.LastIndexByte(b, '{'); idx >= 0 && bytes.Contains(b[idx:], []byte("duration_us")) {
		b = bytes.TrimSpace(b[:idx])
	}
	_ = os.WriteFile(outPath, b, 0o644)
	if updating() || len(want) == 0 {
		return
	}
	if !bytes.Equal(b, want) {
		t.Fatalf("output mismatch\nGot: %s\nWant: %s", b, want)
	}
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

	if only := os.Getenv("MOCHI_ROSETTA_ONLY"); only != "" {
		names = []string{only + ".mochi"}
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
	outDir := filepath.Join(root, "tests", "rosetta", "ir")
	md := filepath.Join(root, "runtime", "vm", "ROSETTA.md")

	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		return
	}
	total := len(names)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for i, nameFile := range names {
		name := strings.TrimSuffix(nameFile, ".mochi")
		status := " "
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			// leave unchecked
		} else if _, err := os.Stat(filepath.Join(outDir, name+".ir")); err == nil {
			compiled++
			status = "✓"
		}
		dur := ""
		mem := ""
		benchFile := filepath.Join(outDir, name+".bench")
		if data, err := os.ReadFile(benchFile); err == nil {
			var js struct {
				Duration int64 `json:"duration_us"`
				Memory   int64 `json:"memory_bytes"`
			}
			if json.Unmarshal(bytes.TrimSpace(data), &js) == nil {
				if js.Duration > 0 {
					dur = humanDuration(js.Duration)
				}
				if js.Memory > 0 {
					mem = humanSize(js.Memory)
				}
			}
		} else if data, err := os.ReadFile(filepath.Join(outDir, name+".out")); err == nil {
			var js struct {
				Duration int64 `json:"duration_us"`
				Memory   int64 `json:"memory_bytes"`
			}
			data = bytes.TrimSpace(data)
			if idx := bytes.LastIndexByte(data, '{'); idx >= 0 {
				if json.Unmarshal(data[idx:], &js) == nil && js.Duration > 0 {
					dur = humanDuration(js.Duration)
					mem = humanSize(js.Memory)
				}
			}
		}
		lines = append(lines, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, status, dur, mem))
	}
	ts := time.Now().UTC().Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# VM Rosetta Progress\n\n")
	buf.WriteString("This checklist is auto-generated.\n")
	buf.WriteString("Generated IR and outputs from programs in `tests/rosetta/x/Mochi` lives in `tests/rosetta/ir`.\n")
	buf.WriteString("Last updated: " + ts + "\n\n")
	fmt.Fprintf(&buf, "## Rosetta Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(md, buf.Bytes(), 0o644)
}

func humanDuration(us int64) string {
	d := time.Duration(us) * time.Microsecond
	return d.String()
}

func humanSize(b int64) string {
	const unit = 1024
	if b < unit {
		return fmt.Sprintf("%d B", b)
	}
	div, exp := int64(unit), 0
	for n := b / unit; n >= unit; n /= unit {
		div *= unit
		exp++
	}
	return fmt.Sprintf("%.1f %cB", float64(b)/float64(div), "KMGTPE"[exp])
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
