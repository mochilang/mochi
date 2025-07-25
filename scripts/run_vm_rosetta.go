package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"time"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func readIndex(path string) ([]string, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	var names []string
	s := bufio.NewScanner(f)
	for s.Scan() {
		parts := strings.Fields(s.Text())
		if len(parts) == 2 {
			names = append(names, parts[1])
		}
	}
	if err := s.Err(); err != nil {
		return nil, err
	}
	return names, nil
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

func runCase(root, name string) error {
	src := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "ir")
	os.MkdirAll(outDir, 0o755)
	irPath := filepath.Join(outDir, name+".ir")
	outPath := filepath.Join(outDir, name+".out")
	benchPath := filepath.Join(outDir, name+".bench")
	errPath := filepath.Join(outDir, name+".error")

	prog, err := parser.Parse(src)
	if err != nil {
		os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
		return fmt.Errorf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
		return fmt.Errorf("type: %v", errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		os.WriteFile(errPath, []byte("compile: "+err.Error()), 0o644)
		return fmt.Errorf("compile: %v", err)
	}

	srcBytes, _ := os.ReadFile(src)
	ir := p.Disassemble(string(srcBytes))
	if err := os.WriteFile(irPath, []byte(ir), 0o644); err != nil {
		return fmt.Errorf("write ir: %v", err)
	}

	bench := os.Getenv("MOCHI_BENCHMARK") == "1" || os.Getenv("MOCHI_BENCHMARK") == "true"
	var out bytes.Buffer
	var r io.Reader = os.Stdin
	if data, err := os.Open(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		r = data
		defer data.Close()
	}
	m := vm.NewWithIO(p, r, &out)
	var start time.Time
	var startMem uint64
	if bench {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem = ms.Alloc
		start = time.Now()
	}
	if err := m.Run(); err != nil {
		os.WriteFile(errPath, []byte("run: "+err.Error()), 0o644)
		return fmt.Errorf("run: %v", err)
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
	os.Remove(errPath)
	b := bytes.TrimSpace(out.Bytes())
	if bench {
		if idx := bytes.LastIndexByte(b, '{'); idx >= 0 {
			b = b[idx:]
		}
		return os.WriteFile(benchPath, b, 0o644)
	}
	if idx := bytes.LastIndexByte(b, '{'); idx >= 0 && bytes.Contains(b[idx:], []byte("duration_us")) {
		b = bytes.TrimSpace(b[:idx])
	}
	return os.WriteFile(outPath, b, 0o644)
}

func updateChecklist(root string, names []string) error {
	outDir := filepath.Join(root, "tests", "rosetta", "ir")
	md := filepath.Join(root, "runtime", "vm", "ROSETTA.md")
	total := len(names)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for i, nameFile := range names {
		name := strings.TrimSuffix(nameFile, ".mochi")
		status := " "
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
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
	return os.WriteFile(md, buf.Bytes(), 0o644)
}

func main() {
	root, _ := filepath.Abs(".")
	names, err := readIndex(filepath.Join(root, "tests", "rosetta", "x", "Mochi", "index.txt"))
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	if len(names) == 0 {
		fmt.Fprintln(os.Stderr, "no rosetta programs found")
		os.Exit(1)
	}
	idxStr := os.Getenv("MOCHI_ROSETTA_INDEX")
	if idxStr == "" {
		fmt.Fprintln(os.Stderr, "MOCHI_ROSETTA_INDEX not set")
		os.Exit(1)
	}
	idx, err := strconv.Atoi(idxStr)
	if err != nil || idx < 1 || idx > len(names) {
		fmt.Fprintln(os.Stderr, "invalid index")
		os.Exit(1)
	}
	name := strings.TrimSuffix(names[idx-1], ".mochi")
	if err := runCase(root, name); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	if err := updateChecklist(root, names); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
