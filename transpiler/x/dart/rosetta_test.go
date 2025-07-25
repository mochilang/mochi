//go:build slow

package dartt_test

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	dartt "mochi/transpiler/x/dart"
	"mochi/types"
)

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

func TestDartTranspiler_Rosetta_Golden(t *testing.T) {
	if _, err := exec.LookPath("dart"); err != nil {
		t.Skip("dart not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Dart")
	os.MkdirAll(outDir, 0o755)

	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		t.Fatalf("read index: %v", err)
	}
	start := 1
	list := names
	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(names) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		start = idx
		list = names[idx-1 : idx]
	}
	bench := os.Getenv("MOCHI_BENCHMARK") == "true"
	for i, fname := range list {
		name := strings.TrimSuffix(fname, ".mochi")
		src := filepath.Join(srcDir, fname)
		idxNum := start + i
		ok := t.Run(fmt.Sprintf("%03d_%s", idxNum, name), func(t *testing.T) {
			codePath := filepath.Join(outDir, name+".dart")
			outPath := filepath.Join(outDir, name+".out")
			errPath := filepath.Join(outDir, name+".error")

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
			ast, err := dartt.Transpile(prog, env, bench)
			if err != nil {
				_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
				t.Fatalf("transpile: %v", err)
			}
			var buf bytes.Buffer
			if err := dartt.Emit(&buf, ast); err != nil {
				_ = os.WriteFile(errPath, []byte("emit: "+err.Error()), 0o644)
				t.Fatalf("emit: %v", err)
			}
			if err := os.WriteFile(codePath, buf.Bytes(), 0o644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			cmd := exec.Command("dart", codePath)
			if bench {
				cmd.Env = append(os.Environ(), "MOCHI_BENCHMARK=1")
			}
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			got := bytes.TrimSpace(out)
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
				t.Fatalf("run: %v", err)
			}
			_ = os.Remove(errPath)
			_ = os.WriteFile(outPath, got, 0o644)
		})
		if !ok {
			t.Fatalf("first failing program: %s", name)
		}
	}
}

func UpdateRosetta() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Dart")
	readmePath := filepath.Join(root, "transpiler", "x", "dart", "ROSETTA.md")

	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := ""
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
				ts = t.In(loc).Format("2006-01-02 15:04 -0700")
			} else {
				ts = t.Format("2006-01-02 15:04 MST")
			}
		}
	}

	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		return
	}
	total := len(names)
	completed := 0
	var lines []string
	fmtDuration := func(us int64) string {
		d := time.Duration(us) * time.Microsecond
		return d.String()
	}
	fmtSize := func(b int64) string {
		const KB = 1024
		const MB = 1024 * KB
		if b >= MB {
			return fmt.Sprintf("%.1f MB", float64(b)/float64(MB))
		}
		if b >= KB {
			return fmt.Sprintf("%.1f KB", float64(b)/float64(KB))
		}
		return fmt.Sprintf("%d B", b)
	}
	for i, fname := range names {
		name := strings.TrimSuffix(fname, ".mochi")
		mark := "[ ]"
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			if _, err2 := os.Stat(filepath.Join(outDir, name+".error")); os.IsNotExist(err2) {
				completed++
				mark = "[x]"
				data, _ := os.ReadFile(filepath.Join(outDir, name+".out"))
				if idx := bytes.LastIndexByte(data, '{'); idx != -1 {
					var bench struct {
						DurationUs  int64 `json:"duration_us"`
						MemoryBytes int64 `json:"memory_bytes"`
					}
					_ = json.Unmarshal(data[idx:], &bench)
					if bench.DurationUs > 0 {
						dur = fmtDuration(bench.DurationUs)
					}
					if bench.MemoryBytes > 0 {
						mem = fmtSize(bench.MemoryBytes)
					}
				}
			}
		}
		lines = append(lines, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, mark, dur, mem))
	}

	var buf bytes.Buffer
	buf.WriteString("# Dart Rosetta Transpiler Output\n\n")
	buf.WriteString("This directory contains Dart code generated from Mochi programs in `tests/rosetta/x/Mochi`. Each program has a `.dart` file and `.out` output. Compilation or runtime failures are captured in a `.error` file.\n\n")
	fmt.Fprintf(&buf, "Compiled and ran: %d/%d\n", completed, total)
	buf.WriteString("\n## Checklist\n")
	buf.WriteString("| Index | Name | Status | Duration | Memory |\n")
	buf.WriteString("| --- | --- | --- | --- | --- |\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	if ts != "" {
		buf.WriteString(fmt.Sprintf("\n_Last updated: %s_\n", ts))
	}
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

// updateRosetta is kept for consistency with other transpiler tests.
func updateRosetta() { UpdateRosetta() }
