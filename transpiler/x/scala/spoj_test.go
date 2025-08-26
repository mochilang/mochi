//go:build slow

package scalat_test

import (
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
	scalat "mochi/transpiler/x/scala"
	"mochi/types"
)

func indexRange() (int, int) {
	from := 1
	to := 1
	if v := os.Getenv("FROM_INDEX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n > 0 {
			from = n
			to = n
		}
	}
	if v := os.Getenv("TO_INDEX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n >= from {
			to = n
		}
	}
	return from, to
}

func TestScalaTranspiler_SPOJ_Golden(t *testing.T) {
	ensureScala(t)
	root := repoRoot()
	outDir := filepath.Join(root, "tests", "spoj", "x", "scala")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateSPOJChecklist)

	from, to := indexRange()

	bench := true
	scalat.SetBenchMain(bench)

	for idx := from; idx <= to; idx++ {
		src := filepath.Join(root, "tests", "spoj", "x", "mochi", fmt.Sprintf("%d.mochi", idx))
		if _, err := os.Stat(src); err != nil {
			t.Logf("skip %d: %v", idx, err)
			continue
		}
		idx := idx
		t.Run(fmt.Sprintf("%d", idx), func(t *testing.T) {
			codePath := filepath.Join(outDir, fmt.Sprintf("%d.scala", idx))
			outPath := filepath.Join(outDir, fmt.Sprintf("%d.out", idx))
			errPath := filepath.Join(outDir, fmt.Sprintf("%d.error", idx))
			benchPath := filepath.Join(outDir, fmt.Sprintf("%d.bench", idx))

			prog, err := parser.Parse(src)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("parse: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				_ = os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
				t.Fatalf("type: %v", errs[0])
			}
			ast, err := scalat.Transpile(prog, env, bench)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("transpile: %v", err)
			}
			code := scalat.Emit(ast)
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			tmp := t.TempDir()
			if out, err := exec.Command("scalac", "-d", tmp, codePath).CombinedOutput(); err != nil {
				_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
				t.Fatalf("compile: %v", err)
			}
			cmd := exec.Command("scala", "-cp", tmp, "Main")
			cmd.Env = append(os.Environ(), "MOCHI_BENCHMARK=1")
			inPath := filepath.Join(root, "tests", "spoj", "x", "mochi", fmt.Sprintf("%d.in", idx))
			if data, err := os.ReadFile(inPath); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			wantPath := filepath.Join(root, "tests", "spoj", "x", "mochi", fmt.Sprintf("%d.out", idx))
			want, _ := os.ReadFile(wantPath)
			want = bytes.TrimSpace(want)

			outBytes, err := cmd.CombinedOutput()
			got := bytes.TrimSpace(outBytes)
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), got...), 0o644)
				t.Fatalf("run: %v", err)
			}
			_ = os.Remove(errPath)

			benchData := got
			outPart := got
			if j := bytes.LastIndexByte(got, '{'); j >= 0 {
				outPart = bytes.TrimSpace(got[:j])
				benchData = got[j:]
			} else {
				benchData = nil
			}
			_ = os.WriteFile(outPath, outPart, 0o644)
			if benchData != nil {
				_ = os.WriteFile(benchPath, benchData, 0o644)
			}
			if want != nil && len(want) > 0 {
				if !bytes.Equal(outPart, want) {
					t.Errorf("output mismatch\nGot: %s\nWant: %s", outPart, want)
				}
			}
		})
	}
}

func updateSPOJChecklist() {
	root := repoRoot()
	outDir := filepath.Join(root, "tests", "spoj", "x", "scala")
	readmePath := filepath.Join(root, "transpiler", "x", "scala", "SPOJ.md")

	type row struct {
		Index  int
		Name   string
		Status string
		Dur    string
		Mem    string
	}

	from, to := indexRange()
	var rows []row
	for idx := from; idx <= to; idx++ {
		mdPath := filepath.Join(root, "tests", "spoj", "x", "mochi", fmt.Sprintf("%d.md", idx))
		if _, err := os.Stat(mdPath); err != nil {
			continue
		}
		name := fmt.Sprintf("Problem %d", idx)
		if data, err := os.ReadFile(mdPath); err == nil {
			line := strings.SplitN(string(data), "\n", 2)[0]
			if strings.HasPrefix(line, "# [") {
				if i := strings.Index(line, "]"); i > 3 {
					inner := line[3:i]
					if j := strings.Index(inner, " - "); j >= 0 {
						name = inner[j+3:]
					} else {
						name = inner
					}
				}
			} else if strings.HasPrefix(line, "# ") {
				name = strings.TrimSpace(line[2:])
			}
		}
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, fmt.Sprintf("%d.error", idx))); err == nil {
			status = "error"
		} else if _, err := os.Stat(filepath.Join(outDir, fmt.Sprintf("%d.scala", idx))); err == nil {
			status = "✓"
			if data, err := os.ReadFile(filepath.Join(outDir, fmt.Sprintf("%d.bench", idx))); err == nil {
				var r struct {
					DurationUS  int64 `json:"duration_us"`
					MemoryBytes int64 `json:"memory_bytes"`
				}
				data = bytes.TrimSpace(data)
				if json.Unmarshal(data, &r) == nil && r.DurationUS > 0 {
					dur = humanDuration(r.DurationUS)
					mem = humanSize(r.MemoryBytes)
				}
			}
		}
		rows = append(rows, row{Index: idx, Name: name, Status: status, Dur: dur, Mem: mem})
	}

	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for _, r := range rows {
		lines = append(lines, fmt.Sprintf("| %d | %s | %s | %s | %s |", r.Index, r.Name, r.Status, r.Dur, r.Mem))
	}

	tsRaw, _ := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := strings.TrimSpace(string(tsRaw))
	if t, err := time.Parse(time.RFC3339, ts); err == nil {
		if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
			ts = t.In(loc).Format("2006-01-02 15:04 -0700")
		} else {
			ts = t.Format("2006-01-02 15:04 MST")
		}
	}
	var buf bytes.Buffer
	compiled := 0
	for _, r := range rows {
		if r.Status == "✓" {
			compiled++
		}
	}
	buf.WriteString("# Scala SPOJ Transpiler Output\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, len(rows))
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	buf.WriteString("Checklist:\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}
