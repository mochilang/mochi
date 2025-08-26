//go:build slow

package cstranspiler_test

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	cs "mochi/transpiler/x/cs"
	"mochi/types"
)

func TestCSTranspiler_SPOJ_Golden(t *testing.T) {
	if _, err := exec.LookPath("dotnet"); err != nil {
		t.Skip("dotnet not installed")
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "cs")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	t.Cleanup(updateSPOJ)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Slice(files, func(i, j int) bool {
		ai, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(files[i]), ".mochi"))
		aj, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(files[j]), ".mochi"))
		return ai < aj
	})
	var filtered []string
	for _, f := range files {
		base := strings.TrimSuffix(filepath.Base(f), ".mochi")
		if _, err := os.Stat(filepath.Join(outDir, base+".in")); err == nil {
			filtered = append(filtered, f)
		}
	}
	files = filtered
	if len(files) == 0 {
		t.Fatalf("no test cases in %s", srcDir)
	}

	from, to := 1, len(files)
	if s := os.Getenv("FROM_INDEX"); s != "" {
		if v, err := strconv.Atoi(s); err == nil && v >= 1 && v <= len(files) {
			from = v
		}
	}
	if s := os.Getenv("TO_INDEX"); s != "" {
		if v, err := strconv.Atoi(s); err == nil && v >= from && v <= len(files) {
			to = v
		}
	}
	files = files[from-1 : to]

	var firstFail string
	for _, src := range files {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		idx, _ := strconv.Atoi(base)
		ok := t.Run(fmt.Sprintf("%03d", idx), func(t *testing.T) {
			codePath := filepath.Join(outDir, base+".cs")
			inPath := filepath.Join(outDir, base+".in")
			outPath := filepath.Join(outDir, base+".out")
			errPath := filepath.Join(outDir, base+".error")
			benchPath := filepath.Join(outDir, base+".bench")

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
			cs.SetBenchMain(true)
			ast, err := cs.Transpile(prog, env)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("transpile: %v", err)
			}
			code := cs.Emit(ast)
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write code: %v", err)
			}

			tmp := t.TempDir()
			proj := filepath.Join(tmp, "app.csproj")
			csproj := `<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework></PropertyGroup></Project>`
			if err := os.WriteFile(proj, []byte(csproj), 0644); err != nil {
				t.Fatalf("write csproj: %v", err)
			}
			file := filepath.Join(tmp, "Program.cs")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write temp code: %v", err)
			}
			cmd := exec.Command("dotnet", "run", "--project", proj)
			cmd.Env = append(os.Environ(), "DOTNET_NOLOGO=1", "DOTNET_SKIP_FIRST_TIME_EXPERIENCE=1", "MOCHI_BENCHMARK=1", "MOCHI_NOW_SEED=1")
			if data, err := os.ReadFile(inPath); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			want, _ := os.ReadFile(outPath)
			want = bytes.TrimSpace(want)
			out, err := cmd.CombinedOutput()
			got := bytes.TrimSpace(out)
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), got...), 0o644)
				t.Fatalf("run: %v", err)
			}
			_ = os.Remove(errPath)
			outPart := got
			benchData := []byte{}
			if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
				outPart = bytes.TrimSpace(got[:idx])
				benchData = bytes.TrimSpace(got[idx:])
			}
			_ = os.WriteFile(outPath, outPart, 0o644)
			if len(benchData) > 0 {
				_ = os.WriteFile(benchPath, benchData, 0o644)
			}
			if want != nil && len(want) > 0 {
				if !bytes.Equal(outPart, want) {
					t.Fatalf("output mismatch\nGot: %s\nWant: %s", outPart, want)
				}
			}
		})
		if !ok && firstFail == "" {
			firstFail = base
		}
	}
	if firstFail != "" {
		t.Fatalf("first failing program: %s", firstFail)
	}
}

func updateSPOJ() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "cs")
	mdPath := filepath.Join(root, "transpiler", "x", "cs", "SPOJ.md")

	pattern := filepath.Join(srcDir, "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		return
	}
	sort.Slice(files, func(i, j int) bool {
		ai, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(files[i]), ".mochi"))
		aj, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(files[j]), ".mochi"))
		return ai < aj
	})

	humanDur := func(us int64) string {
		d := time.Duration(us) * time.Microsecond
		return d.String()
	}
	humanBytes := func(n int64) string {
		units := []string{"B", "KB", "MB", "GB", "TB", "PB", "EB"}
		v := float64(n)
		u := 0
		for v >= 1024 && u < len(units)-1 {
			v /= 1024
			u++
		}
		return fmt.Sprintf("%.1f%s", v, units[u])
	}

	total := len(files)
	compiled := 0
	var rows []string
	for _, f := range files {
		base := strings.TrimSuffix(filepath.Base(f), ".mochi")
		idx, _ := strconv.Atoi(base)
		name := base
		if data, err := os.ReadFile(filepath.Join(srcDir, base+".md")); err == nil {
			if lines := strings.Split(string(data), "\n"); len(lines) > 0 {
				line := lines[0]
				if i := strings.Index(line, "["); i >= 0 {
					if j := strings.Index(line[i+1:], "]"); j >= 0 {
						name = line[i+1 : i+1+j]
					}
				} else {
					name = strings.TrimPrefix(line, "# ")
				}
			}
		}
		mark := " "
		dur := ""
		mem := ""
		errPath := filepath.Join(outDir, base+".error")
		outPath := filepath.Join(outDir, base+".out")
		benchPath := filepath.Join(outDir, base+".bench")
		if _, err := os.Stat(errPath); os.IsNotExist(err) {
			if _, err := os.Stat(outPath); err == nil {
				mark = "✓"
				compiled++
				if data, err := os.ReadFile(benchPath); err == nil {
					var res struct {
						Duration int64 `json:"duration_us"`
						Memory   int64 `json:"memory_bytes"`
					}
					data = bytes.TrimSpace(data)
					if json.Unmarshal(data, &res) == nil && res.Duration > 0 {
						dur = humanDur(res.Duration)
						mem = humanBytes(res.Memory)
					}
				}
			}
		}
		rows = append(rows, fmt.Sprintf("| %d | %s | %s | %s | %s |", idx, name, mark, dur, mem))
	}

	var buf bytes.Buffer
	fmt.Fprintf(&buf, "# SPOJ C# Transpiler Output\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", time.Now().Format("2006-01-02 15:04 MST"))
	buf.WriteString("## Checklist\n")
	buf.WriteString("| Index | Name | Status | Duration | Memory |\n")
	buf.WriteString("|------:|------|:-----:|---------:|-------:|\n")
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(mdPath, buf.Bytes(), 0o644)
}
