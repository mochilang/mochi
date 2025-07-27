//go:build slow

package cstranspiler_test

import (
	"bufio"
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

func TestCSTranspiler_Rosetta_Golden(t *testing.T) {
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "CS")
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateRosetta)

	if _, err := exec.LookPath("dotnet"); err != nil {
		t.Skip("dotnet not installed")
	}

	_ = updateIndex(srcDir)
	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		t.Fatalf("read index: %v", err)
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
	files := make([]string, len(names))
	for i, n := range names {
		files[i] = filepath.Join(srcDir, n)
	}
	firstOnly := os.Getenv("MOCHI_FIRST_ERROR") == "1"

	runCase := func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".cs")
		outPath := filepath.Join(outDir, base+".out")
		benchPath := filepath.Join(outDir, base+".bench")
		errPath := filepath.Join(outDir, base+".error")

		prog, err := parser.Parse(src)
		if err != nil {
			_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
			return nil, errs[0]
		}
		bench := os.Getenv("MOCHI_BENCHMARK") == "true" || os.Getenv("MOCHI_BENCHMARK") == "1"
		cs.SetBenchMain(bench)
		ast, err := cs.Transpile(prog, env)
		if err != nil {
			_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
			return nil, err
		}
		code := cs.Emit(ast)
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		tmp := t.TempDir()
		proj := filepath.Join(tmp, "app.csproj")
		csproj := `<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework></PropertyGroup></Project>`
		if err := os.WriteFile(proj, []byte(csproj), 0644); err != nil {
			return nil, err
		}
		file := filepath.Join(tmp, "Program.cs")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, err
		}
		cmd := exec.Command("dotnet", "run", "--project", proj)
		envs := []string{"DOTNET_NOLOGO=1", "DOTNET_SKIP_FIRST_TIME_EXPERIENCE=1"}
		if !bench {
			envs = append(envs, "MOCHI_NOW_SEED=1")
		}
		cmd.Env = append(os.Environ(), envs...)
		inPath := filepath.Join(srcDir, base+".in")
		if data, err := os.ReadFile(inPath); err == nil {
			env := "MOCHI_INPUT_FILE=" + inPath
			cmd.Env = append(cmd.Env, env)
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			_ = os.WriteFile(errPath, append([]byte("dotnet run: "+err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		_ = os.Remove(errPath)
		outBytes := bytes.TrimSpace(out)
		if bench {
			if idx := bytes.LastIndexByte(outBytes, '{'); idx >= 0 {
				outBytes = outBytes[idx:]
			}
			_ = os.WriteFile(benchPath, outBytes, 0o644)
		} else {
			_ = os.WriteFile(outPath, outBytes, 0o644)
		}
		return outBytes, nil
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		ok := t.Run(name, func(t *testing.T) {
			if _, err := runCase(src); err != nil {
				t.Fatalf("%v", err)
			}
		})
		if firstOnly && !ok {
			break
		}
	}
}

func updateRosetta() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "CS")
	mdPath := filepath.Join(root, "transpiler", "x", "cs", "ROSETTA.md")

	names, _ := readIndex(filepath.Join(srcDir, "index.txt"))
	total := len(names)
	compiled := 0
	var rows []string

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

	for i, name := range names {
		base := strings.TrimSuffix(name, ".mochi")
		mark := " "
		dur := ""
		mem := ""
		benchFile := filepath.Join(outDir, base+".bench")
		outPath := filepath.Join(outDir, base+".out")
		if data, err := os.ReadFile(benchFile); err == nil {
			var res struct {
				Duration int64 `json:"duration_us"`
				Memory   int64 `json:"memory_bytes"`
			}
			data = bytes.TrimSpace(data)
			if json.Unmarshal(data, &res) == nil {
				if res.Duration > 0 {
					dur = humanDur(res.Duration)
				}
				if res.Memory > 0 {
					mem = humanBytes(res.Memory)
				}
			}
			if _, err2 := os.Stat(filepath.Join(outDir, base+".error")); os.IsNotExist(err2) {
				compiled++
				mark = "✓"
			}
		} else if data, err := os.ReadFile(outPath); err == nil {
			var res struct {
				Duration int64 `json:"duration_us"`
				Memory   int64 `json:"memory_bytes"`
			}
			if idx := bytes.LastIndexByte(data, '{'); idx >= 0 {
				if json.Unmarshal(data[idx:], &res) == nil {
					if res.Duration > 0 {
						dur = humanDur(res.Duration)
					}
					if res.Memory > 0 {
						mem = humanBytes(res.Memory)
					}
				}
			}
			if _, err2 := os.Stat(filepath.Join(outDir, base+".error")); os.IsNotExist(err2) {
				compiled++
				mark = "✓"
			}
		}
		rows = append(rows, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, base, mark, dur, mem))
	}

	var buf bytes.Buffer
	fmt.Fprintf(&buf, "# Rosetta C# Transpiler Output\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	if out, err := exec.Command("git", "log", "-1", "--format=%cI").Output(); err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			fmt.Fprintf(&buf, "Last updated: %s\n", t.Format("2006-01-02 15:04 MST"))
		}
	}
	buf.WriteString("\n## Checklist\n")
	buf.WriteString("| Index | Name | Status | Duration | Memory |\n")
	buf.WriteString("|------:|------|:-----:|---------:|-------:|\n")
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(mdPath, buf.Bytes(), 0o644)
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

func updateIndex(dir string) error {
	pattern := filepath.Join(dir, "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		return err
	}
	sort.Strings(files)
	var buf bytes.Buffer
	for i, f := range files {
		fmt.Fprintf(&buf, "%d %s\n", i+1, filepath.Base(f))
	}
	return os.WriteFile(filepath.Join(dir, "index.txt"), buf.Bytes(), 0o644)
}
