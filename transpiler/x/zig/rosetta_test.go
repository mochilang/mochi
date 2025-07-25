//go:build slow

package zigt_test

import (
	"bufio"
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	zigt "mochi/transpiler/x/zig"
	"mochi/types"
)

var updateRosetta = flag.Bool("update-rosetta-zig", false, "update rosetta golden files")

func readIndex(dir string) ([]string, error) {
	path := filepath.Join(dir, "index.txt")
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	scanner := bufio.NewScanner(f)
	var names []string
	for scanner.Scan() {
		fields := strings.Fields(scanner.Text())
		if len(fields) == 2 {
			names = append(names, fields[1])
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return names, nil
}

func TestZigTranspiler_Rosetta(t *testing.T) {
	t.Cleanup(updateRosettaReadme)
	if _, err := exec.LookPath("zig"); err != nil {
		t.Skip("zig not installed")
	}
	root := rosettaRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Zig")
	os.MkdirAll(outDir, 0o755)

	names, err := readIndex(srcDir)
	if err != nil {
		t.Fatalf("read index: %v", err)
	}

	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(names) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		name := names[idx-1]
		src := filepath.Join(srcDir, name)
		if _, err := runCase(src, outDir); err != nil {
			t.Fatalf("%s: %v", name, err)
		}
		return
	}

	var firstFail string
	for _, name := range names {
		src := filepath.Join(srcDir, name)
		base := strings.TrimSuffix(name, ".mochi")
		ok := t.Run(base, func(t *testing.T) {
			if _, err := runCase(src, outDir); err != nil {
				t.Fatalf("%v", err)
			}
		})
		if !ok {
			firstFail = base
			break
		}
	}
	if firstFail != "" {
		t.Fatalf("first failing program: %s", firstFail)
	}
	return
}

func runCase(src, outDir string) ([]byte, error) {
	name := strings.TrimSuffix(filepath.Base(src), ".mochi")
	bench := os.Getenv("MOCHI_BENCHMARK") == "true"
	prog, err := parser.Parse(src)
	if err != nil {
		writeErr(outDir, name, fmt.Errorf("parse: %w", err))
		return nil, err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeErr(outDir, name, fmt.Errorf("type: %v", errs[0]))
		return nil, errs[0]
	}
	ast, err := zigt.Transpile(prog, env, bench)
	if err != nil {
		writeErr(outDir, name, fmt.Errorf("transpile: %v", err))
		return nil, err
	}
	code := ast.Emit()
	codePath := filepath.Join(outDir, name+".zig")
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		return nil, err
	}
	cmdArgs := []string{"run", codePath}
	if bench {
		cmdArgs = append(cmdArgs, "-lc")
	}
	cmd := exec.Command("zig", cmdArgs...)
	cmdEnv := append(os.Environ(), "MOCHI_BENCHMARK=0")
	if bench {
		cmdEnv[len(cmdEnv)-1] = "MOCHI_BENCHMARK=1"
	} else {
		cmdEnv = append(cmdEnv, "MOCHI_NOW_SEED=1")
	}
	cmd.Env = cmdEnv
	if data, err := os.ReadFile(filepath.Join(filepath.Dir(src), name+".in")); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		writeErr(outDir, name, fmt.Errorf("run: %v\n%s", err, out))
		return nil, err
	}
	os.Remove(filepath.Join(outDir, name+".error"))
	outPath := filepath.Join(outDir, name+".out")
	benchPath := filepath.Join(outDir, name+".bench")
	if bench {
		outBytes := got
		if idx := bytes.LastIndexByte(outBytes, '{'); idx >= 0 {
			outBytes = outBytes[idx:]
		}
		_ = os.WriteFile(benchPath, outBytes, 0o644)
		var js struct {
			Duration int64  `json:"duration_us"`
			Memory   int64  `json:"memory_bytes"`
			Name     string `json:"name"`
		}
		_ = json.Unmarshal(outBytes, &js)
		return got, nil
	}
	if *updateRosetta {
		if err := os.WriteFile(outPath, append(got, '\n'), 0o644); err != nil {
			return nil, err
		}
	} else if want, err := os.ReadFile(outPath); err == nil {
		if !bytes.Equal(got, bytes.TrimSpace(want)) {
			return got, fmt.Errorf("output mismatch for %s", name)
		}
	}
	return got, nil
}

func writeErr(dir, name string, err error) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(err.Error()), 0o644)
}

func rosettaRepoRoot(t *testing.T) string {
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
	t.Fatal("go.mod not found")
	return ""
}

func updateRosettaReadme() {
	root := rosettaRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Zig")
	readme := filepath.Join(root, "transpiler", "x", "zig", "ROSETTA.md")

	names, err := readIndex(srcDir)
	if err != nil {
		return
	}
	total := len(names)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for i, name := range names {
		base := strings.TrimSuffix(name, ".mochi")
		status := " "
		if _, err := os.Stat(filepath.Join(outDir, base+".error")); err == nil {
			// leave unchecked
		} else if _, err := os.Stat(filepath.Join(outDir, base+".zig")); err == nil {
			compiled++
			status = "✓"
		}
		dur := ""
		mem := ""
		benchFile := filepath.Join(outDir, base+".bench")
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
		} else if data, err := os.ReadFile(filepath.Join(outDir, base+".out")); err == nil {
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
		lines = append(lines, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, base, status, dur, mem))
	}
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
	var buf bytes.Buffer
	buf.WriteString("# Zig Rosetta Transpiler\n\n")
	buf.WriteString("Generated Zig code for Rosetta tasks lives under `tests/rosetta/transpiler/Zig`.\n\n")
	if ts != "" {
		fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	}
	fmt.Fprintf(&buf, "## Program Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteByte('\n')
	_ = os.WriteFile(readme, buf.Bytes(), 0o644)
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
