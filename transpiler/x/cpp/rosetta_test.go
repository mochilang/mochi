//go:build slow

package cpp_test

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
	cpp "mochi/transpiler/x/cpp"
	"mochi/types"
)

var update = flag.Bool("update-rosetta-cpp", false, "update rosetta golden files")

func updateEnabled() bool { return *update }

func readIndex(dir string) ([]string, error) {
	path := filepath.Join(dir, "index.txt")
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	var names []string
	scanner := bufio.NewScanner(f)
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

func TestCPPTranspiler_Rosetta_Golden(t *testing.T) {
	if _, err := exec.LookPath("g++"); err != nil {
		t.Skip("g++ not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "CPP")
	os.MkdirAll(outDir, 0o755)
	bench := os.Getenv("MOCHI_BENCHMARK") != ""

	names, err := readIndex(filepath.Join(root, "tests", "rosetta", "x", "Mochi"))
	if err != nil {
		t.Fatalf("read index: %v", err)
	}
	files := make([]string, len(names))
	for i, name := range names {
		files[i] = filepath.Join(root, "tests", "rosetta", "x", "Mochi", name)
	}

	startIdx := 1
	if v := os.Getenv("ROSETTA_INDEX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n > 0 && n <= len(files) {
			startIdx = n
			files = files[n-1 : n]
		} else {
			t.Fatalf("invalid ROSETTA_INDEX %s", v)
		}
	} else if v := os.Getenv("MOCHI_ROSETTA_INDEX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n > 0 && n <= len(files) {
			startIdx = n
			files = files[n-1 : n]
		} else {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX %s", v)
		}
	}
	t.Cleanup(updateRosettaReadme)
	var firstErr string
	runOne := func(src string) error {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".cpp")
		outPath := filepath.Join(outDir, base+".out")
		benchPath := filepath.Join(outDir, base+".bench")
		errPath := filepath.Join(outDir, base+".error")
		want, err := os.ReadFile(outPath)
		if err != nil && !updateEnabled() && !bench {
			return fmt.Errorf("missing golden output: %v", err)
		}
		if !bench {
			want = bytes.TrimSpace(want)
		}

		prog, err := parser.Parse(src)
		if err != nil {
			_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
			return err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
			return errs[0]
		}
		cpp.SetBenchMain(bench)
		ast, err := cpp.Transpile(prog, env)
		if err != nil {
			_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
			return err
		}
		code := ast.Emit()
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return err
		}
		bin := filepath.Join(outDir, base)
		args := []string{codePath, "-std=c++20"}
		if ast.UseSHA256 || ast.UseMD5 {
			args = append(args, "-lcrypto")
		}
		args = append(args, "-o", bin)
		if out, err := exec.Command("g++", args...).CombinedOutput(); err != nil {
			_ = os.WriteFile(errPath, append([]byte("compile: "+err.Error()+"\n"), out...), 0o644)
			return err
		}
		defer os.Remove(bin)
		cmd := exec.Command(bin)
		envv := os.Environ()
		if bench {
			envv = append(envv, "MOCHI_BENCHMARK=1")
		}
		envv = append(envv, "MOCHI_NOW_SEED=1")
		cmd.Env = envv
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		got := bytes.TrimSpace(out)
		if err != nil {
			_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
			if !updateEnabled() {
				return err
			}
		} else {
			_ = os.Remove(errPath)
		}
		if bench {
			if updateEnabled() {
				outBytes := got
				if idx := bytes.LastIndexByte(outBytes, '{'); idx >= 0 {
					outBytes = outBytes[idx:]
				}
				_ = os.WriteFile(benchPath, outBytes, 0o644)
			}
			return nil
		}
		if updateEnabled() {
			_ = os.WriteFile(outPath, got, 0o644)
			return nil
		}
		if !bytes.Equal(got, want) {
			return fmt.Errorf("output mismatch")
		}
		return nil
	}

	for i, src := range files {
		idx := startIdx + i
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		tname := fmt.Sprintf("%03d_%s", idx, name)
		ok := t.Run(tname, func(t *testing.T) {
			if err := runOne(src); err != nil {
				firstErr = name
				t.Fail()
			}
		})
		if !ok {
			break
		}
	}
	if firstErr != "" {
		t.Fatalf("failed program: %s", firstErr)
	}
}

func updateRosettaReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "CPP")
	readmePath := filepath.Join(root, "transpiler", "x", "cpp", "ROSETTA.md")

	names, err := readIndex(srcDir)
	if err != nil {
		return
	}
	total := len(names)
	compiled := 0
	var rows []string
	rows = append(rows, "| Index | Name | Status | Duration | Memory |")
	rows = append(rows, "| ---: | --- | :---: | ---: | ---: |")
	for i, f := range names {
		name := strings.TrimSuffix(f, ".mochi")
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			// leave unchecked
		} else if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, name+".bench")); err == nil {
			var r struct {
				DurationUS  int64 `json:"duration_us"`
				MemoryBytes int64 `json:"memory_bytes"`
			}
			trimmed := bytes.TrimSpace(data)
			if idx := bytes.LastIndex(trimmed, []byte("{")); idx >= 0 {
				trimmed = trimmed[idx:]
			}
			if json.Unmarshal(trimmed, &r) == nil {
				dur = formatDuration(time.Duration(r.DurationUS) * time.Microsecond)
				mem = formatBytes(r.MemoryBytes)
			}
		}
		rows = append(rows, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, status, dur, mem))
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
	buf.WriteString("# C++ Transpiler Rosetta Output\n\n")
	buf.WriteString("This directory stores C++ code generated from Mochi programs in `tests/rosetta/x/Mochi`. Each file is compiled and executed during tests. Successful runs keep the generated `.cpp` source along with a matching `.out` file. Failures are recorded in `.error` files when tests run with `-update`.\n\n")
	fmt.Fprintf(&buf, "Checklist of programs that currently transpile and run (%d/%d) - Last updated %s:\n", compiled, total, ts)
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func formatDuration(d time.Duration) string {
	switch {
	case d < time.Microsecond:
		return fmt.Sprintf("%dns", d.Nanoseconds())
	case d < time.Millisecond:
		return fmt.Sprintf("%.1fµs", float64(d.Microseconds()))
	case d < time.Second:
		return fmt.Sprintf("%.1fms", float64(d.Milliseconds()))
	default:
		return fmt.Sprintf("%.2fs", d.Seconds())
	}
}

func formatBytes(n int64) string {
	const (
		_KB = 1024
		_MB = _KB * 1024
		_GB = _MB * 1024
	)
	switch {
	case n >= _GB:
		return fmt.Sprintf("%.2fGB", float64(n)/float64(_GB))
	case n >= _MB:
		return fmt.Sprintf("%.2fMB", float64(n)/float64(_MB))
	case n >= _KB:
		return fmt.Sprintf("%.2fKB", float64(n)/float64(_KB))
	default:
		return fmt.Sprintf("%dB", n)
	}
}
