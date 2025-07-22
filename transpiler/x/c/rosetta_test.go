//go:build slow

package ctrans_test

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/golden"
	ctrans "mochi/transpiler/x/c"
)

func TestRosettaTranspilerGolden(t *testing.T) {
	if _, err := ctrans.EnsureCC(); err != nil {
		t.Skipf("C compiler not installed: %v", err)
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "C")
	os.MkdirAll(outDir, 0o755)

	// Allow running a specific Rosetta program by numeric index.
	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
		names, err := readIndex(filepath.Join(srcDir, "index.txt"))
		if err == nil {
			if idx, err2 := strconv.Atoi(idxStr); err2 == nil && idx >= 1 && idx <= len(names) {
				name := strings.TrimSuffix(filepath.Base(names[idx-1]), ".mochi")
				t.Setenv("MOCHI_ROSETTA_ONLY", name)
			}
		}
	}

	t.Cleanup(updateRosettaReadme)

	golden.RunFirstFailure(t, "tests/rosetta/x/Mochi", ".mochi", ".out", func(src string) ([]byte, error) {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, name+".c")
		errPath := filepath.Join(outDir, name+".error")

		code, err := transpileFile(src)
		if err != nil {
			_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		if updateEnabled() {
			norm := normalize(root, code)
			_ = os.WriteFile(codePath, norm, 0o644)
		}
		out, runErr := transpileAndRun(src)
		if runErr != nil {
			_ = os.WriteFile(errPath, []byte(runErr.Error()+"\n"), 0o644)
			return nil, runErr
		}
		_ = os.Remove(errPath)
		if updateEnabled() {
			trimmed := bytes.TrimSpace(out)
			_ = os.WriteFile(filepath.Join(outDir, name+".out"), trimmed, 0o644)
		}
		return bytes.TrimSpace(out), nil
	})
}

func updateRosettaReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "C")
	readmePath := filepath.Join(root, "transpiler", "x", "c", "ROSETTA.md")
	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		return
	}
	total := len(names)
	compiled := 0
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := ""
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
				ts = t.In(loc).Format("2006-01-02 15:04 -0700")
			} else {
				ts = t.Format("2006-01-02 15:04 -0700")
			}
		}
	}
	var lines []string
	for i, f := range names {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			mark = "[x]"
			compiled++
		}
		lines = append(lines, fmt.Sprintf("%d. %s %s", i+1, mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# C Transpiler Rosetta Output\n\n")
	buf.WriteString("This directory stores C code generated from Mochi programs in `tests/rosetta/x/Mochi`. Each file is compiled and executed during tests. Successful runs keep the generated `.c` source along with a matching `.out` file. Failures are recorded in `.error` files when tests run with `-update`.\n\n")
	buf.WriteString(fmt.Sprintf("Checklist of programs that currently transpile and run (%d/%d) - Last updated %s:\n", compiled, total, ts))
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
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
