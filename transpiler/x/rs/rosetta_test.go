//go:build slow

package rs_test

import (
	"bufio"
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"testing"
	"time"
	"unicode"

	"mochi/parser"
	rs "mochi/transpiler/x/rs"
	"mochi/types"
)

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

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

func runRosetta(t *testing.T, src, outDir string) ([]byte, error) {
	base := strings.TrimSuffix(filepath.Base(src), ".mochi")
	codePath := filepath.Join(outDir, base+".rs")
	errPath := filepath.Join(outDir, base+".error")

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
		return nil, fmt.Errorf("parse: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
		return nil, fmt.Errorf("type: %v", errs[0])
	}
	gprog, err := rs.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
		return nil, fmt.Errorf("transpile: %w", err)
	}
	code := rs.Emit(gprog)
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		return nil, err
	}
	tmpDir := t.TempDir()
	bin := filepath.Join(tmpDir, base)
	crate := strings.Map(func(r rune) rune {
		if unicode.IsLetter(r) || unicode.IsDigit(r) || r == '_' {
			return r
		}
		return '_'
	}, base)
	if out, err := exec.Command("rustc", codePath, "-O", "--crate-name", crate, "-o", bin).CombinedOutput(); err != nil {
		_ = os.WriteFile(errPath, out, 0o644)
		return nil, fmt.Errorf("rustc: %w", err)
	}
	cmd := exec.Command(bin)
	cmd.Env = append(os.Environ(), "MOCHI_NOW_SEED=1")
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(errPath, out, 0o644)
		return nil, fmt.Errorf("run: %w", err)
	}
	_ = os.Remove(errPath)
	return got, nil
}

func TestTranspiler_Rosetta(t *testing.T) {
	if _, err := exec.LookPath("rustc"); err != nil {
		t.Skip("rustc not installed")
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Rust")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateRosetta)
	names, err := readIndex(srcDir)
	if err != nil {
		t.Fatalf("read index: %v", err)
	}
	if only := os.Getenv("MOCHI_ROSETTA_ONLY"); only != "" {
		names = []string{only + ".mochi"}
	}
	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(names) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		names = names[idx-1 : idx]
	}
	if len(names) == 0 {
		t.Fatalf("no rosetta programs found")
	}
	var passed, failed int
	var firstFail string
	for _, name := range names {
		src := filepath.Join(srcDir, name)
		base := strings.TrimSuffix(name, ".mochi")
		ok := t.Run(base, func(t *testing.T) {
			got, err := runRosetta(t, src, outDir)
			if err != nil {
				t.Fatalf("%v", err)
			}
			outFile := filepath.Join(outDir, base+".out")
			if shouldUpdate() {
				_ = os.WriteFile(outFile, append(got, '\n'), 0o644)
				return
			}
			want, err := os.ReadFile(outFile)
			if err != nil {
				t.Fatalf("read want: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
			}
		})
		if ok {
			passed++
		} else {
			failed++
			firstFail = base
			break
		}
	}
	if firstFail != "" {
		t.Fatalf("first failing program: %s", firstFail)
	}
	t.Logf("Summary: %d passed, %d failed", passed, failed)
}

func updateRosetta() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Rust")
	readmePath := filepath.Join(root, "transpiler", "x", "rs", "ROSETTA.md")
	_ = updateIndex(srcDir)
	names, _ := readIndex(srcDir)
	total := len(names)
	compiled := 0
	var lines []string
	for i, f := range names {
		name := strings.TrimSuffix(f, ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			if _, err2 := os.Stat(filepath.Join(outDir, name+".error")); os.IsNotExist(err2) {
				compiled++
				mark = "[x]"
			}
		}
		lines = append(lines, fmt.Sprintf("%3d. %s %s (%d)", i+1, mark, name, i+1))
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
	fmt.Fprintf(&buf, "# Rosetta Rust Transpiler Output (%d/%d)\n", compiled, total)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	buf.WriteString("## Program checklist\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}
