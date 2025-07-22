//go:build slow

package scalat_test

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

	"mochi/compiler/x/testutil"
	"mochi/parser"
	scalat "mochi/transpiler/x/scala"
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

func TestScalaTranspiler_Rosetta_Golden(t *testing.T) {
	ensureScala(t)
	root := testutil.FindRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Scala")
	os.MkdirAll(outDir, 0o755)

	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		t.Fatalf("read index: %v", err)
	}
	if len(names) == 0 {
		t.Fatalf("no programs found in index")
	}

	start := 0
	if v := os.Getenv("MOCHI_ROSETTA_INDEX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n >= 1 && n <= len(names) {
			start = n - 1
			names = names[start : start+1]
		}
	} else {
		names = names[:1]
	}

	var passed, failed int
	for i, nameFile := range names {
		name := strings.TrimSuffix(filepath.Base(nameFile), ".mochi")
		ok := t.Run(fmt.Sprintf("%03d_%s", start+i+1, name), func(t *testing.T) {
			codePath := filepath.Join(outDir, name+".scala")
			outPath := filepath.Join(outDir, name+".out")
			errPath := filepath.Join(outDir, name+".error")

			prog, err := parser.Parse(filepath.Join(srcDir, nameFile))
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("parse: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				_ = os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
				t.Fatalf("type: %v", errs[0])
			}
			ast, err := scalat.Transpile(prog, env)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("transpile: %v", err)
			}
			code := scalat.Emit(ast)
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write: %v", err)
			}
			tmp := t.TempDir()
			if out, err := exec.Command("scalac", "-d", tmp, codePath).CombinedOutput(); err != nil {
				_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
				t.Fatalf("compile: %v", err)
			}
			cmd := exec.Command("scala", "-cp", tmp, "Main")
			out, err := cmd.CombinedOutput()
			got := bytes.TrimSpace(out)
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
				t.Fatalf("run: %v", err)
			}
			_ = os.Remove(errPath)
			_ = os.WriteFile(outPath, got, 0o644)
			want, err := os.ReadFile(outPath)
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
			break
		}
	}
	t.Logf("Summary: %d passed, %d failed", passed, failed)
}

func updateRosettaChecklist() {
	root := repoRoot()
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Scala")
	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		return
	}
	total := len(names)
	completed := 0
	var lines []string
	for i, nameFile := range names {
		name := strings.TrimSuffix(filepath.Base(nameFile), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			completed++
			mark = "[x]"
		} else if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			mark = "[ ]"
		}
		lines = append(lines, fmt.Sprintf("%d. %s %s", i+1, mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Scala Transpiler Rosetta Output\n\n")
	buf.WriteString("Generated Scala code for Rosetta tasks in `tests/rosetta/x/Mochi`. Each program has a `.scala` file produced by the transpiler and a `.out` file with its runtime output. Compilation or execution errors are captured in `.error` files.\n\n")
	loc, _ := time.LoadLocation("Asia/Bangkok")
	ts := time.Now().In(loc).Format("2006-01-02 15:04 -0700")
	buf.WriteString(fmt.Sprintf("## Golden Test Checklist (%d/%d)\n", completed, total))
	buf.WriteString(fmt.Sprintf("_Last updated: %s_\n\n", ts))
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(filepath.Join(root, "transpiler", "x", "scala", "ROSETTA.md"), buf.Bytes(), 0o644)
}
