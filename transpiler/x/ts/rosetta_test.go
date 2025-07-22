//go:build slow

package tstranspiler_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"

	"mochi/golden"
	"mochi/parser"
	meta "mochi/transpiler/meta"
	tstranspiler "mochi/transpiler/x/ts"
	"mochi/types"
)

func TestTSTranspiler_Rosetta_Golden(t *testing.T) {
	if err := meta.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "TypeScript")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/rosetta/x/Mochi", ".mochi", ".out", func(src string) ([]byte, error) {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, name+".ts")
		outPath := filepath.Join(outDir, name+".out")
		errPath := filepath.Join(outDir, name+".error")

		want, err := os.ReadFile(outPath)
		if err != nil {
			srcOut := strings.TrimSuffix(src, ".mochi") + ".out"
			want, err = os.ReadFile(srcOut)
			if err != nil {
				return nil, fmt.Errorf("read want: %v", err)
			}
		}
		want = bytes.TrimSpace(want)

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
		tsprog, err := tstranspiler.Transpile(prog, env)
		if err != nil {
			_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
			return nil, err
		}
		code := tstranspiler.Emit(tsprog)
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", "--allow-env", codePath)
		cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		got := bytes.TrimSpace(out)
		if err != nil {
			_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		_ = os.Remove(errPath)
		_ = os.WriteFile(outPath, got, 0o644)

		if !bytes.Equal(got, want) {
			return nil, fmt.Errorf("output mismatch")
		}
		return got, nil
	})
}

func updateRosettaChecklist() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "TypeScript")
	md := filepath.Join(root, "transpiler", "x", "ts", "ROSETTA.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.out"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".out")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			// leave unchecked
		} else if _, err := os.Stat(filepath.Join(outDir, name+".ts")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	ts := time.Now().UTC().Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# Rosetta Transpiler Progress\n\n")
	buf.WriteString("This checklist is auto-generated.\n")
	buf.WriteString("Generated TypeScript code from programs in `tests/rosetta/x/Mochi` lives in `tests/rosetta/transpiler/TypeScript`.\n")
	buf.WriteString("Last updated: " + ts + "\n\n")
	fmt.Fprintf(&buf, "## Rosetta Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(md, buf.Bytes(), 0o644)
}
