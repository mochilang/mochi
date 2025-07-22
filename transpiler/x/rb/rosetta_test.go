//go:build slow

package rb_test

import (
	"bytes"
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
	rb "mochi/transpiler/x/rb"
	"mochi/types"
)

func runRosetta(t *testing.T, src, name, outDir string) {
	codePath := filepath.Join(outDir, name+".rb")
	outPath := filepath.Join(outDir, name+".out")
	errPath := filepath.Join(outDir, name+".error")

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
		t.Skipf("parse error: %v", err)
		return
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
		t.Skipf("type error: %v", errs[0])
		return
	}
	ast, err := rb.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
		t.Skipf("transpile error: %v", err)
		return
	}
	var buf bytes.Buffer
	if err := rb.Emit(&buf, ast); err != nil {
		_ = os.WriteFile(errPath, []byte("emit: "+err.Error()), 0o644)
		t.Skipf("emit error: %v", err)
		return
	}
	code := buf.Bytes()
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("ruby", codePath)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
		t.Skipf("run error: %v", err)
		return
	}
	_ = os.Remove(errPath)

	wantPath := filepath.Join(outDir, name+".out")
	if update := shouldUpdate(); update {
		_ = os.WriteFile(wantPath, append(got, '\n'), 0o644)
	} else if want, err := os.ReadFile(wantPath); err == nil {
		if !bytes.Equal(got, bytes.TrimSpace(want)) {
			t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, bytes.TrimSpace(want))
		}
	}
	_ = os.WriteFile(outPath, got, 0o644)
}

func shouldUpdate() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

func TestRubyTranspiler_Rosetta(t *testing.T) {
	if _, err := exec.LookPath("ruby"); err != nil {
		t.Skip("ruby not installed")
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "rb")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateRosetta)
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Strings(files)
	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		f := files[idx-1]
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		t.Run(fmt.Sprintf("%03d_%s", idx, name), func(t *testing.T) {
			runRosetta(t, f, name, outDir)
		})
		return
	}
	max := len(files)
	if v := os.Getenv("ROSETTA_MAX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n < max {
			max = n
		}
	}
	for i, f := range files[:max] {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		t.Run(fmt.Sprintf("%03d_%s", i+1, name), func(t *testing.T) {
			runRosetta(t, f, name, outDir)
		})
	}
}

func updateRosetta() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "rb")
	readmePath := filepath.Join(root, "transpiler", "x", "rb", "ROSETTA.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for i, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			if _, err2 := os.Stat(filepath.Join(outDir, name+".error")); os.IsNotExist(err2) {
				compiled++
				mark = "[x]"
			}
		}
		lines = append(lines, fmt.Sprintf("%3d. %s %s", i+1, mark, name))
	}
	ts := time.Now().Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "# Ruby Rosetta Transpiler Output (%d/%d)\n", compiled, total)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}
