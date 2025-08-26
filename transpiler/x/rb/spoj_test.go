//go:build slow

package rb_test

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

	"mochi/parser"
	rb "mochi/transpiler/x/rb"
	"mochi/types"
)

func TestRubyTranspilerSPOJ(t *testing.T) {
	if _, err := exec.LookPath("ruby"); err != nil {
		t.Skip("ruby not installed")
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "ruby")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateSPOJ)

	src := filepath.Join(srcDir, "1.mochi")
	name := "1"
	codePath := filepath.Join(outDir, name+".rb")
	outPath := filepath.Join(outDir, name+".out")
	errPath := filepath.Join(outDir, name+".error")
	inPath := filepath.Join(outDir, name+".in")

	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := rb.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := rb.Emit(&buf, ast); err != nil {
		t.Fatalf("emit: %v", err)
	}
	if err := os.WriteFile(codePath, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write code: %v", err)
	}

	cmd := exec.Command("ruby", codePath)
	cmd.Env = append(os.Environ(), "MOCHI_NOW_SEED=1")
	if data, err := os.ReadFile(inPath); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(errPath)

	if shouldUpdate() {
		_ = os.WriteFile(outPath, append(got, '\n'), 0o644)
	} else if want, err := os.ReadFile(outPath); err == nil {
		want = bytes.TrimSpace(want)
		if !bytes.Equal(got, want) {
			t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
		}
	}
	_ = os.WriteFile(outPath, got, 0o644)
}
func updateSPOJ() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "ruby")
	readmePath := filepath.Join(root, "transpiler", "x", "rb", "SPOJ.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for _, f := range files {
		base := strings.TrimSuffix(filepath.Base(f), ".mochi")
		name := problemName(srcDir, base)
		status := " "
		if exists(filepath.Join(outDir, base+".rb")) && exists(filepath.Join(outDir, base+".out")) && !exists(filepath.Join(outDir, base+".error")) {
			status = "✓"
			compiled++
		}
		lines = append(lines, fmt.Sprintf("| %s | %s | %s |  |  |", base, name, status))
	}
	ts := time.Now().Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# Ruby SPOJ Transpiler Output\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	buf.WriteString("Generated Ruby code from Mochi SPOJ tasks lives in `tests/spoj/x/ruby`.\n\n")
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	buf.WriteString("Checklist:\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func problemName(dir, base string) string {
	mdPath := filepath.Join(dir, base+".md")
	data, err := os.ReadFile(mdPath)
	if err != nil {
		return base
	}
	if idx := bytes.Index(data, []byte("/problems/")); idx >= 0 {
		start := idx + len("/problems/")
		end := start
		for end < len(data) && data[end] != ')' && data[end] != '\n' {
			end++
		}
		slug := strings.TrimSpace(string(data[start:end]))
		slug = strings.TrimRight(slug, "/")
		return strings.ToLower(slug)
	}
	return base
}
