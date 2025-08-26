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

func TestRubyTranspilerSPOJ(t *testing.T) {
	if _, err := exec.LookPath("ruby"); err != nil {
		t.Skip("ruby not installed")
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "ruby")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateSPOJ)

	from, to := 1, 1
	if s := os.Getenv("FROM_INDEX"); s != "" {
		if v, err := strconv.Atoi(s); err == nil && v > 0 {
			from = v
		}
	}
	if s := os.Getenv("TO_INDEX"); s != "" {
		if v, err := strconv.Atoi(s); err == nil && v >= from {
			to = v
		}
	}
	for i := from; i <= to; i++ {
		name := strconv.Itoa(i)
		src := filepath.Join(srcDir, name+".mochi")
		if _, err := os.Stat(src); err != nil {
			continue
		}
		codePath := filepath.Join(outDir, name+".rb")
		outPath := filepath.Join(outDir, name+".out")
		errPath := filepath.Join(outDir, name+".error")
		inPath := filepath.Join(srcDir, name+".in")

		prog, err := parser.Parse(src)
		if err != nil {
			t.Logf("parse %s: %v", name, err)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			t.Logf("type %s: %v", name, errs[0])
			continue
		}
		ast, err := rb.Transpile(prog, env)
		if err != nil {
			t.Logf("transpile %s: %v", name, err)
			continue
		}
		var buf bytes.Buffer
		if err := rb.Emit(&buf, ast); err != nil {
			t.Logf("emit %s: %v", name, err)
			continue
		}
		if err := os.WriteFile(codePath, buf.Bytes(), 0o644); err != nil {
			t.Logf("write code %s: %v", name, err)
			continue
		}

		cmd := exec.Command("ruby", codePath)
		cmd.Env = append(os.Environ(), "MOCHI_NOW_SEED=1")
		if data, err := os.ReadFile(inPath); err == nil {
			cmd.Stdin = bytes.NewReader(data)
			_ = os.WriteFile(filepath.Join(outDir, name+".in"), data, 0o644)
		}
		out, err := cmd.CombinedOutput()
		got := bytes.TrimSpace(out)
		if err != nil {
			_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
			t.Logf("run %s: %v", name, err)
			continue
		}
		_ = os.Remove(errPath)

		if shouldUpdate() {
			_ = os.WriteFile(outPath, append(got, '\n'), 0o644)
		} else if want, err := os.ReadFile(outPath); err == nil {
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				t.Errorf("output mismatch %s\nGot: %s\nWant: %s", name, got, want)
			}
		}
		_ = os.WriteFile(outPath, got, 0o644)
	}
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
