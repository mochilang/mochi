//go:build slow

package php_test

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
	php "mochi/transpiler/x/php"
	"mochi/types"
)

func TestPHPTranspiler_Rosetta_Golden(t *testing.T) {
	if _, err := exec.LookPath("php"); err != nil {
		t.Skip("php not installed")
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "php")
	os.MkdirAll(outDir, 0o755)
	defer updateRosettaReadme()

	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Strings(files)

	if s := os.Getenv("ROSETTA_INDEX"); s != "" {
		idx, err := strconv.Atoi(s)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid ROSETTA_INDEX: %s", s)
		}
		files = files[idx-1 : idx]
	} else {
		limit := len(files)
		if s := os.Getenv("ROSETTA_LIMIT"); s != "" {
			if n, err := strconv.Atoi(s); err == nil && n < limit {
				limit = n
			}
		}
		files = files[:limit]
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		if err := runRosettaPHP(src, outDir); err != nil {
			t.Fatalf("%s: %v", name, err)
		}
	}
}

func runRosettaPHP(src string, outDir string) error {
	name := strings.TrimSuffix(filepath.Base(src), ".mochi")
	codePath := filepath.Join(outDir, name+".php")
	outPath := filepath.Join(outDir, name+".out")
	errPath := filepath.Join(outDir, name+".error")

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
		return fmt.Errorf("parse: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
		return fmt.Errorf("type: %w", errs[0])
	}
	ast, err := php.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
		return fmt.Errorf("transpile: %w", err)
	}
	var buf bytes.Buffer
	if err := php.Emit(&buf, ast); err != nil {
		_ = os.WriteFile(errPath, []byte("emit: "+err.Error()), 0o644)
		return fmt.Errorf("emit: %w", err)
	}
	if err := os.WriteFile(codePath, buf.Bytes(), 0o644); err != nil {
		return fmt.Errorf("write code: %w", err)
	}
	cmd := exec.Command("php", codePath)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
		return fmt.Errorf("run: %w", err)
	}
	_ = os.Remove(errPath)
	_ = os.WriteFile(outPath, got, 0o644)
	if want, err := os.ReadFile(outPath); err == nil {
		want = bytes.TrimSpace(want)
		if !bytes.Equal(got, want) {
			return fmt.Errorf("output mismatch: got %s want %s", got, want)
		}
	}
	return nil
}

func updateRosettaReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "php")
	readmePath := filepath.Join(root, "transpiler", "x", "php", "ROSETTA.md")

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

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		phpFile := filepath.Join(outDir, name+".php")
		outFile := filepath.Join(outDir, name+".out")
		errFile := filepath.Join(outDir, name+".error")
		if exists(phpFile) && exists(outFile) && !exists(errFile) {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}

	var buf bytes.Buffer
	buf.WriteString("# PHP Rosetta Transpiler Output\n\n")
	buf.WriteString("Generated PHP code from Mochi Rosetta tasks lives in `tests/rosetta/transpiler/php`.\n\n")
	if ts != "" {
		fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	}
	fmt.Fprintf(&buf, "## Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func exists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}
