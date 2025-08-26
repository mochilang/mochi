//go:build slow

package dartt_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	dartt "mochi/transpiler/x/dart"
	"mochi/types"
)

var updateSpojFlag = flag.Bool("update-spoj-dart", false, "update golden files")

func spojUpdateEnabled() bool { return *updateSpojFlag }

func runSpojCase(t *testing.T, idx string) {
	t.Helper()
	ensureDart(t)
	root := repoRoot(t)

	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "dart")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(srcDir, idx+".mochi")
	codePath := filepath.Join(outDir, idx+".dart")
	inPath := filepath.Join(outDir, idx+".in")
	outPath := filepath.Join(outDir, idx+".out")
	errPath := filepath.Join(outDir, idx+".error")

	if data, err := os.ReadFile(filepath.Join(srcDir, idx+".in")); err == nil {
		_ = os.WriteFile(inPath, data, 0o644)
	}
	if data, err := os.ReadFile(filepath.Join(srcDir, idx+".out")); err == nil {
		_ = os.WriteFile(outPath, data, 0o644)
	}

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := dartt.Transpile(prog, env, false, false)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := dartt.Emit(&buf, ast); err != nil {
		_ = os.WriteFile(errPath, []byte("emit: "+err.Error()), 0o644)
		t.Fatalf("emit: %v", err)
	}
	if err := os.WriteFile(codePath, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write code: %v", err)
	}

	cmd := exec.Command("dart", codePath)
	if data, err := os.ReadFile(inPath); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(errPath)

	want, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)

	if spojUpdateEnabled() {
		_ = os.WriteFile(outPath, got, 0o644)
		return
	}

	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", idx, got, want)
	}
}

func TestDartTranspiler_Spoj_Golden(t *testing.T) {
	t.Cleanup(updateSpoj)
	runSpojCase(t, "1")
}

func updateSpoj() {
	if !spojUpdateEnabled() {
		return
	}
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "dart")
	mdPath := filepath.Join(root, "transpiler", "x", "dart", "SPOJ.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var rows []string
	rows = append(rows, "| Index | Name | Status | Duration | Memory |")
	rows = append(rows, "|------:|------|:-----:|---------:|-------:|")
	for _, f := range files {
		base := strings.TrimSuffix(filepath.Base(f), ".mochi")
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, base+".error")); err == nil {
			status = "error"
		} else if _, err := os.Stat(filepath.Join(outDir, base+".dart")); err == nil {
			status = "✓"
			compiled++
		}
		rows = append(rows, fmt.Sprintf("| %s | %s | %s | %s | %s |", base, base, status, dur, mem))
	}
	var buf bytes.Buffer
	buf.WriteString("# SPOJ Transpiler Progress\n\n")
	buf.WriteString("This checklist is auto-generated.\n")
	buf.WriteString("Generated Dart code from programs in `tests/spoj/x/mochi` lives in `tests/spoj/x/dart`.\n")
	loc := time.FixedZone("GMT+7", 7*60*60)
	buf.WriteString("Last updated: " + time.Now().In(loc).Format("2006-01-02 15:04 MST") + "\n\n")
	fmt.Fprintf(&buf, "## SPOJ Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(mdPath, buf.Bytes(), 0o644)
}
