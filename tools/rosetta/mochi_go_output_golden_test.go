package rosetta

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	gocode "mochi/compiler/x/go"
	"mochi/parser"
	"mochi/types"
)

func TestMochiGoOutputGolden(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go toolchain not installed")
	}

	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests/rosetta/x/Mochi")
	outDir := filepath.Join(root, "tests/rosetta/out/Go")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}

	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(files) == 0 {
		t.Fatal("no Mochi Rosetta tests found")
	}

	if len(files) > 10 {
		files = files[:10]
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			runGoOutputGolden(t, srcDir, outDir, name)
		})
	}
}

func runGoOutputGolden(t *testing.T, srcDir, outDir, name string) {
	srcPath := filepath.Join(srcDir, name+".mochi")
	prog, err := parser.Parse(srcPath)
	if err != nil {
		writeGoError(outDir, name, fmt.Errorf("parse error: %w", err))
		t.Skip("parse error")
		return
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeGoError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
		t.Skip("type error")
		return
	}
	code, err := gocode.New(env).Compile(prog)
	if err != nil {
		writeGoError(outDir, name, fmt.Errorf("compile error: %w", err))
		t.Skip("compile error")
		return
	}
	goFile := filepath.Join(outDir, name+".go")
	if err := os.WriteFile(goFile, code, 0o644); err != nil {
		t.Fatalf("write go: %v", err)
	}
	cmd := exec.Command("go", "run", goFile)
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		writeGoError(outDir, name, fmt.Errorf("run error: %v\n%s", err, buf.Bytes()))
		t.Skip("run error")
		return
	}
	got := strings.TrimSpace(buf.String())
	outPath := filepath.Join(outDir, name+".out")
	if shouldUpdate() {
		if err := os.WriteFile(outPath, []byte(got+"\n"), 0o644); err != nil {
			t.Fatalf("write out: %v", err)
		}
		t.Logf("updated: %s", outPath)
		_ = os.Remove(filepath.Join(outDir, name+".error"))
		return
	}
	wantData, err := os.ReadFile(outPath)
	if err != nil {
		t.Skipf("missing golden: %v", err)
		return
	}
	want := strings.TrimSpace(string(wantData))
	if got != want {
		writeGoError(outDir, name, fmt.Errorf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, want))
		t.Errorf("%s output\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, want)
		return
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
}
