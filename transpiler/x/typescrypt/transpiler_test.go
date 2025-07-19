//go:build slow

package typescrypttranspiler_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
	meta "mochi/transpiler/meta"
    typescrypttranspiler "mochi/transpiler/x/typescrypt"
	"mochi/types"
)

// TestTranspilePrintHello compiles the simple print_hello.mochi program to
// TypeScript, runs the result with Deno and compares the runtime output with the
// existing golden .out file. The generated TypeScript itself is written to
// tests/transpiler/x/typescrypt but not compared.
func TestTranspilePrintHello(t *testing.T) {
	if err := meta.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := meta.RepoRoot()
	src := filepath.Join(root, "tests", "vm", "valid", "print_hello.mochi")
    outDir := filepath.Join(root, "tests", "transpiler", "x", "typescrypt")
	os.MkdirAll(outDir, 0o755)

	base := "print_hello"

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("parse: "+err.Error()), 0o644)
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("type: "+errs[0].Error()), 0o644)
		t.Fatalf("type error: %v", errs[0])
	}

    tsProg, err := typescrypttranspiler.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile error: %v", err)
	}

    code := typescrypttranspiler.Emit(tsProg)

    tsPath := filepath.Join(outDir, base+".ts")
	if err := os.WriteFile(tsPath, code, 0o644); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", tsPath)
	cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("run: "+err.Error()+"\n"+buf.String()), 0o644)
		t.Fatalf("deno run: %v\n%s", err, buf.Bytes())
	}
	got := strings.TrimSpace(buf.String())
	wantBytes, err := os.ReadFile(filepath.Join(outDir, base+".out"))
	if err != nil {
		t.Fatal(err)
	}
	want := strings.TrimSpace(string(wantBytes))
	if got != want {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("output mismatch\n-- got --\n"+got+"\n-- want --\n"+want), 0o644)
		t.Fatalf("unexpected output: got %q want %q", got, want)
	}
	if err := os.WriteFile(filepath.Join(outDir, base+".out"), []byte(got+"\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	_ = os.Remove(filepath.Join(outDir, base+".error"))

}

func TestTranspileIfElse(t *testing.T) {
	if err := meta.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := meta.RepoRoot()
	src := filepath.Join(root, "tests", "vm", "valid", "if_else.mochi")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "typescrypt")
	os.MkdirAll(outDir, 0o755)

	base := "if_else"

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("parse: "+err.Error()), 0o644)
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("type: "+errs[0].Error()), 0o644)
		t.Fatalf("type error: %v", errs[0])
	}

	tsProg, err := typescrypttranspiler.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile error: %v", err)
	}

	code := typescrypttranspiler.Emit(tsProg)

	tsPath := filepath.Join(outDir, base+".ts")
	if err := os.WriteFile(tsPath, code, 0o644); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", tsPath)
	cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("run: "+err.Error()+"\n"+buf.String()), 0o644)
		t.Fatalf("deno run: %v\n%s", err, buf.Bytes())
	}
	got := strings.TrimSpace(buf.String())
	wantBytes, err := os.ReadFile(filepath.Join(outDir, base+".out"))
	if err != nil {
		t.Fatal(err)
	}
	want := strings.TrimSpace(string(wantBytes))
	if got != want {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("output mismatch\n-- got --\n"+got+"\n-- want --\n"+want), 0o644)
		t.Fatalf("unexpected output: got %q want %q", got, want)
	}
	if err := os.WriteFile(filepath.Join(outDir, base+".out"), []byte(got+"\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	_ = os.Remove(filepath.Join(outDir, base+".error"))
}

func TestTranspileForLoop(t *testing.T) {
	if err := meta.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := meta.RepoRoot()
	src := filepath.Join(root, "tests", "vm", "valid", "for_loop.mochi")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "typescrypt")
	os.MkdirAll(outDir, 0o755)

	base := "for_loop"

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("parse: "+err.Error()), 0o644)
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("type: "+errs[0].Error()), 0o644)
		t.Fatalf("type error: %v", errs[0])
	}

	tsProg, err := typescrypttranspiler.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile error: %v", err)
	}

	code := typescrypttranspiler.Emit(tsProg)

	tsPath := filepath.Join(outDir, base+".ts")
	if err := os.WriteFile(tsPath, code, 0o644); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", tsPath)
	cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("run: "+err.Error()+"\n"+buf.String()), 0o644)
		t.Fatalf("deno run: %v\n%s", err, buf.Bytes())
	}
	got := strings.TrimSpace(buf.String())
	wantBytes, err := os.ReadFile(filepath.Join(outDir, base+".out"))
	if err != nil {
		t.Fatal(err)
	}
	want := strings.TrimSpace(string(wantBytes))
	if got != want {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("output mismatch\n-- got --\n"+got+"\n-- want --\n"+want), 0o644)
		t.Fatalf("unexpected output: got %q want %q", got, want)
	}
	if err := os.WriteFile(filepath.Join(outDir, base+".out"), []byte(got+"\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	_ = os.Remove(filepath.Join(outDir, base+".error"))
}

func TestTranspileWhileLoop(t *testing.T) {
	if err := meta.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := meta.RepoRoot()
	src := filepath.Join(root, "tests", "vm", "valid", "while_loop.mochi")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "typescrypt")
	os.MkdirAll(outDir, 0o755)

	base := "while_loop"

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("parse: "+err.Error()), 0o644)
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("type: "+errs[0].Error()), 0o644)
		t.Fatalf("type error: %v", errs[0])
	}

	tsProg, err := typescrypttranspiler.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile error: %v", err)
	}

	code := typescrypttranspiler.Emit(tsProg)

	tsPath := filepath.Join(outDir, base+".ts")
	if err := os.WriteFile(tsPath, code, 0o644); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", tsPath)
	cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("run: "+err.Error()+"\n"+buf.String()), 0o644)
		t.Fatalf("deno run: %v\n%s", err, buf.Bytes())
	}
	got := strings.TrimSpace(buf.String())
	wantBytes, err := os.ReadFile(filepath.Join(outDir, base+".out"))
	if err != nil {
		t.Fatal(err)
	}
	want := strings.TrimSpace(string(wantBytes))
	if got != want {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("output mismatch\n-- got --\n"+got+"\n-- want --\n"+want), 0o644)
		t.Fatalf("unexpected output: got %q want %q", got, want)
	}
	if err := os.WriteFile(filepath.Join(outDir, base+".out"), []byte(got+"\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	_ = os.Remove(filepath.Join(outDir, base+".error"))
}

func TestTranspileVarAssignment(t *testing.T) {
	if err := meta.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := meta.RepoRoot()
	src := filepath.Join(root, "tests", "vm", "valid", "var_assignment.mochi")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "typescrypt")
	os.MkdirAll(outDir, 0o755)

	base := "var_assignment"

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("parse: "+err.Error()), 0o644)
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("type: "+errs[0].Error()), 0o644)
		t.Fatalf("type error: %v", errs[0])
	}

	tsProg, err := typescrypttranspiler.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile error: %v", err)
	}

	code := typescrypttranspiler.Emit(tsProg)

	tsPath := filepath.Join(outDir, base+".ts")
	if err := os.WriteFile(tsPath, code, 0o644); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", tsPath)
	cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("run: "+err.Error()+"\n"+buf.String()), 0o644)
		t.Fatalf("deno run: %v\n%s", err, buf.Bytes())
	}
	got := strings.TrimSpace(buf.String())
	wantBytes, err := os.ReadFile(filepath.Join(outDir, base+".out"))
	if err != nil {
		t.Fatal(err)
	}
	want := strings.TrimSpace(string(wantBytes))
	if got != want {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("output mismatch\n-- got --\n"+got+"\n-- want --\n"+want), 0o644)
		t.Fatalf("unexpected output: got %q want %q", got, want)
	}
	if err := os.WriteFile(filepath.Join(outDir, base+".out"), []byte(got+"\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	_ = os.Remove(filepath.Join(outDir, base+".error"))
}

func TestTranspileTypedLet(t *testing.T) {
	if err := meta.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := meta.RepoRoot()
	src := filepath.Join(root, "tests", "vm", "valid", "typed_let.mochi")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "typescrypt")
	os.MkdirAll(outDir, 0o755)

	base := "typed_let"

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("parse: "+err.Error()), 0o644)
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("type: "+errs[0].Error()), 0o644)
		t.Fatalf("type error: %v", errs[0])
	}

	tsProg, err := typescrypttranspiler.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile error: %v", err)
	}

	code := typescrypttranspiler.Emit(tsProg)

	tsPath := filepath.Join(outDir, base+".ts")
	if err := os.WriteFile(tsPath, code, 0o644); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", tsPath)
	cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("run: "+err.Error()+"\n"+buf.String()), 0o644)
		t.Fatalf("deno run: %v\n%s", err, buf.Bytes())
	}
	got := strings.TrimSpace(buf.String())
	wantBytes, err := os.ReadFile(filepath.Join(outDir, base+".out"))
	if err != nil {
		t.Fatal(err)
	}
	want := strings.TrimSpace(string(wantBytes))
	if got != want {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("output mismatch\n-- got --\n"+got+"\n-- want --\n"+want), 0o644)
		t.Fatalf("unexpected output: got %q want %q", got, want)
	}
	if err := os.WriteFile(filepath.Join(outDir, base+".out"), []byte(got+"\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	_ = os.Remove(filepath.Join(outDir, base+".error"))
}

func TestTranspileTypedVar(t *testing.T) {
	if err := meta.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := meta.RepoRoot()
	src := filepath.Join(root, "tests", "vm", "valid", "typed_var.mochi")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "typescrypt")
	os.MkdirAll(outDir, 0o755)

	base := "typed_var"

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("parse: "+err.Error()), 0o644)
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("type: "+errs[0].Error()), 0o644)
		t.Fatalf("type error: %v", errs[0])
	}

	tsProg, err := typescrypttranspiler.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile error: %v", err)
	}

	code := typescrypttranspiler.Emit(tsProg)

	tsPath := filepath.Join(outDir, base+".ts")
	if err := os.WriteFile(tsPath, code, 0o644); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", tsPath)
	cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("run: "+err.Error()+"\n"+buf.String()), 0o644)
		t.Fatalf("deno run: %v\n%s", err, buf.Bytes())
	}
	got := strings.TrimSpace(buf.String())
	wantBytes, err := os.ReadFile(filepath.Join(outDir, base+".out"))
	if err != nil {
		t.Fatal(err)
	}
	want := strings.TrimSpace(string(wantBytes))
	if got != want {
		_ = os.WriteFile(filepath.Join(outDir, base+".error"), []byte("output mismatch\n-- got --\n"+got+"\n-- want --\n"+want), 0o644)
		t.Fatalf("unexpected output: got %q want %q", got, want)
	}
	if err := os.WriteFile(filepath.Join(outDir, base+".out"), []byte(got+"\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	_ = os.Remove(filepath.Join(outDir, base+".error"))
}
