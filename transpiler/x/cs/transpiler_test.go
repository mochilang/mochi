//go:build slow

package cstranspiler_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
	transpiler "mochi/transpiler/x/cs"
	"mochi/types"
)

func repoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found")
	return ""
}

func TestTranspilePrintHello(t *testing.T) {
	if _, err := exec.LookPath("dotnet"); err != nil {
		t.Skip("dotnet not installed")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "vm", "valid", "print_hello.mochi")
	wantPath := filepath.Join(root, "tests", "vm", "valid", "print_hello.out")

	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatal(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatal(errs[0])
	}
	ast, err := transpiler.Transpile(prog, env)
	if err != nil {
		t.Fatal(err)
	}
	code := transpiler.Emit(ast)

	dir := t.TempDir()
	proj := filepath.Join(dir, "app.csproj")
	csproj := `<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework></PropertyGroup></Project>`
	if err := os.WriteFile(proj, []byte(csproj), 0644); err != nil {
		t.Fatal(err)
	}
	file := filepath.Join(dir, "Program.cs")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("dotnet", "run", "--project", proj)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("dotnet run error: %v\n%s", err, out)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read expected output: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Fatalf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestCSTranspiler_Golden(t *testing.T) {
	if _, err := exec.LookPath("dotnet"); err != nil {
		t.Skip("dotnet not installed")
	}

	root := repoRoot(t)
	goldenDir := filepath.Join(root, "tests", "transpiler", "x", "cs")
	files, err := filepath.Glob(filepath.Join(goldenDir, "*.cs"))
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no golden files in %s", goldenDir)
	}

	for _, csPath := range files {
		name := strings.TrimSuffix(filepath.Base(csPath), ".cs")
		src := filepath.Join(root, "tests", "vm", "valid", name+".mochi")
		wantPath := strings.TrimSuffix(csPath, ".cs") + ".out"

		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			ast, err := transpiler.Transpile(prog, env)
			if err != nil {
				t.Fatalf("transpile error: %v", err)
			}
			code := transpiler.Emit(ast)
			if err := os.WriteFile(csPath, code, 0644); err != nil {
				t.Fatalf("write generated: %v", err)
			}

			dir := t.TempDir()
			proj := filepath.Join(dir, "app.csproj")
			csproj := `<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework></PropertyGroup></Project>`
			if err := os.WriteFile(proj, []byte(csproj), 0644); err != nil {
				t.Fatal(err)
			}
			file := filepath.Join(dir, "Program.cs")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatal(err)
			}
			cmd := exec.Command("dotnet", "run", "--project", proj)
			out, err := cmd.CombinedOutput()
			errPath := strings.TrimSuffix(csPath, ".cs") + ".error"
			if err != nil {
				_ = os.WriteFile(errPath, out, 0644)
				t.Fatalf("dotnet run error: %v\n%s", err, out)
			}
			if _, statErr := os.Stat(errPath); statErr == nil {
				_ = os.Remove(errPath)
			}
			got := bytes.TrimSpace(out)
			want, err := os.ReadFile(wantPath)
			if err != nil {
				t.Fatalf("read expected output: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", got, want)
			}
		})
	}
}
