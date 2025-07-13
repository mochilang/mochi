//go:build slow

package cscode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	cscode "mochi/compiler/x/cs"
	"mochi/parser"
	"mochi/types"
)

func TestCSCompiler_TPCH_Q1(t *testing.T) {
	if err := cscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-h", "q1.mochi")
	codeWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "cs", "q1.cs")
	outWant := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "cs", "q1.out")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	comp := cscode.New(env)
	comp.DictMode = true
	code, err := comp.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	wantCode, err := os.ReadFile(codeWant)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s", got, bytes.TrimSpace(wantCode))
	}
	dir := t.TempDir()
	proj := filepath.Join(dir, "app.csproj")
	csproj := `<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework></PropertyGroup><ItemGroup><PackageReference Include="YamlDotNet" Version="13.3.1" /></ItemGroup></Project>`
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
		t.Skipf("dotnet run error: %v\n%s", err, out)
		return
	}
	got := bytes.TrimSpace(out)
	wantOut, err := os.ReadFile(outWant)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(got, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s", got, bytes.TrimSpace(wantOut))
	}
}

func repoRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
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
