//go:build slow

package cscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	cscode "mochi/compile/cs"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestCSCompiler_SubsetPrograms(t *testing.T) {
	if err := cscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	if err := exec.Command("dotnet", "--version").Run(); err != nil {
		t.Skipf("dotnet not runnable: %v", err)
	}

	run := func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := cscode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		projDir := filepath.Join(dir, "app")
		if err := os.MkdirAll(projDir, 0755); err != nil {
			return nil, fmt.Errorf("mkdir: %w", err)
		}
		csproj := `<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework></PropertyGroup>
  <ItemGroup><PackageReference Include="YamlDotNet" Version="13.3.1" /></ItemGroup>
</Project>`
		if err := os.WriteFile(filepath.Join(projDir, "app.csproj"), []byte(csproj), 0644); err != nil {
			return nil, fmt.Errorf("write csproj: %w", err)
		}
		file := filepath.Join(projDir, "Program.cs")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("dotnet", "run", "--project", projDir)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ dotnet run error: %w\n%s", err, out)
		}
		res := bytes.TrimSpace(out)
		if res == nil {
			res = []byte{}
		}
		return res, nil
	}

	golden.Run(t, "tests/compiler/valid", ".mochi", ".out", run)
	golden.Run(t, "tests/compiler/cs", ".mochi", ".out", run)
}

func TestCSCompiler_GoldenOutput(t *testing.T) {
	compile := func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		code, err := cscode.New(env).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	}

	golden.Run(t, "tests/compiler/cs", ".mochi", ".cs.out", compile)
}

func TestCSCompiler_LeetCodeExamples(t *testing.T) {
	if err := cscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	if err := exec.Command("dotnet", "--version").Run(); err != nil {
		t.Skipf("dotnet not runnable: %v", err)
	}
	wants := map[int]string{1: "0\n1"}
	for i := 1; i <= 10; i++ {
		i := i
		want := wants[i]
		t.Run(fmt.Sprintf("example_%d", i), func(t *testing.T) {
			runLeetCode(t, i, want)
		})
	}
}

func runLeetCode(t *testing.T, id int, want string) {
	dir := filepath.Join("..", "..", "examples", "leetcode", fmt.Sprint(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil || len(files) == 0 {
		t.Fatalf("no source for problem %d", id)
	}
	src := files[0]
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := cscode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	tmp := t.TempDir()
	proj := filepath.Join(tmp, "app")
	if err := os.MkdirAll(proj, 0755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	csproj := `<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework></PropertyGroup>
  <ItemGroup><PackageReference Include="YamlDotNet" Version="13.3.1" /></ItemGroup>
</Project>`
	if err := os.WriteFile(filepath.Join(proj, "app.csproj"), []byte(csproj), 0644); err != nil {
		t.Fatalf("write csproj: %v", err)
	}
	if err := os.WriteFile(filepath.Join(proj, "Program.cs"), code, 0644); err != nil {
		t.Fatalf("write code: %v", err)
	}
	out, err := exec.Command("dotnet", "run", "--project", proj).CombinedOutput()
	if err != nil {
		t.Fatalf("dotnet run error: %v\n%s", err, out)
	}
	got := strings.TrimSpace(string(out))
	if got != want {
		t.Fatalf("unexpected output\nwant:\n%s\n got:\n%s", want, got)
	}
}
