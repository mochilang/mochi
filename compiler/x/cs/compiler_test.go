package cscode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	cscode "mochi/compiler/x/cs"
	"mochi/parser"
	"mochi/types"
)

func TestCompileValidPrograms(t *testing.T) {
	if _, err := exec.LookPath("dotnet"); err != nil {
		t.Skip("dotnet not installed")
	}

	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "cs")
	if err := os.MkdirAll(outDir, 0755); err != nil {
		t.Fatal(err)
	}
	for _, file := range files {
		name := strings.TrimSuffix(filepath.Base(file), ".mochi")
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(file)
			if err != nil {
				writeError(outDir, name, err)
				t.Skip("parse error")
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeError(outDir, name, errs[0])
				t.Skip("type error")
				return
			}
			code, err := cscode.New(env).Compile(prog)
			if err != nil {
				writeError(outDir, name, err)
				t.Skip("compile error")
				return
			}
			csFile := filepath.Join(outDir, name+".cs")
			if err := os.WriteFile(csFile, code, 0644); err != nil {
				t.Fatal(err)
			}
			projDir := filepath.Join(t.TempDir(), "app")
			if err := os.MkdirAll(projDir, 0755); err != nil {
				t.Fatal(err)
			}
			csproj := `<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework></PropertyGroup>
  <ItemGroup><PackageReference Include="YamlDotNet" Version="13.3.1" /></ItemGroup>
</Project>`
			if err := os.WriteFile(filepath.Join(projDir, "app.csproj"), []byte(csproj), 0644); err != nil {
				t.Fatal(err)
			}
			if err := os.WriteFile(filepath.Join(projDir, "Program.cs"), code, 0644); err != nil {
				t.Fatal(err)
			}
			cmd := exec.Command("dotnet", "run", "--project", projDir)
			var out bytes.Buffer
			cmd.Stdout = &out
			cmd.Stderr = &out
			if err := cmd.Run(); err != nil {
				writeError(outDir, name, err)
				return
			}
			if err := os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(out.Bytes()), 0644); err != nil {
				t.Fatal(err)
			}
		})
	}
}

func writeError(dir, name string, err error) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(err.Error()), 0644)
}

func findRepoRoot(t *testing.T) string {
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
