//go:build slow

package cscode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	cscode "mochi/compile/x/cs"
	"mochi/parser"
	"mochi/types"
)

func TestCSCompiler_JOBQ1Q2(t *testing.T) {
	if err := cscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	if err := exec.Command("dotnet", "--version").Run(); err != nil {
		t.Skipf("dotnet not runnable: %v", err)
	}
	root := findRepoRoot(t)
	for _, q := range []string{"q1", "q2"} {
		q := q
		t.Run(q, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "job", q+".mochi")
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
			codeWant := filepath.Join(root, "tests", "dataset", "job", "compiler", "cs", q+".cs.out")
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
				t.Errorf("generated code mismatch for %s.cs.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, got, bytes.TrimSpace(wantCode))
			}
			dir := t.TempDir()
			proj := filepath.Join(dir, "app")
			if err := os.MkdirAll(proj, 0755); err != nil {
				t.Fatalf("mkdir: %v", err)
			}
			csproj := `<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework></PropertyGroup><ItemGroup><PackageReference Include="YamlDotNet" Version="13.3.1" /></ItemGroup></Project>`
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
			gotOut := bytes.TrimSpace(out)
			outWant := filepath.Join(root, "tests", "dataset", "job", "compiler", "cs", q+".out")
			wantOut, err := os.ReadFile(outWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}
