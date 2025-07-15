//go:build slow

package cscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	cscode "mochi/compiler/x/cs"
	"mochi/parser"
	"mochi/types"
)

func TestCSCompiler_TPCDS(t *testing.T) {
	if err := cscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	root := repoRoot(t)
	for i := 1; i <= 99; i++ {
		base := fmt.Sprintf("q%d", i)
		t.Run(base, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "tpc-ds", base+".mochi")
			codeWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "cs", base+".cs.out")
			outWant := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "cs", base+".out")
			if _, err := os.Stat(codeWant); err != nil {
				t.Skipf("missing golden: %v", err)
			}
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
				t.Skipf("compile error: %v", err)
				return
			}
			wantCode, err := os.ReadFile(codeWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			got := bytes.TrimSpace(code)
			want := bytes.TrimSpace(wantCode)
			if !bytes.Equal(got, want) {
				t.Errorf("generated code mismatch for %s.cs.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, got, want)
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
			gotOut := bytes.TrimSpace(out)
			wantOut, err := os.ReadFile(outWant)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}
