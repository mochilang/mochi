//go:build slow

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
	"mochi/runtime/vm"
	"mochi/types"
)

func TestCSCompiler_VMValid_Golden(t *testing.T) {
	if err := cscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "cs")

	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
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
			wantCodePath := filepath.Join(outDir, name+".cs")
			wantOutPath := filepath.Join(outDir, name+".out")
			if want, err := os.ReadFile(wantCodePath); err == nil {
				if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(want)) {
					t.Errorf("generated code mismatch for %s.cs\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, bytes.TrimSpace(want))
				}
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
				t.Fatalf("dotnet run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			if want, err := os.ReadFile(wantOutPath); err == nil {
				if !bytes.Equal(gotOut, bytes.TrimSpace(want)) {
					t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, gotOut, bytes.TrimSpace(want))
				}
			}
			if p, err := vm.Compile(prog, env); err == nil {
				var buf bytes.Buffer
				m := vm.New(p, &buf)
				if err := m.Run(); err == nil {
					if vmOut := bytes.TrimSpace(buf.Bytes()); !bytes.Equal(gotOut, vmOut) {
						t.Errorf("vm mismatch\n\n--- VM ---\n%s\n\n--- C# ---\n%s", vmOut, gotOut)
					}
				}
			}
			_ = os.WriteFile(wantCodePath, code, 0644)
			_ = os.WriteFile(wantOutPath, gotOut, 0644)
		})
	}
}
