//go:build archived && slow

package cscode_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	cscode "mochi/archived/x/cs"
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

// TestCSCompiler_GoldenOutputRunnable ensures the golden C# code compiles
// and produces the expected output when executed with dotnet.
func TestCSCompiler_GoldenOutputRunnable(t *testing.T) {
	if err := cscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	if err := exec.Command("dotnet", "--version").Run(); err != nil {
		t.Skipf("dotnet not runnable: %v", err)
	}

	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "compiler", "cs", "*.cs.out")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no golden files: %s", pattern)
	}
	for _, file := range files {
		name := strings.TrimSuffix(filepath.Base(file), ".cs.out")
		t.Run(name, func(t *testing.T) {
			code, err := os.ReadFile(file)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			dir := t.TempDir()
			projDir := filepath.Join(dir, "app")
			if err := os.MkdirAll(projDir, 0755); err != nil {
				t.Fatalf("mkdir: %v", err)
			}
			csproj := `<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework></PropertyGroup>
  <ItemGroup><PackageReference Include="YamlDotNet" Version="13.3.1" /></ItemGroup>
</Project>`
			if err := os.WriteFile(filepath.Join(projDir, "app.csproj"), []byte(csproj), 0644); err != nil {
				t.Fatalf("write csproj: %v", err)
			}
			if err := os.WriteFile(filepath.Join(projDir, "Program.cs"), code, 0644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			cmd := exec.Command("dotnet", "run", "--project", projDir)
			inPath := filepath.Join(root, "tests", "compiler", "cs", name+".in")
			if data, err := os.ReadFile(inPath); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("dotnet run error: %v\n%s", err, out)
			}
			got := strings.TrimSpace(string(out))
			wantPath := filepath.Join(root, "tests", "compiler", "cs", name+".out")
			wantData, err := os.ReadFile(wantPath)
			if err != nil {
				t.Fatalf("read golden output: %v", err)
			}
			want := strings.TrimSpace(string(wantData))
			if got != want {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, want)
			}
		})
	}
}

func TestCSCompiler_TPCHQ1(t *testing.T) {
	if err := cscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	if err := exec.Command("dotnet", "--version").Run(); err != nil {
		t.Skipf("dotnet not runnable: %v", err)
	}
	root := findRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-h", "q1.mochi")
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
	codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "cs", "q1.cs.out")
	wantCode, err := os.ReadFile(codeWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q1.cs.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(wantCode))
	}
	dir := t.TempDir()
	proj := filepath.Join(dir, "app")
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
	gotOut := bytes.TrimSpace(out)
	outWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "cs", "q1.out")
	wantOut, err := os.ReadFile(outWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch for q1.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", gotOut, bytes.TrimSpace(wantOut))
	}
}

func TestCSCompiler_TPCHQ2(t *testing.T) {
	if err := cscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	if err := exec.Command("dotnet", "--version").Run(); err != nil {
		t.Skipf("dotnet not runnable: %v", err)
	}
	root := findRepoRoot(t)
	src := filepath.Join(root, "tests", "dataset", "tpc-h", "q2.mochi")
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
	codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "cs", "q2.cs.out")
	wantCode, err := os.ReadFile(codeWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
		t.Errorf("generated code mismatch for q2.cs.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, bytes.TrimSpace(wantCode))
	}
	dir := t.TempDir()
	proj := filepath.Join(dir, "app")
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
	gotOut := bytes.TrimSpace(out)
	outWantPath := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "cs", "q2.out")
	wantOut, err := os.ReadFile(outWantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
		t.Errorf("output mismatch for q2.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", gotOut, bytes.TrimSpace(wantOut))
	}
}

func TestCSCompiler_TPCDSQueries(t *testing.T) {
	if err := cscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	if err := exec.Command("dotnet", "--version").Run(); err != nil {
		t.Skipf("dotnet not runnable: %v", err)
	}
	root := findRepoRoot(t)
	for i := 1; i <= 99; i++ {
		q := fmt.Sprintf("q%d", i)
		t.Run(q, func(t *testing.T) {
			src := filepath.Join(root, "tests", "dataset", "tpc-ds", q+".mochi")
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
			codeWantPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "cs", q+".cs.out")
			wantCode, err := os.ReadFile(codeWantPath)
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
			gotOut := bytes.TrimSpace(out)
			outWantPath := filepath.Join(root, "tests", "dataset", "tpc-ds", "compiler", "cs", q+".out")
			wantOut, err := os.ReadFile(outWantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}

func TestCSCompiler_JOBQueries(t *testing.T) {
	if err := cscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	if err := exec.Command("dotnet", "--version").Run(); err != nil {
		t.Skipf("dotnet not runnable: %v", err)
	}
	root := findRepoRoot(t)
	for i := 1; i <= 10; i++ {
		q := fmt.Sprintf("q%d", i)
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
			codePath := filepath.Join(root, "tests", "dataset", "job", "compiler", "cs", q+".cs.out")
			if flag.Lookup("update") != nil && flag.Lookup("update").Value.String() == "true" {
				if err := os.WriteFile(codePath, bytes.TrimSpace(code), 0644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
			} else {
				want, err := os.ReadFile(codePath)
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(want)) {
					t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", filepath.Base(codePath), got, bytes.TrimSpace(want))
				}
			}

			dir := t.TempDir()
			proj := filepath.Join(dir, "app")
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
			gotOut := bytes.TrimSpace(out)
			outPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "cs", q+".out")
			if flag.Lookup("update") != nil && flag.Lookup("update").Value.String() == "true" {
				if err := os.WriteFile(outPath, gotOut, 0644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
				return
			}
			wantOut, err := os.ReadFile(outPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", q+".out", gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
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

func findRepoRoot(t *testing.T) string {
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
	t.Fatal("go.mod not found (not in Go module)")
	return ""
}
