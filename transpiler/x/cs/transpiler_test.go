//go:build slow

package cstranspiler_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

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
	cmd.Env = append(os.Environ(), "DOTNET_NOLOGO=1", "DOTNET_SKIP_FIRST_TIME_EXPERIENCE=1")
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
			cmd.Env = append(os.Environ(), "DOTNET_NOLOGO=1", "DOTNET_SKIP_FIRST_TIME_EXPERIENCE=1")
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

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	updateTasks()
	os.Exit(code)
}

func countCompiled() (int, int) {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "cs")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
		}
	}
	return compiled, total
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	binDir := filepath.Join(root, "tests", "transpiler", "x", "cs")
	readmePath := filepath.Join(root, "transpiler", "x", "cs", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(binDir, name+".out")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# C# Transpiler Output\n\n")
	buf.WriteString("Generated C# code for programs in `tests/vm/valid`. Each program has a `.cs` file produced by the transpiler and a `.out` file containing its runtime output. Compilation or execution errors are captured in a `.error` file placed next to the source.\n\n")
	fmt.Fprintf(&buf, "Compiled programs: %d/%d\n\n", compiled, total)
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRoot(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "cs", "TASKS.md")
	compiled, total := countCompiled()
	out, err := exec.Command("git", "log", "-1", "--format=%cI;%s").Output()
	ts := ""
	msg := "updated"
	if err == nil {
		parts := strings.SplitN(strings.TrimSpace(string(out)), ";", 2)
		if len(parts) == 2 {
			if t, perr := time.Parse(time.RFC3339, parts[0]); perr == nil {
				ts = t.Format("2006-01-02 15:04 MST")
			}
			msg = parts[1]
		}
	}
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "## Progress (%s)\n", ts)
	fmt.Fprintf(&buf, "- %s (progress %d/%d)\n\n", msg, compiled, total)
	if data, err := os.ReadFile(taskFile); err == nil {
		lines := strings.Split(string(data), "\n")
		for _, ln := range lines {
			if strings.Contains(ln, "VM valid golden test results updated") {
				continue
			}
			buf.WriteString(ln)
			if !strings.HasSuffix(ln, "\n") {
				buf.WriteByte('\n')
			}
		}
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
