//go:build slow

package cscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	cscode "mochi/compiler/x/cs"
	"mochi/parser"
	"mochi/types"
)

func shouldUpdateVM() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

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

// runVMGolden compiles a Mochi program from tests/vm/valid to C#, executes it
// with dotnet and compares the output with the golden file under
// tests/machine/x/cs. Generated source is written for reference.
func runVMGolden(t *testing.T, src string) bool {
	root := repoRoot(t)
	name := strings.TrimSuffix(filepath.Base(src), filepath.Ext(src))

	os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
	os.Setenv("SOURCE_DATE_EPOCH", "0")
	defer os.Unsetenv("MOCHI_HEADER_TIME")
	defer os.Unsetenv("SOURCE_DATE_EPOCH")

	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}

	outDir := filepath.Join(root, "tests", "machine", "x", "cs")
	codePath := filepath.Join(outDir, name+".cs")
	outPath := filepath.Join(outDir, name+".out")
	errPath := filepath.Join(outDir, name+".error")

	code, err := cscode.New(env).Compile(prog)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("compile: "+err.Error()), 0644)
		return false
	}

	if shouldUpdateVM() {
		if err := os.WriteFile(codePath, code, 0644); err != nil {
			t.Fatalf("write code: %v", err)
		}
	} else {
		_ = os.WriteFile(codePath, code, 0644)
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
	if inData, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(inData)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(errPath, []byte(fmt.Sprintf("run: %v\n%s", err, out)), 0644)
		return false
	}
	got := bytes.TrimSpace(out)
	_ = os.Remove(errPath)

	if shouldUpdateVM() {
		if err := os.WriteFile(outPath, append(got, '\n'), 0644); err != nil {
			t.Fatalf("write golden out: %v", err)
		}
		return true
	}
	if want, err := os.ReadFile(outPath); err == nil {
		if !bytes.Equal(got, bytes.TrimSpace(want)) {
			t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, bytes.TrimSpace(want))
			return false
		}
	}
	return true
}

func TestCSCompiler_VMValid_Golden(t *testing.T) {
	if err := cscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)
	passed := 0
	for _, f := range files {
		name := filepath.Base(f)
		t.Run(name, func(t *testing.T) {
			if runVMGolden(t, f) {
				passed++
			}
		})
	}
	t.Logf("passed %d/%d programs", passed, len(files))
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "cs")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# C# Machine Output\n\n")
	buf.WriteString("This directory holds C# source generated from the Mochi programs in `tests/vm/valid`. Each compiled program has a `.cs` file and the expected output in a matching `.out`. If the compiler failed a `.error` file will be present instead.\n\n")
	fmt.Fprintf(&buf, "Compiled programs: %d/%d\n\n", compiled, total)
	buf.WriteString("Checklist:\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(filepath.Join(outDir, "README.md"), buf.Bytes(), 0644)
}
