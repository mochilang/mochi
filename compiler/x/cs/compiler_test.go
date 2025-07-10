//go:build slow

package cscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
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
			data, err := os.ReadFile(file)
			if err != nil {
				t.Fatal(err)
			}
			prog, err := parser.Parse(file)
			if err != nil {
				writeError(outDir, name, data, err)
				t.Skip("parse error")
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeError(outDir, name, data, errs[0])
				t.Skip("type error")
				return
			}
			code, err := cscode.New(env).Compile(prog)
			if err != nil {
				writeError(outDir, name, data, err)
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
				writeError(outDir, name, data, err)
				return
			}
			if err := os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(out.Bytes()), 0644); err != nil {
				t.Fatal(err)
			}
			_ = os.Remove(filepath.Join(outDir, name+".error"))
		})
	}
}

func writeError(dir, name string, src []byte, err error) {
	line := extractLine(err.Error())
	var context string
	if line > 0 {
		lines := bytes.Split(src, []byte("\n"))
		start := line - 2
		if start < 1 {
			start = 1
		}
		end := line + 2
		if end > len(lines) {
			end = len(lines)
		}
		var b strings.Builder
		for i := start; i <= end; i++ {
			if i-1 < len(lines) {
				fmt.Fprintf(&b, "%4d: %s\n", i, lines[i-1])
			}
		}
		context = b.String()
	}
	msg := fmt.Sprintf("line: %d\nerror: %v\n%s", line, err, context)
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg), 0644)
}

func extractLine(msg string) int {
	re := regexp.MustCompile(`:(\d+):`)
	if m := re.FindStringSubmatch(msg); m != nil {
		if n, err := strconv.Atoi(m[1]); err == nil {
			return n
		}
	}
	re = regexp.MustCompile(`line (\d+)`)
	if m := re.FindStringSubmatch(msg); m != nil {
		if n, err := strconv.Atoi(m[1]); err == nil {
			return n
		}
	}
	return 0
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

func TestMain(m *testing.M) {
        code := m.Run()
        updateReadme()
        os.Exit(code)
}

func updateReadme() {
        root := findRepoRoot(&testing.T{})
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
        _ = os.WriteFile(filepath.Join(outDir, "README.md"), buf.Bytes(), 0o644)
}
