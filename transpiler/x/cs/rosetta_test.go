//go:build slow

package cstranspiler_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	cs "mochi/transpiler/x/cs"
	"mochi/types"
)

func TestCSTranspiler_Rosetta_Golden(t *testing.T) {
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "CS")
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	os.MkdirAll(outDir, 0o755)

	if _, err := exec.LookPath("dotnet"); err != nil {
		t.Skip("dotnet not installed")
	}

	pattern := filepath.Join(srcDir, "*.mochi")
	if only := os.Getenv("MOCHI_ROSETTA_ONLY"); only != "" {
		pattern = filepath.Join(srcDir, only+".mochi")
	}
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(files) == 0 {
		t.Fatalf("no Mochi Rosetta tests found: %s", pattern)
	}
	sort.Strings(files)
	firstOnly := os.Getenv("MOCHI_FIRST_ERROR") == "1"

	runCase := func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".cs")
		outPath := filepath.Join(outDir, base+".out")
		errPath := filepath.Join(outDir, base+".error")

		prog, err := parser.Parse(src)
		if err != nil {
			_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
			return nil, errs[0]
		}
		ast, err := cs.Transpile(prog, env)
		if err != nil {
			_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
			return nil, err
		}
		code := cs.Emit(ast)
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		tmp := t.TempDir()
		proj := filepath.Join(tmp, "app.csproj")
		csproj := `<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework></PropertyGroup></Project>`
		if err := os.WriteFile(proj, []byte(csproj), 0644); err != nil {
			return nil, err
		}
		file := filepath.Join(tmp, "Program.cs")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, err
		}
		cmd := exec.Command("dotnet", "run", "--project", proj)
		cmd.Env = append(os.Environ(), "DOTNET_NOLOGO=1", "DOTNET_SKIP_FIRST_TIME_EXPERIENCE=1")
		out, err := cmd.CombinedOutput()
		if err != nil {
			_ = os.WriteFile(errPath, append([]byte("dotnet run: "+err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		_ = os.Remove(errPath)
		outBytes := bytes.TrimSpace(out)
		_ = os.WriteFile(outPath, outBytes, 0o644)
		return outBytes, nil
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		ok := t.Run(name, func(t *testing.T) {
			if _, err := runCase(src); err != nil {
				t.Fatalf("%v", err)
			}
		})
		if firstOnly && !ok {
			break
		}
	}
}

func updateRosetta() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "CS")
	mdPath := filepath.Join(root, "transpiler", "x", "cs", "ROSETTA.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			if _, err2 := os.Stat(filepath.Join(outDir, name+".error")); os.IsNotExist(err2) {
				compiled++
				mark = "[x]"
			}
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}

	var buf bytes.Buffer
	fmt.Fprintf(&buf, "# Rosetta C# Transpiler Output\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	if out, err := exec.Command("git", "log", "-1", "--format=%cI").Output(); err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			fmt.Fprintf(&buf, "Last updated: %s\n", t.Format("2006-01-02 15:04 MST"))
		}
	}
	buf.WriteString("\n## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(mdPath, buf.Bytes(), 0o644)
}
