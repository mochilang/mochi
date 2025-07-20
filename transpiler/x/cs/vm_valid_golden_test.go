//go:build slow

package cstranspiler_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/golden"
	"mochi/parser"
	cs "mochi/transpiler/x/cs"
	"mochi/types"
)

func TestCSTranspiler_VMValid_Golden(t *testing.T) {
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "cs")
	os.MkdirAll(outDir, 0o755)

	if _, err := exec.LookPath("dotnet"); err != nil {
		t.Skip("dotnet not installed")
	}

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
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
	})
}
