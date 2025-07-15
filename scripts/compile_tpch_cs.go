package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	cscode "mochi/compiler/x/cs"
	"mochi/parser"
	"mochi/types"
)

func main() {
	root, _ := os.Getwd()
	for {
		if _, err := os.Stat(filepath.Join(root, "go.mod")); err == nil {
			break
		}
		parent := filepath.Dir(root)
		if parent == root {
			panic("go.mod not found")
		}
		root = parent
	}
	outDir := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "cs")
	_ = os.MkdirAll(outDir, 0o755)
	for i := 1; i <= 22; i++ {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-h", q+".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			fmt.Fprintln(os.Stderr, "parse", q, err)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			fmt.Fprintln(os.Stderr, "type", q, errs[0])
			continue
		}
		comp := cscode.New(env)
		comp.DictMode = true
		code, err := comp.Compile(prog)
		if err != nil {
			fmt.Fprintln(os.Stderr, "compile", q, err)
			continue
		}
		codePath := filepath.Join(outDir, q+".cs")
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write code", q, err)
			continue
		}
		dir, err := os.MkdirTemp("", "tpchcs")
		if err != nil {
			fmt.Fprintln(os.Stderr, "tmp dir", q, err)
			continue
		}
		proj := filepath.Join(dir, "app.csproj")
		csproj := `<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework></PropertyGroup><ItemGroup><PackageReference Include="YamlDotNet" Version="13.3.1" /></ItemGroup></Project>`
		if err := os.WriteFile(proj, []byte(csproj), 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "proj", q, err)
			continue
		}
		file := filepath.Join(dir, "Program.cs")
		if err := os.WriteFile(file, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "code", q, err)
			continue
		}
		cmd := exec.Command("dotnet", "run", "--project", proj)
		out, err := cmd.CombinedOutput()
		if err != nil {
			fmt.Fprintf(os.Stderr, "run %s: %v\n%s\n", q, err, out)
			continue
		}
		outPath := filepath.Join(outDir, q+".out")
		if err := os.WriteFile(outPath, bytes.TrimSpace(out), 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write out", q, err)
		}
	}
}
