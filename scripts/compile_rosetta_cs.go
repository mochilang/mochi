//go:build archive && slow

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	cscode "mochi/compiler/x/cs"
	"mochi/parser"
	"mochi/types"
)

func repoRoot() string {
	dir, _ := os.Getwd()
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		p := filepath.Dir(dir)
		if p == dir {
			break
		}
		dir = p
	}
	return dir
}

func writeError(dir, name, msg string) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg), 0o644)
}

func main() {
	os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
	defer os.Unsetenv("MOCHI_HEADER_TIME")

	root := repoRoot()
	outDir := filepath.Join(root, "tests", "rosetta", "out", "CS")
	_ = os.MkdirAll(outDir, 0o755)

	var tasks []string
	if env := os.Getenv("TASKS"); env != "" {
		for _, part := range strings.Split(env, ",") {
			n := strings.TrimSpace(part)
			if n != "" {
				tasks = append(tasks, n)
			}
		}
	} else {
		pattern := filepath.Join(root, "tests", "rosetta", "x", "Mochi", "*.mochi")
		files, _ := filepath.Glob(pattern)
		for _, f := range files {
			tasks = append(tasks, strings.TrimSuffix(filepath.Base(f), ".mochi"))
		}
	}

	csproj := `<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework></PropertyGroup><ItemGroup><PackageReference Include="YamlDotNet" Version="13.3.1" /></ItemGroup></Project>`

        for _, name := range tasks {
                func(name string) {
                        defer func() {
                                if r := recover(); r != nil {
                                        writeError(outDir, name, fmt.Sprintf("panic: %v", r))
                                }
                        }()

                        src := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".mochi")
                        prog, err := parser.Parse(src)
                        if err != nil {
                                writeError(outDir, name, fmt.Sprintf("parse: %v", err))
                                return
                        }
                        env := types.NewEnv(nil)
                        if errs := types.Check(prog, env); len(errs) > 0 {
                                writeError(outDir, name, fmt.Sprintf("type: %v", errs[0]))
                                return
                        }
                        code, err := cscode.New(env).Compile(prog)
                        if err != nil {
                                writeError(outDir, name, fmt.Sprintf("compile: %v", err))
                                return
                        }
                        codeFile := filepath.Join(outDir, name+".cs")
                        if err := os.WriteFile(codeFile, code, 0o644); err != nil {
                                fmt.Fprintln(os.Stderr, "write code", name, err)
                                return
                        }
                        tmpDir := filepath.Join(os.TempDir(), "TestMochiToCS"+name)
                        _ = os.RemoveAll(tmpDir)
                        if err := os.MkdirAll(tmpDir, 0o755); err != nil {
                                writeError(outDir, name, fmt.Sprintf("tmp dir: %v", err))
                                os.Remove(filepath.Join(outDir, name+".out"))
                                return
                        }
                        proj := filepath.Join(tmpDir, "app.csproj")
                        if err := os.WriteFile(proj, []byte(csproj), 0644); err != nil {
                                writeError(outDir, name, fmt.Sprintf("csproj: %v", err))
                                os.Remove(filepath.Join(outDir, name+".out"))
                                return
                        }
                        file := filepath.Join(tmpDir, "Program.cs")
                        if err := os.WriteFile(file, code, 0644); err != nil {
                                writeError(outDir, name, fmt.Sprintf("tmp write: %v", err))
                                os.Remove(filepath.Join(outDir, name+".out"))
                                return
                        }
                        cmd := exec.Command("dotnet", "run", "--project", proj)
                        out, err := cmd.CombinedOutput()
                        if err != nil {
                                writeError(outDir, name, fmt.Sprintf("run: %v\n%s", err, out))
                                os.Remove(filepath.Join(outDir, name+".out"))
                                return
                        }
                        os.Remove(filepath.Join(outDir, name+".error"))
                        cleaned := append(bytes.TrimSpace(out), '\n')
                        if err := os.WriteFile(filepath.Join(outDir, name+".out"), cleaned, 0o644); err != nil {
                                fmt.Fprintln(os.Stderr, "write out", name, err)
                        }
                }(name)
        }
}
