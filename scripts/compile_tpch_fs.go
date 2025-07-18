//go:build archive && slow

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	fscode "mochi/compiler/x/fs"
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
	outDir := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "fs")
	_ = os.MkdirAll(outDir, 0o755)

	var queries []string
	if env := os.Getenv("QUERIES"); env != "" {
		for _, part := range strings.Split(env, ",") {
			n := strings.TrimSpace(part)
			if n != "" {
				queries = append(queries, n)
			}
		}
	} else {
		for i := 1; i <= 22; i++ {
			queries = append(queries, fmt.Sprintf("q%d", i))
		}
	}

	jsonRef := "/usr/lib/dotnet/shared/Microsoft.NETCore.App/8.0.17/System.Text.Json.dll"
	runtimeRef := "/usr/lib/dotnet/shared/Microsoft.NETCore.App/8.0.17/System.Runtime.dll"
	if matches, _ := filepath.Glob("/usr/lib/dotnet/shared/Microsoft.NETCore.App/*/System.Text.Json.dll"); len(matches) > 0 {
		jsonRef = matches[0]
	}
	if matches, _ := filepath.Glob("/usr/lib/dotnet/shared/Microsoft.NETCore.App/*/System.Runtime.dll"); len(matches) > 0 {
		runtimeRef = matches[0]
	}

	for _, name := range queries {
		src := filepath.Join(root, "tests", "dataset", "tpc-h", name+".mochi")
		if _, err := os.Stat(src); err != nil {
			continue
		}
		prog, err := parser.Parse(src)
		if err != nil {
			writeError(outDir, name, fmt.Sprintf("parse: %v", err))
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			writeError(outDir, name, fmt.Sprintf("type: %v", errs[0]))
			continue
		}
		code, err := fscode.CompileFile(src)
		if err != nil {
			writeError(outDir, name, fmt.Sprintf("compile: %v", err))
			continue
		}
		codePath := filepath.Join(outDir, name+".fs")
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write code", name, err)
			continue
		}
		tmp := filepath.Join(os.TempDir(), name+".fs")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			writeError(outDir, name, fmt.Sprintf("tmp write: %v", err))
			os.Remove(filepath.Join(outDir, name+".out"))
			continue
		}
		exe := filepath.Join(os.TempDir(), name+".exe")
		cmd := exec.Command("fsharpc", "--target:exe", fmt.Sprintf("--out:%s", exe), "-r:"+jsonRef, "-r:"+runtimeRef, tmp)
		if out, err := cmd.CombinedOutput(); err != nil {
			writeError(outDir, name, fmt.Sprintf("fsharpc: %v\n%s", err, out))
			os.Remove(filepath.Join(outDir, name+".out"))
			continue
		}
		out, err := exec.Command("mono", exe).CombinedOutput()
		if err != nil {
			writeError(outDir, name, fmt.Sprintf("run: %v\n%s", err, out))
			os.Remove(filepath.Join(outDir, name+".out"))
			continue
		}
		os.Remove(filepath.Join(outDir, name+".error"))
		cleaned := append(bytes.TrimSpace(out), '\n')
		if err := os.WriteFile(filepath.Join(outDir, name+".out"), cleaned, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write out", name, err)
		}
	}
}
