//go:build archive

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	luacode "mochi/compiler/x/lua"
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
	_ = os.Remove(filepath.Join(dir, name+".out"))
}

func main() {
	os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
	defer os.Unsetenv("MOCHI_HEADER_TIME")

	root := repoRoot()
	outDir := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "lua")
	_ = os.MkdirAll(outDir, 0o755)

	if err := luacode.EnsureLua(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	for i := 1; i <= 22; i++ {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-h", q+".mochi")

		prog, err := parser.Parse(src)
		if err != nil {
			writeError(outDir, q, fmt.Sprintf("parse: %v", err))
			continue
		}

		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			writeError(outDir, q, fmt.Sprintf("type: %v", errs[0]))
			continue
		}

		code, err := luacode.New(env).Compile(prog)
		if err != nil {
			writeError(outDir, q, fmt.Sprintf("compile: %v", err))
			continue
		}

		codePath := filepath.Join(outDir, q+".lua")
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write code", q, err)
			continue
		}

		tmp := filepath.Join(os.TempDir(), q+".lua")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			writeError(outDir, q, fmt.Sprintf("tmp write: %v", err))
			continue
		}

		cmd := exec.Command("lua", tmp)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			writeError(outDir, q, fmt.Sprintf("run: %v\n%s", err, out))
			continue
		}

		os.Remove(filepath.Join(outDir, q+".error"))
		cleaned := append(bytes.TrimSpace(out), '\n')
		if err := os.WriteFile(filepath.Join(outDir, q+".out"), cleaned, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write out", q, err)
		}
	}
}
