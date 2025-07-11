//go:build ignore

package main

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	luacode "mochi/compiler/x/lua"
	"mochi/parser"
	"mochi/types"
)

func main() {
	pattern := filepath.Join("tests", "vm", "valid", "*.mochi")
	if p := os.Getenv("PATTERN"); p != "" {
		pattern = p
	}
	files, err := filepath.Glob(pattern)
	if err != nil {
		panic(err)
	}
	outDir := filepath.Join("tests", "machine", "x", "lua")
	_ = os.MkdirAll(outDir, 0755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		errPath := filepath.Join(outDir, name+".error")
		luaPath := filepath.Join(outDir, name+".lua")
		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			os.Remove(luaPath)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			os.WriteFile(errPath, []byte(errs[0].Error()), 0644)
			os.Remove(luaPath)
			continue
		}
		code, err := luacode.New(env).Compile(prog)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			os.Remove(luaPath)
			continue
		}
		os.WriteFile(luaPath, code, 0644)
		os.Remove(errPath)
		// run the generated code if lua interpreter available
		if _, err := exec.LookPath("lua"); err == nil {
			cmd := exec.Command("lua", luaPath)
			if in, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(in)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				os.WriteFile(errPath, []byte(err.Error()+"\n"+string(out)), 0644)
				continue
			}
			outPath := filepath.Join(outDir, name+".out")
			os.WriteFile(outPath, bytes.TrimSpace(out), 0644)
		}
	}
}
