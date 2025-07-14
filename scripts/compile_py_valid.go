//go:build archive

package main

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	py "mochi/compiler/x/python"
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
	outDir := filepath.Join("tests", "machine", "x", "python")
	_ = os.MkdirAll(outDir, 0o755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		errPath := filepath.Join(outDir, name+".error")
		pyPath := filepath.Join(outDir, name+".py")
		outPath := filepath.Join(outDir, name+".out")

		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0o644)
			os.Remove(pyPath)
			os.Remove(outPath)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
			os.Remove(pyPath)
			os.Remove(outPath)
			continue
		}
		c := py.New(env)
		c.SetTypeHints(false)
		code, err := c.Compile(prog)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0o644)
			os.Remove(pyPath)
			os.Remove(outPath)
			continue
		}
		os.WriteFile(pyPath, code, 0o644)
		cmd := exec.Command("python3", pyPath)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()+"\n"+string(out)), 0o644)
			continue
		}
		os.WriteFile(outPath, bytes.TrimSpace(out), 0o644)
		os.Remove(errPath)
	}
}
