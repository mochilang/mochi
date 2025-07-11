//go:build ignore

package main

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	dartcode "mochi/compiler/x/dart"
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
	outDir := filepath.Join("tests", "machine", "x", "dart")
	_ = os.MkdirAll(outDir, 0755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		errPath := filepath.Join(outDir, name+".error")
		dartPath := filepath.Join(outDir, name+".dart")
		outPath := filepath.Join(outDir, name+".out")

		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			os.Remove(dartPath)
			os.Remove(outPath)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			os.WriteFile(errPath, []byte(errs[0].Error()), 0644)
			os.Remove(dartPath)
			os.Remove(outPath)
			continue
		}
		code, err := dartcode.New(env).Compile(prog)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			os.Remove(dartPath)
			os.Remove(outPath)
			continue
		}
		os.WriteFile(dartPath, code, 0644)
		os.Remove(errPath)
		if dartExe, err := exec.LookPath("dart"); err == nil {
			cmd := exec.Command(dartExe, dartPath)
			if in, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(in)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				os.WriteFile(errPath, []byte(err.Error()+"\n"+string(out)), 0644)
				continue
			}
			os.WriteFile(outPath, bytes.TrimSpace(out), 0644)
		}
	}
}
