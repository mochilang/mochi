//go:build ignore

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	py "mochi/compiler/x/python"
	"mochi/parser"
	"mochi/types"
)

func main() {
	outDir := filepath.Join("tests", "dataset", "tpc-h", "compiler", "py")
	_ = os.MkdirAll(outDir, 0o755)
	for i := 1; i <= 5; i++ {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join("tests", "dataset", "tpc-h", q+".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			panic(err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			panic(errs[0])
		}
		c := py.New(env)
		c.SetTypeHints(true)
		code, err := c.Compile(prog)
		if err != nil {
			panic(err)
		}
		codeOut := filepath.Join(outDir, q+".py.out")
		os.WriteFile(codeOut, code, 0o644)

		tmpDir := os.TempDir()
		file := filepath.Join(tmpDir, q+".py")
		os.WriteFile(file, code, 0o644)
		cmd := exec.Command("python3", file)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			panic(fmt.Errorf("run %s: %v\n%s", q, err, out))
		}
		os.WriteFile(filepath.Join(outDir, q+".out"), bytes.TrimSpace(out), 0o644)
	}
}
