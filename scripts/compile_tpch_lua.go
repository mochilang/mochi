//go:build ignore

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

func main() {
	outDir := filepath.Join("tests", "dataset", "tpc-h", "compiler", "lua")
	_ = os.MkdirAll(outDir, 0o755)
	// Compile queries q1 through q22. Earlier versions of this script only
	// handled q11 and above.
	for i := 1; i <= 22; i++ {
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
		code, err := luacode.New(env).Compile(prog)
		if err != nil {
			panic(err)
		}
		outPath := filepath.Join(outDir, q+".lua")
		if err := os.WriteFile(outPath, code, 0o644); err != nil {
			panic(err)
		}
		tmp := filepath.Join(os.TempDir(), q+".lua")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			panic(err)
		}
		cmd := exec.Command("lua", tmp)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			panic(fmt.Errorf("run %s: %v\n%s", q, err, out))
		}
		cleaned := append(bytes.TrimSpace(out), '\n')
		if err := os.WriteFile(filepath.Join(outDir, q+".out"), cleaned, 0o644); err != nil {
			panic(err)
		}
	}
}
