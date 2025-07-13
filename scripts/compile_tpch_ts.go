//go:build ignore

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	tscode "mochi/compiler/x/ts"
	"mochi/parser"
	"mochi/types"
)

func main() {
	os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
	defer os.Unsetenv("MOCHI_HEADER_TIME")

	outDir := filepath.Join("tests", "dataset", "tpc-h", "compiler", "ts")
	_ = os.MkdirAll(outDir, 0o755)
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
		c := tscode.New(env, "")
		code, err := c.Compile(prog)
		if err != nil {
			panic(err)
		}
		codeOut := filepath.Join(outDir, q+".ts")
		if err := os.WriteFile(codeOut, code, 0o644); err != nil {
			panic(err)
		}
		tmp := filepath.Join(os.TempDir(), q+".ts")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			panic(err)
		}
		cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", tmp)
		cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
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
