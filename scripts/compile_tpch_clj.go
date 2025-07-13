//go:build archive && slow

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	cljcode "mochi/compiler/x/clj"
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
	outDir := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "clj")
	_ = os.MkdirAll(outDir, 0o755)
	for i := 1; i <= 5; i++ {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-h", q+".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			panic(err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			panic(errs[0])
		}
		code, err := cljcode.New(env).Compile(prog)
		if err != nil {
			panic(err)
		}
		outPath := filepath.Join(outDir, q+".clj")
		if err := os.WriteFile(outPath, code, 0o644); err != nil {
			panic(err)
		}
		// Execute program to create .out
		tmp := filepath.Join(os.TempDir(), q+".clj")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			panic(err)
		}
		cmd := exec.Command("clojure", tmp)
		cmd.Env = append(os.Environ(), "CLASSPATH=/usr/share/java/data.json.jar:/usr/share/java/snakeyaml-engine.jar")
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
