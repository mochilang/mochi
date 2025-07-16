//go:build archive

package main

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	cljcode "mochi/compiler/x/clj"
	"mochi/parser"
	"mochi/types"
)

func main() {
	flag.Parse()
	root, _ := filepath.Abs(".")
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "clj")
	os.MkdirAll(outDir, 0755)
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	for _, src := range files {
		base := filepath.Base(src)
		name := base[:len(base)-6]
		codePath := filepath.Join(outDir, name+".clj")
		outPath := filepath.Join(outDir, name+".out")
		errPath := filepath.Join(outDir, name+".error")

		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			os.WriteFile(errPath, []byte(errs[0].Error()), 0644)
			continue
		}
		code, err := cljcode.New(env).Compile(prog)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			continue
		}
		os.WriteFile(codePath, code, 0644)
		cmd := exec.Command("clojure", codePath)
		if data, err := os.ReadFile(src[:len(src)-6] + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		cmd.Env = append(os.Environ(), "CLASSPATH=/usr/share/java/data.json.jar:/usr/share/java/snakeyaml-engine.jar")
		out, err := cmd.CombinedOutput()
		if err != nil {
			os.WriteFile(errPath, []byte(fmt.Sprintf("%v\n%s", err, out)), 0644)
			continue
		}
		os.WriteFile(outPath, out, 0644)
		os.Remove(errPath)
	}
}
