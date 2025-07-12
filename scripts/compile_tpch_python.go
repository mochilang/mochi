//go:build ignore

package main

import (
	"os"
	"os/exec"
	"path/filepath"

	pycode "mochi/compiler/x/python"
)

func main() {
	src := filepath.Join("tests", "dataset", "tpc-h", "q1.mochi")
	outDir := filepath.Join("tests", "dataset", "tpc-h", "generated", "python")
	os.MkdirAll(outDir, 0755)
	pyPath := filepath.Join(outDir, "q1.py")
	outPath := filepath.Join(outDir, "q1.out")

	code, err := pycode.CompileFile(src)
	if err != nil {
		panic(err)
	}
	if err := os.WriteFile(pyPath, code, 0644); err != nil {
		panic(err)
	}
	cmd := exec.Command("python3", pyPath)
	out, err := cmd.CombinedOutput()
	if err != nil {
		panic(string(out))
	}
	os.WriteFile(outPath, out, 0644)
}
