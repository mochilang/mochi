//go:build archived

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	gocode "mochi/compiler/x/go"
	"mochi/parser"
	"mochi/types"
)

func main() {
	files, err := filepath.Glob("tests/vm/valid/*.mochi")
	if err != nil {
		fmt.Fprintln(os.Stderr, "glob error:", err)
		os.Exit(1)
	}
	success := 0
	for _, src := range files {
		if err := process(src); err != nil {
			fmt.Fprintln(os.Stderr, "❌", src, err)
		} else {
			success++
		}
	}
	fmt.Printf("%d/%d programs compiled and executed\n", success, len(files))
}

func process(src string) error {
	base := strings.TrimSuffix(filepath.Base(src), ".mochi")
	outDir := filepath.Join("tests", "machine", "x", "go")
	if err := os.MkdirAll(outDir, 0755); err != nil {
		return err
	}
	prog, err := parser.Parse(src)
	if err != nil {
		return writeErr(outDir, base, err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return writeErr(outDir, base, errs[0])
	}
	code, err := gocode.New(env).Compile(prog)
	if err != nil {
		return writeErr(outDir, base, err)
	}
	codePath := filepath.Join(outDir, base+".go")
	if err := os.WriteFile(codePath, code, 0644); err != nil {
		return err
	}
	cmd := exec.Command("go", "run", codePath)
	cmd.Env = append(os.Environ(), "GO111MODULE=on", "LLM_PROVIDER=echo")
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stdout
	if err := cmd.Run(); err != nil {
		return writeErr(outDir, base, fmt.Errorf("run error: %w\n%s", err, stdout.String()))
	}
	outPath := filepath.Join(outDir, base+".out")
	return os.WriteFile(outPath, stdout.Bytes(), 0644)
}

func writeErr(outDir, base string, err error) error {
	errPath := filepath.Join(outDir, base+".error")
	return os.WriteFile(errPath, []byte(err.Error()), 0644)
}
