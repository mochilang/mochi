package mlcode

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

// RoundTripRun compiles the Mochi source file to OCaml, executes the
// resulting binary and compares the output with executing the program
// using the Mochi VM.
func RoundTripRun(src string) error {
	prog, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}
	code, err := New(env).Compile(prog)
	if err != nil {
		return fmt.Errorf("compile error: %w", err)
	}

	tmpDir, err := os.MkdirTemp("", "ocamlprog")
	if err != nil {
		return err
	}
	defer os.RemoveAll(tmpDir)
	mlfile := filepath.Join(tmpDir, "prog.ml")
	if err := os.WriteFile(mlfile, code, 0644); err != nil {
		return err
	}
	exe := filepath.Join(tmpDir, "prog")
	cmdName := "ocamlc"
	args := []string{mlfile, "-o", exe}
	if _, err := exec.LookPath("ocamlfind"); err == nil {
		cmdName = "ocamlfind"
		args = []string{"ocamlc", "-package", "yojson,unix", "-linkpkg", mlfile, "-o", exe}
	}
	if out, err := exec.Command(cmdName, args...).CombinedOutput(); err != nil {
		return fmt.Errorf("ocamlc error: %w\n%s", err, out)
	}

	var inData io.Reader
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		inData = bytes.NewReader(data)
	}
	cmd := exec.Command(exe)
	cmd.Dir = findRepoRoot()
	if inData != nil {
		cmd.Stdin = inData
	}
	ocamlOut, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("run error: %w\n%s", err, ocamlOut)
	}
	ocamlRes := strings.TrimSpace(string(ocamlOut))

	p, err := vm.Compile(prog, env)
	if err != nil {
		return fmt.Errorf("vm compile error: %w", err)
	}
	var vmIn io.Reader
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		vmIn = bytes.NewReader(data)
	}
	var vmBuf bytes.Buffer
	m := vm.NewWithIO(p, vmIn, &vmBuf)
	if err := m.Run(); err != nil {
		if ve, ok := err.(*vm.VMError); ok {
			return fmt.Errorf("vm run error:\n%s", ve.Format(p))
		}
		return fmt.Errorf("vm run error: %v", err)
	}
	vmRes := strings.TrimSpace(vmBuf.String())

	if ocamlRes != vmRes {
		return fmt.Errorf("output mismatch\n-- ocaml --\n%s\n-- vm --\n%s", ocamlRes, vmRes)
	}
	return nil
}

func findRepoRoot() string {
	dir, _ := os.Getwd()
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}
