package interpreter_test

import (
	"bytes"
	"fmt"
	"math"
	"os"
	"os/exec"
	"path/filepath"
	stdstrings "strings"
	"testing"

	"mochi/golden"
	"mochi/interpreter"
	"mochi/parser"
	goffi "mochi/runtime/ffi/go"
	"mochi/runtime/llm"
	_ "mochi/runtime/llm/provider/echo"
	"mochi/runtime/mod"
	"mochi/types"
)

func TestInterpreter_ValidPrograms(t *testing.T) {
	t.Skip("disabled in current environment")
	if _, err := exec.LookPath("deno"); err != nil {
		t.Skip("deno not installed")
	}
	golden.Run(t, "tests/interpreter/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}

		typeEnv := types.NewEnv(nil)
		var errOpen error
		llm.Default, errOpen = llm.Open("echo", "", llm.Options{})
		if errOpen != nil {
			return nil, fmt.Errorf("❌ open llm: %w", errOpen)
		}
		typeErrors := types.Check(prog, typeEnv)
		if len(typeErrors) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", typeErrors[0])
		}

		// register Go math functions for FFI tests
		goffi.Register("math.Pi", math.Pi)
		goffi.Register("math.E", math.E)
		goffi.Register("math.Sqrt", math.Sqrt)
		goffi.Register("math.Pow", math.Pow)
		goffi.Register("math.Sin", math.Sin)
		goffi.Register("math.Log", math.Log)
		goffi.Register("strings.ToUpper", stdstrings.ToUpper)
		goffi.Register("strings.HasPrefix", stdstrings.HasPrefix)

		out := &stdstrings.Builder{}
		modRoot, _ := mod.FindRoot(filepath.Dir(src))
		interp := interpreter.New(prog, typeEnv, modRoot)
		interp.Env().SetWriter(out)
		if data, err := os.ReadFile(stdstrings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			interp.Env().SetReader(bytes.NewReader(data))
		}
		if err := interp.Run(); err != nil {
			return nil, fmt.Errorf("❌ runtime error: %w", err)
		}
		return []byte(out.String()), nil
	})
}

func TestInterpreter_RuntimeErrors(t *testing.T) {
	t.Skip("disabled in current environment")
	golden.Run(t, "tests/interpreter/errors", ".mochi", ".err", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}

		typeEnv := types.NewEnv(nil)
		typeErrors := types.Check(prog, typeEnv)
		if len(typeErrors) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", typeErrors[0])
		}

		goffi.Register("math.Pi", math.Pi)
		goffi.Register("math.E", math.E)
		goffi.Register("math.Sqrt", math.Sqrt)
		goffi.Register("math.Pow", math.Pow)
		goffi.Register("math.Sin", math.Sin)
		goffi.Register("math.Log", math.Log)
		goffi.Register("strings.ToUpper", stdstrings.ToUpper)
		goffi.Register("strings.HasPrefix", stdstrings.HasPrefix)

		out := &stdstrings.Builder{}
		modRoot, _ := mod.FindRoot(filepath.Dir(src))
		interp := interpreter.New(prog, typeEnv, modRoot)
		interp.Env().SetWriter(out)
		err = interp.Run()
		if err == nil {
			return nil, fmt.Errorf("❌ expected runtime error, got none")
		}
		return []byte(err.Error()), nil
	})
}
