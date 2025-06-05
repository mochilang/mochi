package interpreter_test

import (
	"fmt"
	"mochi/golden"
	"mochi/interpreter"
	"mochi/parser"
	"mochi/runtime/llm"
	_ "mochi/runtime/llm/provider/echo"
	"mochi/types"
	"strings"
	"testing"
)

func TestInterpreter_ValidPrograms(t *testing.T) {
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

		out := &strings.Builder{}
		interp := interpreter.New(prog, typeEnv)
		interp.Env().SetWriter(out)
		if err := interp.Run(); err != nil {
			return nil, fmt.Errorf("❌ runtime error: %w", err)
		}
		return []byte(out.String()), nil
	})
}

func TestInterpreter_RuntimeErrors(t *testing.T) {
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

		out := &strings.Builder{}
		interp := interpreter.New(prog, typeEnv)
		interp.Env().SetWriter(out)
		err = interp.Run()
		if err == nil {
			return nil, fmt.Errorf("❌ expected runtime error, got none")
		}
		return []byte(err.Error()), nil
	})
}
