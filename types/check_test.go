package types_test

import (
	"fmt"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
	"os"
	"testing"
)

func TestTypeChecker_Valid(t *testing.T) {
	if os.Getenv("RUN_TYPE_VALID") == "" {
		t.Skip("types valid tests disabled")
	}
	golden.Run(t, "tests/types/valid", ".mochi", ".golden", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}

		typeEnv := types.NewEnv(nil)
		errs := types.Check(prog, typeEnv)
		if len(errs) > 0 {
			var out string
			for i, err := range errs {
				out += fmt.Sprintf("  %2d. %v\n", i+1, err)
			}
			return []byte("❌ Type Check Failed\n" + out), fmt.Errorf("type errors: %d", len(errs))
		}

		return []byte("✅ Type Check Passed"), nil
	})
}

func TestTypeChecker_Errors(t *testing.T) {
	golden.Run(t, "tests/types/errors", ".mochi", ".err", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}

		typeEnv := types.NewEnv(nil)
		errs := types.Check(prog, typeEnv)
		if len(errs) == 0 {
			return nil, fmt.Errorf("expected type error, got none")
		}

		var out string
		for i, err := range errs {
			out += fmt.Sprintf("  %2d. %v\n", i+1, err)
		}
		return []byte(out), nil
	})
}
