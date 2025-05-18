package parser_test

import (
	"fmt"
	"mochi/ast"
	"mochi/diagnostic"
	"mochi/golden"
	"mochi/parser"
	"testing"
)

func TestParser_ValidPrograms(t *testing.T) {
	diagnostic.NoColor = true
	golden.Run(t, "tests/parser/valid", ".mochi", ".golden", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("parse error: %w", err)
		}
		return []byte(ast.FromProgram(prog).String()), nil
	})
}

func TestParser_SyntaxErrors(t *testing.T) {
	diagnostic.NoColor = true
	golden.Run(t, "tests/parser/errors", ".mochi", ".err", func(src string) ([]byte, error) {
		_, err := parser.Parse(src)
		if err == nil {
			return nil, fmt.Errorf("expected error, got nil")
		}
		return []byte(err.Error()), nil
	})
}
