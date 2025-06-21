package aws_test

import (
	"fmt"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/golden"
	"mochi/interpreter"
	"mochi/parser"
	"mochi/runtime/cloud/aws"
	"mochi/runtime/mod"
	"mochi/types"
)

func runProgram(t *testing.T, srcPath, baseURL string) ([]byte, error) {
	data, err := os.ReadFile(srcPath)
	if err != nil {
		return nil, err
	}
	src := strings.ReplaceAll(string(data), "http://localhost", baseURL)
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	modRoot, _ := mod.FindRoot(filepath.Dir(srcPath))
	out := &strings.Builder{}
	interp := interpreter.New(prog, env, modRoot)
	interp.Env().SetWriter(out)
	if err := interp.Run(); err != nil {
		return nil, fmt.Errorf("run: %w", err)
	}
	return []byte(out.String()), nil
}

func TestAWSLibrary(t *testing.T) {
	srv := httptest.NewServer(nil)
	server := aws.NewServer()
	srv.Config.Handler = server
	server.RegisterLambda("greeter", func(p any) any {
		if m, ok := p.(map[string]any); ok {
			if n, ok := m["name"].(string); ok {
				return "hi " + n
			}
		}
		return ""
	})
	defer srv.Close()

	golden.Run(t, "tests/cloud/aws", ".mochi", ".out", func(src string) ([]byte, error) {
		return runProgram(t, src, srv.URL)
	})
}
