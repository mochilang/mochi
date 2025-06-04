package mcp

import (
	"context"
	_ "embed"
	"fmt"
	"os"
	"strings"

	"mochi/interpreter"
	"mochi/parser"
	"mochi/types"
)

// Register registers all available Mochi tools to the MCP server.
func Register(s *Server) {
	registerEval(s)
	registerCheatsheet(s)
}

// registerEval registers the "mochi_eval" tool.
func registerEval(s *Server) {
	tool := Tool{Name: "mochi_eval", Description: "Evaluate Mochi source code and return the result."}

	s.AddTool(tool, func(ctx context.Context, args map[string]any) (string, error) {
		code, _ := args["code"].(string)
		output, err := runProgram(code)
		return output, err
	})
}

// registerCheatsheet registers the "mochi_cheatsheet" tool.
func registerCheatsheet(s *Server) {
	tool := Tool{Name: "mochi_cheatsheet", Description: "Return the full Mochi language cheatsheet."}

	s.AddTool(tool, func(ctx context.Context, _ map[string]any) (string, error) {
		return cheatsheet, nil
	})
}

// runProgram parses, type-checks, and executes the given Mochi source code.
// It returns captured output as a string and a possible error.
func runProgram(source string) (string, error) {
	output := &strings.Builder{}

	prog, err := parser.ParseString(source)
	if err != nil {
		return output.String(), fmt.Errorf("parse error: %w, output: %s", err, output.String())
	}

	env := types.NewEnv(nil)
	typeErrors := types.Check(prog, env)
	if len(typeErrors) > 0 {
		return output.String(), fmt.Errorf("type error (%d issue(s)): %s", len(typeErrors), output.String())
	}

	interp := interpreter.New(prog, env)
	interp.Env().SetWriter(output)

	if err := interp.Run(); err != nil {
		return output.String(), fmt.Errorf("runtime error: %w: output = %s", err, output.String())
	}

	return output.String(), nil
}

// ServeStdio starts the MCP server using stdio (for Claude/GPT compatibility).
func ServeStdio() error {
	s := NewServer("mochi", "0.2.2")
	Register(s)
	return s.Serve(context.Background(), os.Stdin, os.Stdout)
}

//go:embed cheatsheet.mochi
var cheatsheet string
