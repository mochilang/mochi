package mcp

import (
	"context"
	_ "embed"
	"fmt"
	"strings"

	"github.com/mark3labs/mcp-go/mcp"
	"github.com/mark3labs/mcp-go/server"
	"mochi/interpreter"
	"mochi/parser"
	"mochi/types"
)

// Register registers all available Mochi tools to the MCP server.
func Register(s *server.MCPServer) {
	registerEval(s)
	registerCheatsheet(s)
}

// registerEval registers the "mochi_eval" tool.
func registerEval(s *server.MCPServer) {
	tool := mcp.NewTool("mochi_eval",
		mcp.WithDescription("Evaluate Mochi source code and return the result."),
		mcp.WithString("code", mcp.Required(), mcp.Description("The Mochi source code to evaluate.")),
	)

	s.AddTool(tool, func(ctx context.Context, req mcp.CallToolRequest) (*mcp.CallToolResult, error) {
		args, ok := req.Params.Arguments.(map[string]any)
		if !ok {
			return mcp.NewToolResultText("invalid arguments: expected map[string]any"), nil
		}

		code, ok := args["code"].(string)
		if !ok {
			return mcp.NewToolResultText("missing 'code' string parameter"), nil
		}

		output, err := runProgram(code)
		return mcp.NewToolResultText(output), err
	})
}

// registerCheatsheet registers the "mochi_cheatsheet" tool.
func registerCheatsheet(s *server.MCPServer) {
	tool := mcp.NewTool("mochi_cheatsheet",
		mcp.WithDescription("Return the full Mochi language cheatsheet."),
	)

	s.AddTool(tool, func(ctx context.Context, req mcp.CallToolRequest) (*mcp.CallToolResult, error) {
		return mcp.NewToolResultText(cheatsheet), nil
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
	s := server.NewMCPServer("mochi", "0.2.2")
	Register(s)
	return server.ServeStdio(s)
}

//go:embed cheatsheet.mochi
var cheatsheet string
