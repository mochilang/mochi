package mcp

import (
	"context"
	_ "embed"
	"fmt"
	"github.com/google/uuid"
	"mochi/tools/db"
	"os"
	"strings"
	"time"

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

		output, err := runProgram(ctx, code)
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
func runProgram(ctx context.Context, source string) (string, error) {
	output := &strings.Builder{}
	start := time.Now()

	status := "ok"
	var errMsg string
	var runErr error

	prog, err := parser.ParseString(source)
	if err != nil {
		status = "parse_error"
		errMsg = err.Error()
		fmt.Fprintf(output, "Parse Error\n\n  %v\n", err)
		runErr = fmt.Errorf("parse error: %w", err)
	} else {
		env := types.NewEnv(nil)
		typeErrors := types.Check(prog, env)
		if len(typeErrors) > 0 {
			status = "type_error"
			errMsg = fmt.Sprintf("%d issues", len(typeErrors))
			fmt.Fprintf(output, "Type Check Failed\n\n")
			for i, e := range typeErrors {
				fmt.Fprintf(output, "  %2d. %v\n", i+1, e)
			}
			runErr = fmt.Errorf("type error: %v", errMsg)
		} else {
			interp := interpreter.New(prog, env)
			interp.Env().SetWriter(output)

			if err := interp.Run(); err != nil {
				status = "runtime_error"
				errMsg = err.Error()
				fmt.Fprintf(output, "Runtime Error\n\n  → %v\n", err)
				runErr = fmt.Errorf("runtime error: %w", err)
			}
		}
	}

	db.LogRun(ctx, &db.RunModel{
		SessionID: getSessionID(),
		Agent:     getAgent(),
		File:      "",
		Source:    source,
		Status:    status,
		Error:     errMsg,
		Duration:  time.Since(start),
	})

	return output.String(), runErr
}

func getSessionID() string {
	if sid := os.Getenv("MOCHI_SESSION"); sid != "" {
		return sid
	}
	return uuid.New().String()
}

func getAgent() string {
	if agent := os.Getenv("MOCHI_AGENT"); agent != "" {
		return agent
	}
	return "MCP"
}

// ServeStdio starts the MCP server using stdio (for Claude/GPT compatibility).
func ServeStdio() error {
	s := server.NewMCPServer("mochi", "0.2.2")
	Register(s)
	return server.ServeStdio(s)
}

//go:embed cheatsheet.mochi
var cheatsheet string
