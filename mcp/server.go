package mcp

import (
	"context"
	_ "embed"
	"fmt"
	"github.com/fatih/color"
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
	start := time.Now()
	output := &strings.Builder{}
	status := "ok"
	errMsg := ""

	sessionID := getSessionID()
	agent := getAgent()

	defer func() {
		db.LogRun(ctx, &db.RunModel{
			SessionID: sessionID,
			Agent:     agent,
			File:      "",
			Source:    source,
			Status:    status,
			Error:     errMsg,
			Duration:  time.Since(start),
		})
	}()

	// Step 1: Parse
	prog, err := parser.ParseString(source)
	if err != nil {
		status = "parse_error"
		fmt.Fprintf(output, "❌ Parse Error\n\n  → %v\n", err)
		errMsg = err.Error()
		return "", fmt.Errorf(output.String())
	}

	// Step 2: Type Check
	env := types.NewEnv(nil)
	typeErrors := types.Check(prog, env)
	if len(typeErrors) > 0 {
		status = "type_error"
		fmt.Fprintln(output, "❌ Type Check Failed\n")
		for i, e := range typeErrors {
			fmt.Fprintf(output, "  %2d. %v\n", i+1, e)
		}
		errMsg = fmt.Sprintf("%d type issues", len(typeErrors))
		return "", fmt.Errorf(output.String())
	}

	// Step 3: Runtime
	interp := interpreter.New(prog, env)
	interp.Env().SetWriter(output)
	if err := interp.Run(); err != nil {
		status = "runtime_error"
		fmt.Fprintf(output, "❌ Runtime Error\n\n  → %v\n", err)
		errMsg = err.Error()
		return "", fmt.Errorf(output.String())
	}

	return output.String(), nil
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
	color.NoColor = true // important for non-TTY environments like Claude/GPT
	s := server.NewMCPServer("mochi", "0.2.10")
	Register(s)
	return server.ServeStdio(s)
}

//go:embed cheatsheet.mochi
var cheatsheet string
