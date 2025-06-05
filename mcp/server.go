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
		mcp.WithString("filename", mcp.Description("Optional filename for logging/debugging.")),
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

		filename := ""
		if fname, ok := args["filename"].(string); ok {
			filename = fname
		}

		output, err := runProgram(ctx, code, filename)
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
func runProgram(ctx context.Context, source string, filename string) (string, error) {
	color.NoColor = true // Ensure plain output for MCP clients

	start := time.Now()
	output := &strings.Builder{}
	sessionID := getSessionID()
	agent := getAgent()

	logSuccess := func() {
		db.LogRun(ctx, &db.RunModel{
			SessionID: sessionID,
			Agent:     agent,
			File:      filename,
			Source:    source,
			Status:    "ok",
			Error:     "",
			Duration:  time.Since(start),
		})
	}

	logFailure := func(stage string) error {
		fullOutput := output.String()
		db.LogRun(ctx, &db.RunModel{
			SessionID: sessionID,
			Agent:     agent,
			File:      filename,
			Source:    source,
			Status:    stage,
			Error:     fullOutput,
			Duration:  time.Since(start),
		})
		return fmt.Errorf("%s", fullOutput)
	}

	// Step 1: Parse
	prog, err := parser.ParseString(source)
	if err != nil {
		fmt.Fprintf(output, "❌ Parse Error\n\n  → %v\n", err)
		return "", logFailure("parse_error")
	}

	// Step 2: Type Check
	env := types.NewEnv(nil)
	typeErrors := types.Check(prog, env)
	if len(typeErrors) > 0 {
		fmt.Fprintln(output, "❌ Type Check Failed")
		for i, e := range typeErrors {
			fmt.Fprintf(output, "  %2d. %v\n", i+1, e)
		}
		return "", logFailure("type_error")
	}

	// Step 3: Run
	interp := interpreter.New(prog, env)
	interp.Env().SetWriter(output)
	if err := interp.Run(); err != nil {
		fmt.Fprintf(output, "❌ Runtime Error\n\n  → %v\n", err)
		return "", logFailure("runtime_error")
	}

	logSuccess()
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
    s := server.NewMCPServer("mochi", "0.3.1")
	Register(s)
	return server.ServeStdio(s)
}

//go:embed cheatsheet.mochi
var cheatsheet string
