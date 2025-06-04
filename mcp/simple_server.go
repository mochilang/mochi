package mcp

import (
	"bufio"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"strings"
)

type ToolHandler func(ctx context.Context, args map[string]any) (string, error)

// Server is a very small MCP server supporting tool calls over JSON-RPC.
type Server struct {
	name         string
	version      string
	instructions string
	tools        map[string]ToolHandler
}

func NewServer(name, version string) *Server {
	return &Server{name: name, version: version, tools: make(map[string]ToolHandler)}
}

func (s *Server) AddTool(t Tool, handler ToolHandler) {
	if s.tools == nil {
		s.tools = make(map[string]ToolHandler)
	}
	s.tools[t.Name] = handler
}

func (s *Server) SetInstructions(instr string) { s.instructions = instr }

func (s *Server) handleRequest(ctx context.Context, req JSONRPCRequest) JSONRPCResponse {
	switch req.Method {
	case "initialize":
		result := InitializeResult{
			ServerInfo:      Implementation{Name: s.name, Version: s.version},
			ProtocolVersion: ProtocolVersion,
			Capabilities:    map[string]any{"tools": map[string]any{}},
			Instructions:    s.instructions,
		}
		return JSONRPCResponse{JSONRPC: "2.0", ID: req.ID, Result: result}
	case "tools/list":
		list := make([]Tool, 0, len(s.tools))
		for name := range s.tools {
			list = append(list, Tool{Name: name})
		}
		return JSONRPCResponse{JSONRPC: "2.0", ID: req.ID, Result: ToolList{Tools: list}}
	case "tools/call":
		var params CallToolParams
		if err := json.Unmarshal(req.Params, &params); err != nil {
			return JSONRPCResponse{JSONRPC: "2.0", ID: req.ID, Error: &JSONRPCError{Code: -32602, Message: "invalid params"}}
		}
		handler, ok := s.tools[params.Name]
		if !ok {
			return JSONRPCResponse{JSONRPC: "2.0", ID: req.ID, Error: &JSONRPCError{Code: -32601, Message: "unknown tool"}}
		}
		out, err := handler(ctx, params.Arguments)
		res := CallToolResult{Content: []TextContent{{Type: "text", Text: out}}}
		if err != nil {
			res.IsError = true
			res.Content = []TextContent{{Type: "text", Text: out}}
		}
		return JSONRPCResponse{JSONRPC: "2.0", ID: req.ID, Result: res}
	default:
		return JSONRPCResponse{JSONRPC: "2.0", ID: req.ID, Error: &JSONRPCError{Code: -32601, Message: "method not found"}}
	}
}

func (s *Server) Serve(ctx context.Context, r io.Reader, w io.Writer) error {
	scanner := bufio.NewScanner(r)
	for scanner.Scan() {
		if err := ctx.Err(); err != nil {
			return err
		}
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		var req JSONRPCRequest
		if err := json.Unmarshal([]byte(line), &req); err != nil {
			resp := JSONRPCResponse{JSONRPC: "2.0", ID: nil, Error: &JSONRPCError{Code: -32700, Message: "parse error"}}
			data, _ := json.Marshal(resp)
			fmt.Fprintln(w, string(data))
			continue
		}
		resp := s.handleRequest(ctx, req)
		data, _ := json.Marshal(resp)
		fmt.Fprintln(w, string(data))
	}
	return scanner.Err()
}
