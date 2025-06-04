package mcp

import "encoding/json"

// JSONRPCMessage is a generic JSON-RPC 2.0 message.
type JSONRPCMessage struct {
	JSONRPC string `json:"jsonrpc"`
}

type JSONRPCRequest struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      any             `json:"id"`
	Method  string          `json:"method"`
	Params  json.RawMessage `json:"params,omitempty"`
}

type JSONRPCResponse struct {
	JSONRPC string        `json:"jsonrpc"`
	ID      any           `json:"id"`
	Result  any           `json:"result,omitempty"`
	Error   *JSONRPCError `json:"error,omitempty"`
}

type JSONRPCError struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
}

const ProtocolVersion = "2025-03-26"

// Implementation describes the name and version of an MCP implementation.
type Implementation struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

// Tool describes a single callable tool.
type Tool struct {
	Name        string `json:"name"`
	Description string `json:"description,omitempty"`
}

// ToolList is the response for tools/list
type ToolList struct {
	Tools []Tool `json:"tools"`
}

// TextContent is simple textual response content.
type TextContent struct {
	Type string `json:"type"`
	Text string `json:"text"`
}

// CallToolParams defines arguments for tools/call
type CallToolParams struct {
	Name      string         `json:"name"`
	Arguments map[string]any `json:"arguments,omitempty"`
}

type CallToolRequest struct {
	Method string         `json:"method"`
	Params CallToolParams `json:"params"`
}

type CallToolResult struct {
	Content []TextContent `json:"content"`
	IsError bool          `json:"isError,omitempty"`
}

// InitializeResult is returned for initialize
type InitializeResult struct {
	ServerInfo      Implementation `json:"serverInfo"`
	ProtocolVersion string         `json:"protocolVersion"`
	Capabilities    map[string]any `json:"capabilities"`
	Instructions    string         `json:"instructions,omitempty"`
}
