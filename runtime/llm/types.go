package llm

import "context"

// Message represents a chat message exchanged with the model.
type Message struct {
	Role    string
	Content string
	// Optional tool call returned by the model.
	ToolCall *ToolCall
}

// ToolCall represents a request from the model to execute a tool.
type ToolCall struct {
	ID   string
	Name string
	Args map[string]any
}

// Tool defines a function that can be called by the model.
type Tool struct {
	Name        string
	Description string
	Parameters  map[string]any
}

// Usage tracks token usage information when provided by the provider.
type Usage struct {
	PromptTokens     int
	CompletionTokens int
	TotalTokens      int
}

// ChatResponse is a single response from the model.
type ChatResponse struct {
	Message    Message
	Model      string
	StopReason string
	Usage      *Usage
}

// Chunk represents a streamed piece of a response.
type Chunk struct {
	Content   string
	ToolCalls []ToolCall
	Done      bool
}

// Stream yields chunks until completion or error.
type Stream interface {
	Recv() (*Chunk, error)
	Close() error
}

// Conn is implemented by providers to handle the actual communication
// with the backing LLM service.
type Conn interface {
	Chat(ctx context.Context, req ChatRequest) (*ChatResponse, error)
	ChatStream(ctx context.Context, req ChatRequest) (Stream, error)
	Close() error
}

// Provider opens new connections for a given configuration.
type Provider interface {
	Open(opts Options) (Conn, error)
}
