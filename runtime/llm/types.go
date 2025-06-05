package llm

import "context"

// Message represents a chat message exchanged with the model.
type Message struct {
	Role     string    `json:"role"`
	Content  string    `json:"content"`
	ToolCall *ToolCall `json:"tool_call,omitempty"`
}

// ToolCall represents a request from the model to execute a tool.
type ToolCall struct {
	ID   string         `json:"id"`
	Name string         `json:"name"`
	Args map[string]any `json:"args"`
}

// Tool defines a function that can be called by the model.
type Tool struct {
	Name        string         `json:"name"`
	Description string         `json:"description"`
	Parameters  map[string]any `json:"parameters"`
}

// Usage tracks token usage information when provided by the provider.
type Usage struct {
	PromptTokens     int `json:"prompt_tokens"`
	CompletionTokens int `json:"completion_tokens"`
	TotalTokens      int `json:"total_tokens"`
}

// ChatResponse is a single response from the model.
type ChatResponse struct {
	Message    Message `json:"message"`
	Model      string  `json:"model"`
	StopReason string  `json:"stop_reason"`
	Usage      *Usage  `json:"usage,omitempty"`
}

// Chunk represents a streamed piece of a response.
type Chunk struct {
	Content   string     `json:"content"`
	ToolCalls []ToolCall `json:"tool_calls,omitempty"`
	Done      bool       `json:"done"`
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
	// Open initializes a new connection using the provider specific DSN.
	// The DSN follows the form BASE_URL?api_key=KEY&opt=value.
	Open(dsn string, opts Options) (Conn, error)
}
