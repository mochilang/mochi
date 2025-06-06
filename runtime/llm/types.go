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
	Embed(ctx context.Context, req EmbedRequest) (*EmbedResponse, error)
	Close() error
}

// Provider opens new connections for a given configuration.
type Provider interface {
	// Open initializes a new connection using the provider specific DSN.
	// The DSN follows the form BASE_URL?api_key=KEY&opt=value.
	Open(dsn string, opts Options) (Conn, error)
}

// EmbedRequest requests an embedding vector for the given text.
type EmbedRequest struct {
	Text      string         `json:"text"`
	Model     string         `json:"model,omitempty"`
	Params    map[string]any `json:"params,omitempty"`
	Normalize bool           `json:"normalize,omitempty"`
}

// EmbedResponse contains the returned embedding vector.
type EmbedResponse struct {
	Vector []float64 `json:"vector"`
	Model  string    `json:"model"`
}

// EmbedOption configures an EmbedRequest.
type EmbedOption func(*EmbedRequest)

// WithEmbedModel sets the embedding model name.
func WithEmbedModel(model string) EmbedOption {
	return func(r *EmbedRequest) { r.Model = model }
}

// WithEmbedParam sets a provider-specific parameter.
func WithEmbedParam(name string, value any) EmbedOption {
	return func(r *EmbedRequest) {
		if r.Params == nil {
			r.Params = map[string]any{}
		}
		r.Params[name] = value
	}
}

// WithEmbedNormalize controls L2 normalization of the returned vector.
func WithEmbedNormalize(v bool) EmbedOption {
	return func(r *EmbedRequest) { r.Normalize = v }
}
