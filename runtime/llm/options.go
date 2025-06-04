package llm

// Options are used when opening a connection to a provider.
type Options struct {
	Model          string
	Temperature    float64
	MaxTokens      int
	Tools          []Tool
	ToolChoice     string
	ResponseFormat string
}

// ChatRequest contains the messages and per-call options sent to the provider.
type ChatRequest struct {
	Messages []Message
	Options
	Stream bool
}

// Option is a functional option for chat requests.
type Option func(*ChatRequest)

// WithModel sets the model name.
func WithModel(model string) Option { return func(r *ChatRequest) { r.Model = model } }

// WithTemperature sets the sampling temperature.
func WithTemperature(t float64) Option { return func(r *ChatRequest) { r.Temperature = t } }

// WithMaxTokens limits the number of completion tokens.
func WithMaxTokens(n int) Option { return func(r *ChatRequest) { r.MaxTokens = n } }

// WithTools declares available tools for the request.
func WithTools(tools []Tool) Option { return func(r *ChatRequest) { r.Tools = tools } }

// WithToolChoice specifies which tool should be used.
func WithToolChoice(choice string) Option { return func(r *ChatRequest) { r.ToolChoice = choice } }

// WithResponseFormat requests structured output.
func WithResponseFormat(format string) Option {
	return func(r *ChatRequest) { r.ResponseFormat = format }
}

// WithStream enables streaming responses.
func WithStream() Option { return func(r *ChatRequest) { r.Stream = true } }
