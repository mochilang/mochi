package llm

// ResponseFormat specifies the type and optional schema of the expected response.
type ResponseFormat struct {
	Type   string         `json:"type"`             // e.g., "json"
	Schema map[string]any `json:"schema,omitempty"` // optional schema definition
}

// Options define model selection, parameters, tool usage, and response formatting.
type Options struct {
	Model          string          `json:"model"`                     // model name, e.g., "gpt-4"
	Params         map[string]any  `json:"params,omitempty"`          // model-specific parameters like temperature, top_p
	Tools          []Tool          `json:"tools,omitempty"`           // available tools to call
	ToolChoice     string          `json:"tool_choice,omitempty"`     // name of the tool to call
	ResponseFormat *ResponseFormat `json:"response_format,omitempty"` // desired response format
}

// ChatRequest contains a series of messages and options for the LLM provider.
type ChatRequest struct {
	Messages []Message `json:"messages"` // chat history
	Options                              // embeds Options fields
	Stream   bool      `json:"stream"`   // whether to stream responses
}

// Option is a functional option for configuring ChatRequest.
type Option func(*ChatRequest)

// WithModel sets the model name.
func WithModel(model string) Option {
	return func(r *ChatRequest) { r.Model = model }
}

// WithParam sets a single generation parameter.
func WithParam(name string, value any) Option {
	return func(r *ChatRequest) {
		if r.Params == nil {
			r.Params = map[string]any{}
		}
		r.Params[name] = value
	}
}

// WithParams sets multiple generation parameters.
func WithParams(params map[string]any) Option {
	return func(r *ChatRequest) {
		if r.Params == nil {
			r.Params = map[string]any{}
		}
		for k, v := range params {
			r.Params[k] = v
		}
	}
}

// WithTemperature sets the sampling temperature.
func WithTemperature(t float64) Option { return WithParam("temperature", t) }

// WithTopP sets the nucleus sampling probability.
func WithTopP(p float64) Option { return WithParam("top_p", p) }

// WithMaxTokens sets the maximum number of generated tokens.
func WithMaxTokens(n int) Option { return WithParam("max_tokens", n) }

// WithStop sets custom stop sequences.
func WithStop(stop []string) Option { return WithParam("stop", stop) }

// WithTools sets the available tools for tool calling.
func WithTools(tools []Tool) Option {
	return func(r *ChatRequest) { r.Tools = tools }
}

// WithToolChoice sets the preferred tool to use during generation.
func WithToolChoice(choice string) Option {
	return func(r *ChatRequest) { r.ToolChoice = choice }
}

// WithResponseFormat sets the expected structured response format.
func WithResponseFormat(format ResponseFormat) Option {
	return func(r *ChatRequest) { r.ResponseFormat = &format }
}

// WithStream enables streaming mode for the response.
func WithStream() Option {
	return func(r *ChatRequest) { r.Stream = true }
}
