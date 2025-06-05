package llm

// Options are used when opening a connection to a provider.
type Options struct {
	Model          string
	Params         map[string]any
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

// WithParam sets an arbitrary generation parameter.
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

// WithTopP sets nucleus sampling probability.
func WithTopP(p float64) Option { return WithParam("top_p", p) }

// WithMaxTokens limits the number of completion tokens.
func WithMaxTokens(n int) Option { return WithParam("max_tokens", n) }

// WithStop specifies stop sequences.
func WithStop(stop []string) Option { return WithParam("stop", stop) }

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
