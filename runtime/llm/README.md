# LLM Runtime

The `llm` package provides a simple, provider‑agnostic client for large language models.
It is inspired by Go's `database/sql` package and lets you swap back‑end providers
without changing application code.

```go
import "mochi/runtime/llm"
```

## Opening a Client

Create a client by calling `llm.Open` with the provider name, a DSN string and
optional defaults:

```go
client, err := llm.Open("openai",
    "https://api.openai.com/v1?api_key=sk-...",
    llm.Options{Model: "gpt-3.5-turbo"},
)
if err != nil {
    // handle error
}
defer client.Close()
```

If no explicit client is created, `llm` initializes a package-level default
client using the `LLM_PROVIDER`, `LLM_DSN` and `LLM_MODEL` environment
variables. The provider defaults to `echo` which simply returns the last user
message when `LLM_PROVIDER` is not set. Helper functions `llm.Chat` and
`llm.ChatStream` use this client.

### Chatting

Send a set of messages and receive a single response:

```go
resp, err := client.Chat(ctx, []llm.Message{
    {Role: "user", Content: "hello"},
}, llm.WithTemperature(0.7))
```

Streaming works similarly:

```go
stream, err := client.ChatStream(ctx, msgs)
for {
    ch, err := stream.Recv()
    if err != nil { /* handle */ }
    if ch.Done { break }
    fmt.Print(ch.Content)
}
```

### Embeddings

Request an embedding vector for a piece of text:

```go
vec, err := client.Embed(ctx, "hello world")
```

## Registering a Provider

A provider implements the `llm.Conn` interface and exposes an `Open` method.
Register it during initialization:

```go
type myProvider struct{}

func (myProvider) Open(dsn string, opts llm.Options) (llm.Conn, error) {
    // parse DSN and return your connection implementation
}

func init() { llm.Register("my", myProvider{}) }
```

`llm.Conn` must implement `Chat`, `ChatStream` and `Close`.

## Options

`Options` passed to `llm.Open` supply defaults for each request:

```go
type Options struct {
    Model          string
    Params         map[string]any
    Tools          []llm.Tool
    ToolChoice     string
    ResponseFormat *llm.ResponseFormat
}
```

`ResponseFormat` specifies structured output requirements. Set `Type` to
`"json_object"` to force JSON output and optionally provide a JSON Schema in
`Schema`. When requesting JSON output, ensure your prompt explicitly instructs
the model to produce JSON.

### Optional Fields

Requests accept arbitrary parameters stored in a `Params` map. Common fields
include `temperature`, `top_p`, `max_tokens` and `stop`. Convenience helpers
like `llm.WithTemperature`, `llm.WithTopP`, `llm.WithMaxTokens` and
`llm.WithStop` set these parameters, but you can pass any supported option using
`llm.WithParam` or `llm.WithParams`.

## DSN Format

Providers are configured through a DSN of the form:

```
BASE_URL?api_key=KEY&opt=value
```

The base URL may be omitted for providers with sensible defaults. Query
parameters are provider specific. Below are examples for the built‑in
providers.

| Provider | Sample DSN | Notes |
|----------|------------|-------|
| openai   | `https://api.openai.com/v1?api_key=sk-...` | base optional, defaults to `https://api.openai.com/v1` |
| echo     | `` | built-in provider that echoes the last user message |
| claude   | `https://api.anthropic.com/v1?api_key=...&version=2023-06-01` | `version` defaults to `2023-06-01` |
| cohere   | `https://api.cohere.ai/v1?api_key=...` | base optional |
| gemini   | `https://generativelanguage.googleapis.com/v1beta?api_key=...` | base optional |
| grok     | `https://api.grok.x.ai/v1?api_key=...` | base optional |
| mistral  | `https://api.mistral.ai/v1?api_key=...` | base optional |
| llamacpp | `http://localhost:8080/v1` | no API key |
| ollama   | `http://localhost:11434/api` | no API key |
| chutes   | `https://host.example/v1?api_token=...` | base and `api_token` required |

Use the appropriate provider name with `llm.Open` to select one of these
implementations.

### Supported Models

Each provider exposes different models. Common options include:

| Provider | Example Models |
|----------|----------------|
| openai   | `gpt-3.5-turbo`, `gpt-4o` |
| claude   | `claude-3-opus`, `claude-3-sonnet` |
| cohere   | `command-r`, `command-r-plus` |
| gemini   | `models/gemini-pro` (default) |
| grok     | `grok-1`, `grok-1-hd` |
| mistral  | `mistral-small`, `mistral-medium` |
| llamacpp | any local model name |
| ollama   | any local model name |
| chutes   | any model exposed by the service |

### Provider Documentation

Links to official API docs for each provider:

- [OpenAI](https://platform.openai.com/docs/api-reference/chat)
- [Claude](https://docs.anthropic.com/claude/reference/messages_post)
- [Cohere](https://docs.cohere.com/reference/chat)
- [Gemini](https://ai.google.dev/api/rest)
- [Grok](https://docs.x.ai)
- [Mistral](https://docs.mistral.ai)
- [llama.cpp](https://github.com/ggerganov/llama.cpp/tree/master/examples/server#rest-api)
- [Ollama](https://github.com/jmorganca/ollama/blob/main/docs/api.md)
- [Chutes](https://github.com/chutes-dev/chutes)
