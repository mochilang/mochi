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
client using the `LLM_PROVIDER` and `LLM_DSN` environment variables. The default
falls back to the built-in `echo` provider which simply returns the last user
message. Helper functions `llm.Chat` and `llm.ChatStream` use this client.

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
    Temperature    float64
    MaxTokens      int
    Tools          []llm.Tool
    ToolChoice     string
    ResponseFormat string
}
```

Per-call overrides use functional options such as `llm.WithModel`,
`llm.WithTemperature`, `llm.WithMaxTokens` and `llm.WithStream`.

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

