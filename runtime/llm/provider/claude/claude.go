package claude

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strings"

	"mochi/runtime/llm"
)

// Claude (Anthropic) provider using REST API.

type provider struct{}

type conn struct {
	opts       llm.Options
	key        string
	baseURL    string
	version    string
	httpClient *http.Client
}

func init() { llm.Register("claude", provider{}) }

func (provider) Open(dsn string, opts llm.Options) (llm.Conn, error) {
	base := "https://api.anthropic.com/v1"
	ver := "2023-06-01"
	var key string
	if dsn != "" {
		u, err := url.Parse(dsn)
		if err != nil {
			return nil, err
		}
		if u.Scheme != "" {
			base = u.Scheme + "://" + u.Host + u.Path
		} else if u.Host != "" {
			base = "https://" + u.Host + u.Path
		} else if u.Path != "" {
			base = u.Path
		}
		q := u.Query()
		key = q.Get("api_key")
		if v := q.Get("version"); v != "" {
			ver = v
		}
	}
	if key == "" {
		return nil, errors.New("claude: missing api_key")
	}
	return &conn{opts: opts, key: key, baseURL: base, version: ver, httpClient: http.DefaultClient}, nil
}

func (c *conn) Close() error { return nil }

func (c *conn) Chat(ctx context.Context, req llm.ChatRequest) (*llm.ChatResponse, error) {
	resp, err := c.doRequest(ctx, req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		b, _ := io.ReadAll(resp.Body)
		return nil, fmt.Errorf("claude: %s", b)
	}
	var r struct {
		ID      string `json:"id"`
		Model   string `json:"model"`
		Role    string `json:"role"`
		Content []struct {
			Text string `json:"text"`
		} `json:"content"`
		StopReason string `json:"stop_reason"`
		Usage      struct {
			InputTokens  int `json:"input_tokens"`
			OutputTokens int `json:"output_tokens"`
		} `json:"usage"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&r); err != nil {
		return nil, err
	}
	text := ""
	if len(r.Content) > 0 {
		text = r.Content[0].Text
	}
	msg := llm.Message{Role: "assistant", Content: text}
	usage := &llm.Usage{PromptTokens: r.Usage.InputTokens, CompletionTokens: r.Usage.OutputTokens, TotalTokens: r.Usage.InputTokens + r.Usage.OutputTokens}
	return &llm.ChatResponse{Message: msg, Model: r.Model, StopReason: r.StopReason, Usage: usage}, nil
}

func (c *conn) ChatStream(ctx context.Context, req llm.ChatRequest) (llm.Stream, error) {
	req.Stream = true
	resp, err := c.doRequest(ctx, req)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode != http.StatusOK {
		b, _ := io.ReadAll(resp.Body)
		resp.Body.Close()
		return nil, fmt.Errorf("claude: %s", b)
	}
	return &stream{r: bufio.NewReader(resp.Body), body: resp.Body}, nil
}

type stream struct {
	r    *bufio.Reader
	body io.ReadCloser
}

func (s *stream) Recv() (*llm.Chunk, error) {
	for {
		line, err := s.r.ReadString('\n')
		if err != nil {
			return nil, err
		}
		line = strings.TrimSpace(line)
		if line == "" || !strings.HasPrefix(line, "data:") {
			continue
		}
		data := strings.TrimSpace(strings.TrimPrefix(line, "data:"))
		if data == "[DONE]" {
			return &llm.Chunk{Done: true}, nil
		}
		var r struct {
			Type  string `json:"type"`
			Delta struct {
				Text       string `json:"text"`
				StopReason string `json:"stop_reason"`
			} `json:"delta"`
		}
		if err := json.Unmarshal([]byte(data), &r); err != nil {
			continue
		}
		if r.Type == "message_delta" && r.Delta.StopReason != "" {
			return &llm.Chunk{Done: true}, nil
		}
		if r.Type == "content_block_delta" {
			return &llm.Chunk{Content: r.Delta.Text}, nil
		}
	}
}

func (s *stream) Close() error { return s.body.Close() }

func (c *conn) doRequest(ctx context.Context, req llm.ChatRequest) (*http.Response, error) {
	model := req.Model
	if model == "" {
		model = c.opts.Model
	}
	payload := map[string]any{
		"model":    model,
		"messages": convertMessages(req.Messages),
	}

	params := map[string]any{}
	for k, v := range c.opts.Params {
		params[k] = v
	}
	for k, v := range req.Params {
		params[k] = v
	}
	for k, v := range params {
		switch k {
		case "stop":
			payload["stop_sequences"] = v
		default:
			payload[k] = v
		}
	}
	if req.Stream {
		payload["stream"] = true
	}
	if len(req.Tools) > 0 {
		payload["tools"] = req.Tools
	}
	if req.ToolChoice != "" {
		payload["tool_choice"] = req.ToolChoice
	}
	b, err := json.Marshal(payload)
	if err != nil {
		return nil, err
	}
	httpReq, err := http.NewRequestWithContext(ctx, "POST", c.baseURL+"/messages", bytes.NewReader(b))
	if err != nil {
		return nil, err
	}
	httpReq.Header.Set("Content-Type", "application/json")
	httpReq.Header.Set("X-API-Key", c.key)
	httpReq.Header.Set("anthropic-version", c.version)
	return c.httpClient.Do(httpReq)
}

func convertMessages(msgs []llm.Message) []map[string]any {
	out := make([]map[string]any, 0, len(msgs))
	for _, m := range msgs {
		out = append(out, map[string]any{"role": m.Role, "content": m.Content})
	}
	return out
}
