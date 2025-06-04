package grok

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

// Grok provider using the xAI API (OpenAI compatible).

type provider struct{}

type conn struct {
	opts       llm.Options
	key        string
	baseURL    string
	httpClient *http.Client
}

func init() { llm.Register("grok", provider{}) }

func (provider) Open(dsn string, opts llm.Options) (llm.Conn, error) {
	base := "https://api.grok.x.ai/v1"
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
		key = u.Query().Get("api_key")
	}
	if key == "" {
		return nil, errors.New("grok: missing api_key")
	}
	return &conn{opts: opts, key: key, baseURL: base, httpClient: http.DefaultClient}, nil
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
		return nil, fmt.Errorf("grok: %s", b)
	}
	var r struct {
		Model   string `json:"model"`
		Choices []struct {
			FinishReason string `json:"finish_reason"`
			Message      struct {
				Role    string `json:"role"`
				Content string `json:"content"`
			} `json:"message"`
		} `json:"choices"`
		Usage struct {
			PromptTokens     int `json:"prompt_tokens"`
			CompletionTokens int `json:"completion_tokens"`
			TotalTokens      int `json:"total_tokens"`
		} `json:"usage"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&r); err != nil {
		return nil, err
	}
	if len(r.Choices) == 0 {
		return nil, errors.New("grok: empty response")
	}
	ch := r.Choices[0]
	msg := llm.Message{Role: ch.Message.Role, Content: ch.Message.Content}
	usage := &llm.Usage{PromptTokens: r.Usage.PromptTokens, CompletionTokens: r.Usage.CompletionTokens, TotalTokens: r.Usage.TotalTokens}
	return &llm.ChatResponse{Message: msg, Model: r.Model, StopReason: ch.FinishReason, Usage: usage}, nil
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
		return nil, fmt.Errorf("grok: %s", b)
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
			Choices []struct {
				Delta struct {
					Content string `json:"content"`
				} `json:"delta"`
				FinishReason string `json:"finish_reason"`
			} `json:"choices"`
		}
		if err := json.Unmarshal([]byte(data), &r); err != nil {
			continue
		}
		if len(r.Choices) == 0 {
			continue
		}
		ch := r.Choices[0]
		chunk := &llm.Chunk{Content: ch.Delta.Content}
		if ch.FinishReason != "" {
			chunk.Done = true
		}
		return chunk, nil
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
	temp := req.Temperature
	if temp == 0 {
		temp = c.opts.Temperature
	}
	if temp != 0 {
		payload["temperature"] = temp
	}
	mt := req.MaxTokens
	if mt == 0 {
		mt = c.opts.MaxTokens
	}
	if mt != 0 {
		payload["max_tokens"] = mt
	}
	if req.Stream {
		payload["stream"] = true
	}
	b, err := json.Marshal(payload)
	if err != nil {
		return nil, err
	}
	httpReq, err := http.NewRequestWithContext(ctx, "POST", c.baseURL+"/chat/completions", bytes.NewReader(b))
	if err != nil {
		return nil, err
	}
	httpReq.Header.Set("Authorization", "Bearer "+c.key)
	httpReq.Header.Set("Content-Type", "application/json")
	return c.httpClient.Do(httpReq)
}

func convertMessages(msgs []llm.Message) []map[string]any {
	out := make([]map[string]any, 0, len(msgs))
	for _, m := range msgs {
		out = append(out, map[string]any{"role": m.Role, "content": m.Content})
	}
	return out
}
