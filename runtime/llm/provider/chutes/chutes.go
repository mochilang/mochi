package chutes

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
	"os"
	"strings"

	"mochi/runtime/llm"
)

// Chutes provider using OpenAI-compatible REST API.

type provider struct{}

type conn struct {
	opts       llm.Options
	token      string
	baseURL    string
	httpClient *http.Client
}

func init() { llm.Register("chutes", provider{}) }

func (provider) Open(dsn string, opts llm.Options) (llm.Conn, error) {
	var key, base string
	if dsn != "" {
		u, err := url.Parse(dsn)
		if err != nil {
			return nil, err
		}
		base = u.Scheme + "://" + u.Host + u.Path
		key = u.Query().Get("api_token")
	}
	if base == "" {
		return nil, errors.New("chutes: missing base url")
	}
	if key == "" {
		key = os.Getenv("CHUTES_API_TOKEN")
	}
	if key == "" {
		return nil, errors.New("chutes: missing api_token")
	}
	return &conn{opts: opts, token: key, baseURL: base, httpClient: http.DefaultClient}, nil
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
		return nil, fmt.Errorf("chutes: %s", b)
	}
	var r struct {
		Model   string `json:"model"`
		Choices []struct {
			FinishReason string `json:"finish_reason"`
			Message      struct {
				Role      string `json:"role"`
				Content   string `json:"content"`
				ToolCalls []struct {
					ID       string `json:"id"`
					Type     string `json:"type"`
					Function struct {
						Name      string `json:"name"`
						Arguments string `json:"arguments"`
					} `json:"function"`
				} `json:"tool_calls"`
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
		return nil, errors.New("chutes: empty response")
	}
	ch := r.Choices[0]
	msg := llm.Message{Role: ch.Message.Role, Content: ch.Message.Content}
	if len(ch.Message.ToolCalls) > 0 {
		tc := ch.Message.ToolCalls[0]
		var args map[string]any
		json.Unmarshal([]byte(tc.Function.Arguments), &args)
		msg.ToolCall = &llm.ToolCall{ID: tc.ID, Name: tc.Function.Name, Args: args}
	}
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
		return nil, fmt.Errorf("chutes: %s", b)
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
					Content   string `json:"content"`
					ToolCalls []struct {
						ID       string `json:"id"`
						Type     string `json:"type"`
						Function struct {
							Name      string `json:"name"`
							Arguments string `json:"arguments"`
						} `json:"function"`
					} `json:"tool_calls"`
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
		d := r.Choices[0].Delta
		chunk := &llm.Chunk{Content: d.Content}
		for _, tc := range d.ToolCalls {
			var args map[string]any
			json.Unmarshal([]byte(tc.Function.Arguments), &args)
			chunk.ToolCalls = append(chunk.ToolCalls, llm.ToolCall{ID: tc.ID, Name: tc.Function.Name, Args: args})
		}
		if r.Choices[0].FinishReason != "" {
			chunk.Done = true
		}
		return chunk, nil
	}
}

func (s *stream) Close() error { return s.body.Close() }

func (c *conn) Embed(ctx context.Context, req llm.EmbedRequest) (*llm.EmbedResponse, error) {
	return nil, errors.New("chutes: embedding not supported")
}

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
		payload[k] = v
	}
	if len(req.Tools) > 0 {
		payload["tools"] = convertTools(req.Tools)
	}
	if req.ToolChoice != "" {
		payload["tool_choice"] = req.ToolChoice
	}
	format := req.ResponseFormat
	if format == nil {
		format = c.opts.ResponseFormat
	}
	if format != nil {
		payload["response_format"] = format
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
	httpReq.Header.Set("Authorization", "Bearer "+c.token)
	httpReq.Header.Set("Content-Type", "application/json")
	return c.httpClient.Do(httpReq)
}

func convertMessages(msgs []llm.Message) []map[string]any {
	out := make([]map[string]any, 0, len(msgs))
	for _, m := range msgs {
		mm := map[string]any{"role": m.Role}
		if m.Content != "" {
			mm["content"] = m.Content
		} else {
			mm["content"] = ""
		}
		if m.ToolCall != nil {
			args, _ := json.Marshal(m.ToolCall.Args)
			mm["tool_calls"] = []map[string]any{{
				"id":       m.ToolCall.ID,
				"type":     "function",
				"function": map[string]any{"name": m.ToolCall.Name, "arguments": string(args)},
			}}
		}
		out = append(out, mm)
	}
	return out
}

func convertTools(tools []llm.Tool) []map[string]any {
	out := make([]map[string]any, 0, len(tools))
	for _, t := range tools {
		out = append(out, map[string]any{
			"type": "function",
			"function": map[string]any{
				"name":        t.Name,
				"description": t.Description,
				"parameters":  t.Parameters,
			},
		})
	}
	return out
}
