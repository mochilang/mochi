package ollama

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"

	"mochi/runtime/llm"
)

// Ollama provider using the REST API.

type provider struct{}

type conn struct {
	opts       llm.Options
	baseURL    string
	httpClient *http.Client
}

func init() { llm.Register("ollama", provider{}) }

func (provider) Open(opts llm.Options) (llm.Conn, error) {
	base := os.Getenv("OLLAMA_BASE_URL")
	if base == "" {
		base = "http://localhost:11434/api"
	}
	return &conn{opts: opts, baseURL: base, httpClient: http.DefaultClient}, nil
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
		return nil, fmt.Errorf("ollama: %s", b)
	}
	var r struct {
		Model   string `json:"model"`
		Message struct {
			Role    string `json:"role"`
			Content string `json:"content"`
		} `json:"message"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&r); err != nil {
		return nil, err
	}
	msg := llm.Message{Role: r.Message.Role, Content: r.Message.Content}
	return &llm.ChatResponse{Message: msg, Model: r.Model}, nil
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
		return nil, fmt.Errorf("ollama: %s", b)
	}
	return &stream{scan: bufio.NewScanner(resp.Body), body: resp.Body}, nil
}

type stream struct {
	scan *bufio.Scanner
	body io.ReadCloser
}

func (s *stream) Recv() (*llm.Chunk, error) {
	if !s.scan.Scan() {
		if err := s.scan.Err(); err != nil {
			return nil, err
		}
		return &llm.Chunk{Done: true}, io.EOF
	}
	var r struct {
		Message struct {
			Content string `json:"content"`
		} `json:"message"`
		Done bool `json:"done"`
	}
	if err := json.Unmarshal(s.scan.Bytes(), &r); err != nil {
		return nil, err
	}
	if r.Done {
		return &llm.Chunk{Done: true}, nil
	}
	return &llm.Chunk{Content: r.Message.Content}, nil
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
	if req.Stream {
		payload["stream"] = true
	}
	b, err := json.Marshal(payload)
	if err != nil {
		return nil, err
	}
	httpReq, err := http.NewRequestWithContext(ctx, "POST", c.baseURL+"/chat", bytes.NewReader(b))
	if err != nil {
		return nil, err
	}
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
