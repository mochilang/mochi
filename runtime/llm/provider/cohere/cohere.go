package cohere

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"strings"

	"mochi/runtime/llm"
)

// Cohere provider using REST API.

type provider struct{}

type conn struct {
	opts       llm.Options
	key        string
	baseURL    string
	httpClient *http.Client
}

func init() { llm.Register("cohere", provider{}) }

func (provider) Open(opts llm.Options) (llm.Conn, error) {
	key := os.Getenv("COHERE_API_KEY")
	if key == "" {
		return nil, errors.New("cohere: missing COHERE_API_KEY")
	}
	base := os.Getenv("COHERE_BASE_URL")
	if base == "" {
		base = "https://api.cohere.ai/v1"
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
		return nil, fmt.Errorf("cohere: %s", b)
	}
	var r struct {
		Text  string `json:"text"`
		Model string `json:"model"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&r); err != nil {
		return nil, err
	}
	msg := llm.Message{Role: "assistant", Content: r.Text}
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
		return nil, fmt.Errorf("cohere: %s", b)
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
			Text       string `json:"text"`
			IsFinished bool   `json:"is_finished"`
		}
		if err := json.Unmarshal([]byte(data), &r); err != nil {
			continue
		}
		if r.IsFinished {
			return &llm.Chunk{Done: true}, nil
		}
		return &llm.Chunk{Content: r.Text}, nil
	}
}

func (s *stream) Close() error { return s.body.Close() }

func (c *conn) doRequest(ctx context.Context, req llm.ChatRequest) (*http.Response, error) {
	model := req.Model
	if model == "" {
		model = c.opts.Model
	}
	message, history := prepareMessages(req.Messages)
	payload := map[string]any{
		"model":   model,
		"message": message,
	}
	if len(history) > 0 {
		payload["chat_history"] = history
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
	httpReq, err := http.NewRequestWithContext(ctx, "POST", c.baseURL+"/chat", bytes.NewReader(b))
	if err != nil {
		return nil, err
	}
	httpReq.Header.Set("Authorization", "Bearer "+c.key)
	httpReq.Header.Set("Content-Type", "application/json")
	return c.httpClient.Do(httpReq)
}

func prepareMessages(msgs []llm.Message) (string, []map[string]string) {
	var history []map[string]string
	var last string
	for i, m := range msgs {
		role := strings.ToUpper(m.Role)
		if i == len(msgs)-1 {
			last = m.Content
		} else {
			if role == "ASSISTANT" {
				role = "CHATBOT"
			}
			history = append(history, map[string]string{"role": role, "message": m.Content})
		}
	}
	return last, history
}
