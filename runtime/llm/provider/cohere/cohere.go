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
	"net/url"
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

func (provider) Open(dsn string, opts llm.Options) (llm.Conn, error) {
	base := "https://api.cohere.ai/v1"
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
		key = os.Getenv("COHERE_API_KEY")
	}
	if key == "" {
		return nil, errors.New("cohere: missing api_key")
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
	params := map[string]any{}
	for k, v := range c.opts.Params {
		params[k] = v
	}
	for k, v := range req.Params {
		params[k] = v
	}
	for k, v := range params {
		switch k {
		case "top_p":
			payload["p"] = v
		case "stop":
			payload["stop_sequences"] = v
		default:
			payload[k] = v
		}
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
