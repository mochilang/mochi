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
		Text      string `json:"text"`
		Model     string `json:"model"`
		Finish    string `json:"finish_reason"`
		ToolCalls []struct {
			Name       string         `json:"name"`
			Parameters map[string]any `json:"parameters"`
		} `json:"tool_calls"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&r); err != nil {
		return nil, err
	}
	msg := llm.Message{Role: "assistant", Content: r.Text}
	if len(r.ToolCalls) > 0 {
		tc := r.ToolCalls[0]
		msg.ToolCall = &llm.ToolCall{Name: tc.Name, Args: tc.Parameters}
	}
	return &llm.ChatResponse{Message: msg, Model: r.Model, StopReason: r.Finish}, nil
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

func (c *conn) Embed(ctx context.Context, req llm.EmbedRequest) (*llm.EmbedResponse, error) {
	return nil, errors.New("cohere: embedding not supported")
}

func (c *conn) doRequest(ctx context.Context, req llm.ChatRequest) (*http.Response, error) {
	model := req.Model
	if model == "" {
		model = c.opts.Model
	}
	message, history, results := prepareMessages(req.Messages)
	payload := map[string]any{
		"model":   model,
		"message": message,
	}
	if len(history) > 0 {
		payload["chat_history"] = history
	}
	if len(results) > 0 {
		payload["tool_results"] = results
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
	if len(req.Tools) > 0 {
		payload["tools"] = convertTools(req.Tools)
	}
	if req.ToolChoice != "" {
		payload["tool_choice"] = req.ToolChoice
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

func prepareMessages(msgs []llm.Message) (string, []map[string]string, []map[string]any) {
	var history []map[string]string
	var results []map[string]any
	var last string
	for i, m := range msgs {
		role := strings.ToUpper(m.Role)
		if m.Role == "assistant" {
			role = "CHATBOT"
		} else if m.Role == "tool" {
			results = append(results, map[string]any{
				"call": map[string]any{
					"name":       m.ToolCall.Name,
					"parameters": m.ToolCall.Args,
				},
				"outputs": []map[string]any{{"text": m.Content}},
			})
			continue
		}
		if i == len(msgs)-1 {
			last = m.Content
		} else {
			history = append(history, map[string]string{"role": role, "message": m.Content})
		}
	}
	return last, history, results
}

func convertTools(tools []llm.Tool) []map[string]any {
	out := make([]map[string]any, 0, len(tools))
	for _, t := range tools {
		params := cloneWithDescriptions(t.Parameters)
		out = append(out, map[string]any{
			"name":        t.Name,
			"description": t.Description,
			"parameters":  params,
		})
	}
	return out
}

func cloneWithDescriptions(m map[string]any) map[string]any {
	if m == nil {
		return nil
	}
	b, _ := json.Marshal(m)
	var out map[string]any
	json.Unmarshal(b, &out)
	addDescriptions(out)
	return out
}

func addDescriptions(v any) {
	switch node := v.(type) {
	case map[string]any:
		if props, ok := node["properties"].(map[string]any); ok {
			for _, pv := range props {
				if pm, ok := pv.(map[string]any); ok {
					if _, ok := pm["description"]; !ok {
						pm["description"] = ""
					}
					addDescriptions(pm)
				}
			}
		}
		if items, ok := node["items"]; ok {
			addDescriptions(items)
		}
	case []any:
		for _, item := range node {
			addDescriptions(item)
		}
	}
}
