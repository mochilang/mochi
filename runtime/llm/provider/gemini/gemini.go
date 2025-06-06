package gemini

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

// Gemini provider using the Google Generative Language REST API.

type provider struct{}

type conn struct {
	opts       llm.Options
	key        string
	baseURL    string
	httpClient *http.Client
}

func init() { llm.Register("gemini", provider{}) }

func (provider) Open(dsn string, opts llm.Options) (llm.Conn, error) {
	base := "https://generativelanguage.googleapis.com/v1beta"
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
		key = os.Getenv("GEMINI_API_KEY")
	}
	if key == "" {
		return nil, errors.New("gemini: missing api_key")
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
		return nil, fmt.Errorf("gemini: %s", b)
	}
	var r struct {
		Candidates []struct {
			Content struct {
				Parts []struct {
					Text string `json:"text"`
				} `json:"parts"`
			} `json:"content"`
			FinishReason string `json:"finishReason"`
		} `json:"candidates"`
		Usage struct {
			PromptTokens     int `json:"promptTokenCount"`
			CompletionTokens int `json:"candidateTokenCount"`
			TotalTokens      int `json:"totalTokenCount"`
		} `json:"usageMetadata"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&r); err != nil {
		return nil, err
	}
	if len(r.Candidates) == 0 {
		return nil, errors.New("gemini: empty response")
	}
	cand := r.Candidates[0]
	text := ""
	if len(cand.Content.Parts) > 0 {
		text = cand.Content.Parts[0].Text
	}
	msg := llm.Message{Role: "assistant", Content: text}
	usage := &llm.Usage{PromptTokens: r.Usage.PromptTokens, CompletionTokens: r.Usage.CompletionTokens, TotalTokens: r.Usage.TotalTokens}
	return &llm.ChatResponse{Message: msg, StopReason: cand.FinishReason, Usage: usage}, nil
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
		return nil, fmt.Errorf("gemini: %s", b)
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
			Candidates []struct {
				Content struct {
					Parts []struct {
						Text string `json:"text"`
					} `json:"parts"`
				} `json:"content"`
				FinishReason string `json:"finishReason"`
			} `json:"candidates"`
		}
		if err := json.Unmarshal([]byte(data), &r); err != nil {
			continue
		}
		if len(r.Candidates) == 0 {
			continue
		}
		cand := r.Candidates[0]
		text := ""
		if len(cand.Content.Parts) > 0 {
			text = cand.Content.Parts[0].Text
		}
		chunk := &llm.Chunk{Content: text}
		if cand.FinishReason != "" {
			chunk.Done = true
		}
		return chunk, nil
	}
}

func (s *stream) Close() error { return s.body.Close() }

func (c *conn) Embed(ctx context.Context, req llm.EmbedRequest) (*llm.EmbedResponse, error) {
	return nil, errors.New("gemini: embedding not supported")
}

func (c *conn) doRequest(ctx context.Context, req llm.ChatRequest) (*http.Response, error) {
	model := req.Model
	if model == "" {
		model = c.opts.Model
	}
	if model == "" {
		model = "models/gemini-pro"
	}
	payload := map[string]any{
		"contents": convertMessages(req.Messages),
	}

	params := map[string]any{}
	for k, v := range c.opts.Params {
		params[k] = v
	}
	for k, v := range req.Params {
		params[k] = v
	}
	gc := map[string]any{}
	for k, v := range params {
		switch k {
		case "temperature":
			gc["temperature"] = v
		case "top_p":
			gc["topP"] = v
		case "max_tokens":
			gc["maxOutputTokens"] = v
		case "stop":
			gc["stopSequences"] = v
		default:
			gc[k] = v
		}
	}
	if len(gc) > 0 {
		payload["generationConfig"] = gc
	}
	if req.Stream {
		payload["stream"] = true
	}
	b, err := json.Marshal(payload)
	if err != nil {
		return nil, err
	}
	url := fmt.Sprintf("%s/%s:generateContent?key=%s", c.baseURL, model, c.key)
	if req.Stream {
		url = fmt.Sprintf("%s/%s:streamGenerateContent?key=%s", c.baseURL, model, c.key)
	}
	httpReq, err := http.NewRequestWithContext(ctx, "POST", url, bytes.NewReader(b))
	if err != nil {
		return nil, err
	}
	httpReq.Header.Set("Content-Type", "application/json")
	return c.httpClient.Do(httpReq)
}

func convertMessages(msgs []llm.Message) []map[string]any {
	out := make([]map[string]any, 0, len(msgs))
	for _, m := range msgs {
		out = append(out, map[string]any{
			"role":  m.Role,
			"parts": []map[string]string{{"text": m.Content}},
		})
	}
	return out
}
