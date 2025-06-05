package openai

import (
	"bytes"
	"context"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"testing"

	"mochi/golden"
	"mochi/runtime/llm"
)

func TestRequestResponseFormat(t *testing.T) {
	golden.Run(t, "tests/llm/response_format", ".txt", ".out", func(src string) ([]byte, error) {
		var captured bytes.Buffer
		srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			io.Copy(&captured, r.Body)
			r.Body.Close()
			w.Header().Set("Content-Type", "application/json")
			w.Write([]byte(`{"choices":[{"message":{"role":"assistant","content":"ok"}}],"model":"test"}`))
		}))
		defer srv.Close()

		cAny, err := provider{}.Open(srv.URL+"?api_key=test", llm.Options{})
		if err != nil {
			return nil, err
		}
		c := cAny.(*conn)
		c.httpClient = srv.Client()

		chatReq := llm.ChatRequest{Messages: []llm.Message{{Role: "user", Content: "hi"}},
			Options: llm.Options{ResponseFormat: &llm.ResponseFormat{Type: "json_object", Schema: map[string]any{"type": "object"}}}}

		_, err = c.Chat(context.Background(), chatReq)
		if err != nil {
			return nil, err
		}

		var r struct {
			ResponseFormat llm.ResponseFormat `json:"response_format"`
		}
		if err := json.Unmarshal(captured.Bytes(), &r); err != nil {
			return nil, err
		}
		out, err := json.Marshal(r.ResponseFormat)
		if err != nil {
			return nil, err
		}
		return out, nil
	})
}
