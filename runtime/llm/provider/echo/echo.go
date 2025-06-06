package echo

import (
	"context"
	"errors"

	"mochi/runtime/llm"
)

// Provider that echoes the last user message.
// Useful for testing and metrics collection without calling a real model.

type provider struct{}

type conn struct{ opts llm.Options }

type stream struct {
	content []rune
	i       int
}

func init() { llm.Register("echo", provider{}) }

func (provider) Open(dsn string, opts llm.Options) (llm.Conn, error) {
	return &conn{opts: opts}, nil
}

func (c *conn) Close() error { return nil }

func (c *conn) Chat(ctx context.Context, req llm.ChatRequest) (*llm.ChatResponse, error) {
	if len(req.Messages) == 0 {
		return nil, errors.New("echo: no messages")
	}
	last := req.Messages[len(req.Messages)-1]
	return &llm.ChatResponse{Message: llm.Message{Role: "assistant", Content: last.Content}, Model: "echo"}, nil
}

func (c *conn) ChatStream(ctx context.Context, req llm.ChatRequest) (llm.Stream, error) {
	if len(req.Messages) == 0 {
		return nil, errors.New("echo: no messages")
	}
	last := req.Messages[len(req.Messages)-1]
	return &stream{content: []rune(last.Content)}, nil
}

func (s *stream) Recv() (*llm.Chunk, error) {
	if s.i >= len(s.content) {
		return &llm.Chunk{Done: true}, nil
	}
	ch := &llm.Chunk{Content: string(s.content[s.i])}
	s.i++
	return ch, nil
}

func (s *stream) Close() error { return nil }

func (c *conn) Embed(ctx context.Context, req llm.EmbedRequest) (*llm.EmbedResponse, error) {
	if req.Text == "" {
		return nil, errors.New("echo: empty text")
	}
	vec := make([]float64, len(req.Text))
	for i, b := range []byte(req.Text) {
		vec[i] = float64(b)
	}
	return &llm.EmbedResponse{Vector: vec, Model: "echo"}, nil
}
