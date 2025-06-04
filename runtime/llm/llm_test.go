package llm

import (
	"context"
	"errors"
	"testing"
)

// echoProvider is a simple provider used for tests. It echoes the last user message.
type echoProvider struct{}

type echoConn struct{ opts Options }

type echoStream struct {
	content []rune
	i       int
}

func (e echoProvider) Open(opts Options) (Conn, error) {
	return &echoConn{opts: opts}, nil
}

func (c *echoConn) Close() error { return nil }

func (c *echoConn) Chat(ctx context.Context, req ChatRequest) (*ChatResponse, error) {
	if len(req.Messages) == 0 {
		return nil, errors.New("no messages")
	}
	last := req.Messages[len(req.Messages)-1]
	return &ChatResponse{Message: Message{Role: "assistant", Content: last.Content}}, nil
}

func (c *echoConn) ChatStream(ctx context.Context, req ChatRequest) (Stream, error) {
	if len(req.Messages) == 0 {
		return nil, errors.New("no messages")
	}
	last := req.Messages[len(req.Messages)-1]
	return &echoStream{content: []rune(last.Content)}, nil
}

func (s *echoStream) Recv() (*Chunk, error) {
	if s.i >= len(s.content) {
		return &Chunk{Done: true}, nil
	}
	ch := &Chunk{Content: string(s.content[s.i])}
	s.i++
	return ch, nil
}

func (s *echoStream) Close() error { return nil }

func TestRegisterOpenChat(t *testing.T) {
	mu.Lock()
	providers = make(map[string]Provider)
	mu.Unlock()
	Register("echo", echoProvider{})

	c, err := Open("echo", Options{})
	if err != nil {
		t.Fatalf("open: %v", err)
	}
	defer c.Close()

	resp, err := c.Chat(context.Background(), []Message{{Role: "user", Content: "hi"}})
	if err != nil {
		t.Fatalf("chat: %v", err)
	}
	if resp.Message.Content != "hi" {
		t.Fatalf("unexpected response: %q", resp.Message.Content)
	}
}

func TestChatStream(t *testing.T) {
	mu.Lock()
	providers = make(map[string]Provider)
	mu.Unlock()
	Register("echo", echoProvider{})

	c, err := Open("echo", Options{})
	if err != nil {
		t.Fatalf("open: %v", err)
	}
	defer c.Close()

	stream, err := c.ChatStream(context.Background(), []Message{{Role: "user", Content: "yo"}})
	if err != nil {
		t.Fatalf("stream: %v", err)
	}

	var out string
	for {
		chunk, err := stream.Recv()
		if err != nil {
			t.Fatalf("recv: %v", err)
		}
		if chunk.Done {
			break
		}
		out += chunk.Content
	}

	if out != "yo" {
		t.Fatalf("unexpected stream output %q", out)
	}
}
