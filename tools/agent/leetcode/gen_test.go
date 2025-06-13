package leetcode

import (
	"context"
	"strings"
	"testing"

	"mochi/runtime/llm"
)

var (
	lastPrompt string
)

type fakeProvider struct{}
type fakeConn struct{}

func init() { llm.Register("genfake", fakeProvider{}) }

func (fakeProvider) Open(dsn string, opts llm.Options) (llm.Conn, error) { return &fakeConn{}, nil }
func (fakeConn) Close() error                                            { return nil }
func (fakeConn) Chat(ctx context.Context, req llm.ChatRequest) (*llm.ChatResponse, error) {
	if len(req.Messages) > 0 {
		lastPrompt = req.Messages[len(req.Messages)-1].Content
	}
	return &llm.ChatResponse{Message: llm.Message{Role: "assistant", Content: "```mochi\nprint(42)\n```"}, Model: "genfake"}, nil
}
func (fakeConn) ChatStream(ctx context.Context, req llm.ChatRequest) (llm.Stream, error) {
	return nil, nil
}
func (fakeConn) Embed(ctx context.Context, req llm.EmbedRequest) (*llm.EmbedResponse, error) {
	return nil, nil
}

// TestGenMochi ensures GenMochi builds the correct prompt and extracts code.
func TestGenMochi(t *testing.T) {
	llm.Default, _ = llm.Open("genfake", "", llm.Options{})
	text, err := Download(1)
	if err != nil {
		t.Fatalf("download failed: %v", err)
	}
	code, err := GenMochi(1, text)
	if err != nil {
		t.Fatalf("GenMochi error: %v", err)
	}
	if !strings.Contains(lastPrompt, text) {
		t.Fatalf("prompt missing problem text")
	}
	if code != "print(42)" {
		t.Fatalf("unexpected code: %q", code)
	}
}
