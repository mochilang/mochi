package leetcode

import (
	"context"
	"strings"
	"testing"

	"mochi/runtime/llm"
	_ "mochi/runtime/llm/provider/cohere"
)

type solveProvider struct{}

type solveConn struct{ calls int }

func init() { llm.Register("solvefake", solveProvider{}) }

func (solveProvider) Open(dsn string, opts llm.Options) (llm.Conn, error) { return &solveConn{}, nil }
func (solveConn) Close() error                                            { return nil }
func (c *solveConn) Chat(ctx context.Context, req llm.ChatRequest) (*llm.ChatResponse, error) {
	c.calls++
	if c.calls == 1 {
		// first attempt returns faulty code
		return &llm.ChatResponse{Message: llm.Message{Role: "assistant", Content: "```mochi\nfun add(a:int,b:int):int { return a - b }\n\n test \"t\" { expect add(2,3) == 5 }\n```"}, Model: "solvefake"}, nil
	}
	// second and subsequent attempts return correct code
	return &llm.ChatResponse{Message: llm.Message{Role: "assistant", Content: "```mochi\nfun add(a:int,b:int):int { return a + b }\n\n test \"t\" { expect add(2,3) == 5 }\n```"}, Model: "solvefake"}, nil
}
func (solveConn) ChatStream(ctx context.Context, req llm.ChatRequest) (llm.Stream, error) {
	return nil, nil
}
func (solveConn) Embed(ctx context.Context, req llm.EmbedRequest) (*llm.EmbedResponse, error) {
	return nil, nil
}

func TestSolve(t *testing.T) {
	// llm.Default, _ = llm.Open("solvefake", "", llm.Options{})
	code, err := Solve(3, 1)
	if err != nil {
		t.Fatalf("Solve error: %v", err)
	}
	if code == "" || !strContains(code, "a + b") {
		t.Fatalf("unexpected code: %q", code)
	}
}

func strContains(s, substr string) bool { return strings.Contains(s, substr) }
