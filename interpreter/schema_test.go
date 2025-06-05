package interpreter_test

import (
	"context"
	"reflect"
	"testing"

	"mochi/interpreter"
	"mochi/parser"
	"mochi/runtime/llm"
	"mochi/types"
)

// capture provider records the last chat request.
var captured *llm.ChatRequest

type captureProvider struct{}

type captureConn struct{}

func init() { llm.Register("capture", captureProvider{}) }

func (captureProvider) Open(dsn string, opts llm.Options) (llm.Conn, error) {
	return &captureConn{}, nil
}

func (c *captureConn) Close() error { return nil }

func (c *captureConn) Chat(ctx context.Context, req llm.ChatRequest) (*llm.ChatResponse, error) {
	r := req // copy
	captured = &r
	return &llm.ChatResponse{Message: llm.Message{Role: "assistant", Content: "{}"}, Model: "capture"}, nil
}

func (c *captureConn) ChatStream(ctx context.Context, req llm.ChatRequest) (llm.Stream, error) {
	return nil, nil
}

func TestGenerateStructSchema(t *testing.T) {
	src := `type Foo { bar: int }
let f = generate Foo { prompt: "{}" }`
	prog, err := parser.ParseString(src)
	if err != nil {
		t.Fatal(err)
	}
	env := types.NewEnv(nil)
	llm.Default, err = llm.Open("capture", "", llm.Options{})
	if err != nil {
		t.Fatal(err)
	}
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	interp := interpreter.New(prog, env)
	if err := interp.Run(); err != nil {
		t.Fatalf("run error: %v", err)
	}
	if captured == nil || captured.ResponseFormat == nil {
		t.Fatal("missing response format")
	}
	expected := map[string]any{
		"type": "object",
		"properties": map[string]any{
			"bar": map[string]any{"type": "integer"},
		},
		"required": []string{"bar"},
	}
	if captured.ResponseFormat.Type != "json_object" {
		t.Fatalf("unexpected type %s", captured.ResponseFormat.Type)
	}
	if !reflect.DeepEqual(captured.ResponseFormat.Schema, expected) {
		t.Fatalf("schema mismatch:\nwant %#v\ngot  %#v", expected, captured.ResponseFormat.Schema)
	}
}
