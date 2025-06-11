package interpreter_test

import (
	"context"
	"reflect"
	"testing"

	"mochi/interpreter"
	"mochi/parser"
	"mochi/runtime/llm"
	"mochi/runtime/mod"
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

func (c *captureConn) Embed(ctx context.Context, req llm.EmbedRequest) (*llm.EmbedResponse, error) {
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
	modRoot, _ := mod.FindRoot(".")
	interp := interpreter.New(prog, env, modRoot)
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

func TestGenerateToolsSchema(t *testing.T) {
	src := `fun getWeather(location: string): string { return "" }
fun calc(expression: string): string { return "" }
let result = generate text { tools: [getWeather, calc], prompt: "hi" }`
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
	modRoot, _ := mod.FindRoot(".")
	interp := interpreter.New(prog, env, modRoot)
	if err := interp.Run(); err != nil {
		t.Fatalf("run error: %v", err)
	}
	if captured == nil {
		t.Fatal("missing request")
	}
	if len(captured.Tools) != 2 {
		t.Fatalf("expected 2 tools, got %d", len(captured.Tools))
	}
	if captured.Tools[0].Name != "getWeather" {
		t.Fatalf("unexpected tool %s", captured.Tools[0].Name)
	}
	params := captured.Tools[0].Parameters
	expected := map[string]any{
		"type":       "object",
		"properties": map[string]any{"location": map[string]any{"type": "string"}},
		"required":   []string{"location"},
	}
	if !reflect.DeepEqual(params, expected) {
		t.Fatalf("schema mismatch:\nwant %#v\ngot  %#v", expected, params)
	}
}
