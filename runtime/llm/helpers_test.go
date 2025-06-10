package llm

import (
	"encoding/json"
	"os"
	"testing"
	"time"
)

type stubStream struct{ closed bool }

func (s *stubStream) Recv() (*Chunk, error) {
	return &Chunk{Done: true}, nil
}

func (s *stubStream) Close() error {
	s.closed = true
	return nil
}

func TestLastUser(t *testing.T) {
	cases := []struct {
		msgs []Message
		want string
	}{
		{nil, ""},
		{[]Message{{Role: "assistant", Content: "a"}}, "a"},
		{[]Message{{Role: "system", Content: "s"}, {Role: "user", Content: "u"}, {Role: "assistant", Content: "a"}}, "u"},
	}
	for i, c := range cases {
		got := lastUser(c.msgs)
		if got != c.want {
			t.Fatalf("case %d: expected %q got %q", i, c.want, got)
		}
	}
}

func TestMustJSON(t *testing.T) {
	in := map[string]int{"x": 1}
	got := mustJSON(in)
	exp, _ := json.Marshal(in)
	if string(got) != string(exp) {
		t.Fatalf("expected %s got %s", exp, got)
	}
}

func TestGetAgentSessionID(t *testing.T) {
	os.Setenv("MOCHI_AGENT", "tester")
	defer os.Unsetenv("MOCHI_AGENT")
	if a := getAgent(); a != "tester" {
		t.Fatalf("getAgent env: %q", a)
	}
	os.Unsetenv("MOCHI_AGENT")
	if a := getAgent(); a != "llm" {
		t.Fatalf("getAgent default: %q", a)
	}

	os.Setenv("MOCHI_SESSION", "abc")
	defer os.Unsetenv("MOCHI_SESSION")
	if s := getSessionID(); s != "abc" {
		t.Fatalf("getSessionID env: %q", s)
	}
	os.Unsetenv("MOCHI_SESSION")
	if s := getSessionID(); s == "" {
		t.Fatalf("getSessionID default empty")
	}
}

func TestOptions(t *testing.T) {
	var r ChatRequest
	WithModel("m1")(&r)
	if r.Model != "m1" {
		t.Fatalf("model not set")
	}
	WithParam("t", 0.5)(&r)
	if r.Params["t"] != 0.5 {
		t.Fatalf("param not set")
	}
	rf := ResponseFormat{Type: "json"}
	WithResponseFormat(rf)(&r)
	if r.ResponseFormat == nil || r.ResponseFormat.Type != "json" {
		t.Fatalf("response format not set")
	}
	tools := []Tool{{Name: "a"}}
	WithTools(tools)(&r)
	if len(r.Tools) != 1 || r.Tools[0].Name != "a" {
		t.Fatalf("tools not set")
	}
	WithToolChoice("x")(&r)
	if r.ToolChoice != "x" {
		t.Fatalf("tool choice not set")
	}
}

func TestEmbedOptions(t *testing.T) {
	var r EmbedRequest
	WithEmbedModel("em")(&r)
	if r.Model != "em" {
		t.Fatalf("embed model not set")
	}
	WithEmbedParam("p", 2)(&r)
	if r.Params["p"] != 2 {
		t.Fatalf("embed param not set")
	}
	WithEmbedNormalize(true)(&r)
	if !r.Normalize {
		t.Fatalf("normalize not set")
	}
}

func TestLogStreamClose(t *testing.T) {
	ss := &stubStream{}
	ls := &logStream{
		Stream:    ss,
		req:       ChatRequest{Messages: []Message{{Role: "user", Content: "hi"}}},
		start:     time.Now(),
		sessionID: "s",
		agent:     "a",
		model:     "m",
	}
	// Recv once to mark done
	ls.Recv()
	if err := ls.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}
	if !ss.closed {
		t.Fatalf("underlying stream not closed")
	}
	if !ls.logged {
		t.Fatalf("log not recorded")
	}
}
