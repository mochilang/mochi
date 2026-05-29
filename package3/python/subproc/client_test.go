package subproc

import (
	"encoding/json"
	"errors"
	"io"
	"strings"
	"sync"
	"testing"
)

// pipePair wires a Client to an in-process Server over a pair of
// io.Pipes so the full protocol can be exercised without spawning a
// real subprocess. The returned cleanup closes both pipes.
func pipePair(t *testing.T, h Handler) (*Client, func()) {
	t.Helper()
	clientReader, serverWriter := io.Pipe()
	serverReader, clientWriter := io.Pipe()
	clientCodec := NewCodec(clientReader, clientWriter)
	serverCodec := NewCodec(serverReader, serverWriter)
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		_ = ServeCodec(serverCodec, h)
	}()
	cleanup := func() {
		_ = clientWriter.Close()
		_ = serverWriter.Close()
		wg.Wait()
	}
	return NewClient(clientCodec, nil), cleanup
}

func TestClientCallEcho(t *testing.T) {
	client, done := pipePair(t, func(method string, params json.RawMessage) (any, error) {
		if method != "echo" {
			return nil, &RPCError{Code: CodeMethodNotFound, Message: "no"}
		}
		var args []string
		_ = json.Unmarshal(params, &args)
		return args, nil
	})
	defer done()
	got, err := client.Call("echo", []string{"hello"})
	if err != nil {
		t.Fatalf("Call: %v", err)
	}
	if string(got) != `["hello"]` {
		t.Errorf("Result = %s", got)
	}
}

func TestClientCallRPCError(t *testing.T) {
	client, done := pipePair(t, func(method string, params json.RawMessage) (any, error) {
		return nil, &RPCError{Code: CodeMethodNotFound, Message: "no such method"}
	})
	defer done()
	_, err := client.Call("nope", nil)
	if err == nil {
		t.Fatal("expected error")
	}
	var rerr *RPCError
	if !errors.As(err, &rerr) {
		t.Fatalf("expected *RPCError, got %T %v", err, err)
	}
	if rerr.Code != CodeMethodNotFound {
		t.Errorf("Code = %d", rerr.Code)
	}
}

func TestClientCallGenericError(t *testing.T) {
	client, done := pipePair(t, func(method string, params json.RawMessage) (any, error) {
		return nil, errors.New("boom")
	})
	defer done()
	_, err := client.Call("any", nil)
	if err == nil || !strings.Contains(err.Error(), "boom") {
		t.Fatalf("expected boom error, got %v", err)
	}
	var rerr *RPCError
	if !errors.As(err, &rerr) {
		t.Fatalf("generic error should map to *RPCError CodeInternalError, got %T %v", err, err)
	}
	if rerr.Code != CodeInternalError {
		t.Errorf("Code = %d, want CodeInternalError", rerr.Code)
	}
}

func TestClientSequentialIDs(t *testing.T) {
	got := []int{}
	var mu sync.Mutex
	client, done := pipePair(t, func(method string, params json.RawMessage) (any, error) {
		mu.Lock()
		got = append(got, len(got)+1)
		mu.Unlock()
		return nil, nil
	})
	defer done()
	for i := 0; i < 3; i++ {
		if _, err := client.Call("noop", nil); err != nil {
			t.Fatalf("Call %d: %v", i, err)
		}
	}
	if len(got) != 3 {
		t.Errorf("expected 3 dispatches, got %d", len(got))
	}
}

func TestClientAfterCloseRejects(t *testing.T) {
	client, done := pipePair(t, func(method string, params json.RawMessage) (any, error) {
		return nil, nil
	})
	defer done()
	if err := client.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	if _, err := client.Call("x", nil); err == nil {
		t.Fatal("Call after Close should error")
	}
}
