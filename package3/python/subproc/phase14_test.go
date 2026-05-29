package subproc

import (
	"encoding/json"
	"errors"
	"io"
	"strings"
	"sync"
	"testing"
)

// TestPhase14SubprocessRuntime is the Phase 14 umbrella sentinel. It
// wires a Client to an in-process Server over io.Pipes (the same
// transport shape the real subprocess will use) and drives the full
// JSON-RPC 2.0 protocol end-to-end:
//
//  1. A typed handler simulating an imported Python surface
//     (add(int, int) -> int + fail() -> RPCError).
//  2. A Client.Call(add, [3, 4]) returns 7.
//  3. A second Client.Call(fail) surfaces a CodeInvalidParams
//     *RPCError to the caller with the handler's error data.
//  4. A third Client.Call against an unknown method returns
//     CodeMethodNotFound.
//  5. Closing the Client + waiting for the server goroutine to drain
//     terminates cleanly without leaking goroutines.
//  6. The worker source the Phase 14 emitter ships embeds the same
//     error codes the host expects (codes are the wire contract).
func TestPhase14SubprocessRuntime(t *testing.T) {
	clientReader, serverWriter := io.Pipe()
	serverReader, clientWriter := io.Pipe()
	clientCodec := NewCodec(clientReader, clientWriter)
	serverCodec := NewCodec(serverReader, serverWriter)

	handler := func(method string, params json.RawMessage) (any, error) {
		switch method {
		case "add":
			var args []int
			if err := json.Unmarshal(params, &args); err != nil {
				return nil, &RPCError{Code: CodeInvalidParams, Message: err.Error()}
			}
			if len(args) != 2 {
				return nil, &RPCError{Code: CodeInvalidParams, Message: "want 2 args"}
			}
			return args[0] + args[1], nil
		case "fail":
			data, _ := json.Marshal(map[string]string{"why": "spec test"})
			return nil, &RPCError{Code: CodeInvalidParams, Message: "intentional", Data: data}
		}
		return nil, &RPCError{Code: CodeMethodNotFound, Message: "no such method: " + method}
	}

	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		_ = ServeCodec(serverCodec, handler)
	}()

	client := NewClient(clientCodec, nil)

	// Step 2: successful call.
	res, err := client.Call("add", []int{3, 4})
	if err != nil {
		t.Fatalf("add: %v", err)
	}
	if string(res) != "7" {
		t.Errorf("add result = %s", res)
	}

	// Step 3: RPCError surfaces with the right code + data.
	_, err = client.Call("fail", nil)
	if err == nil {
		t.Fatal("fail should error")
	}
	var rerr *RPCError
	if !errors.As(err, &rerr) {
		t.Fatalf("expected *RPCError, got %T %v", err, err)
	}
	if rerr.Code != CodeInvalidParams || rerr.Message != "intentional" {
		t.Errorf("RPCError wrong: %+v", rerr)
	}
	var dataObj map[string]string
	if err := json.Unmarshal(rerr.Data, &dataObj); err != nil || dataObj["why"] != "spec test" {
		t.Errorf("RPCError data lost: %s err=%v", rerr.Data, err)
	}

	// Step 4: unknown method.
	_, err = client.Call("unknown", nil)
	if err == nil {
		t.Fatal("unknown should error")
	}
	if !errors.As(err, &rerr) || rerr.Code != CodeMethodNotFound {
		t.Errorf("expected CodeMethodNotFound, got %v", err)
	}

	// Step 5: clean shutdown.
	_ = clientWriter.Close()
	_ = serverWriter.Close()
	_ = client.Close()
	wg.Wait()

	// Step 6: worker source ships the same error codes the host
	// reads. Confirm the contract is wire-compatible.
	src, err := RenderWorker(WorkerOptions{
		ModuleImport: "from mochi_pkg import add, fail",
		Methods:      []string{"add", "fail"},
	})
	if err != nil {
		t.Fatalf("RenderWorker: %v", err)
	}
	for _, want := range []string{
		"_PARSE_ERROR = -32700",
		"_METHOD_NOT_FOUND = -32601",
		"_INTERNAL_ERROR = -32603",
		`"jsonrpc": "2.0"`,
		`"add": add,`,
		`"fail": fail,`,
	} {
		if !strings.Contains(src, want) {
			t.Errorf("worker missing %q\n%s", want, src)
		}
	}
}
