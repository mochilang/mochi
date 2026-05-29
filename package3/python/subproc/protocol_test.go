package subproc

import (
	"encoding/json"
	"strings"
	"testing"
)

func TestNewRequest(t *testing.T) {
	req, err := NewRequest(7, "echo", []any{"hi"})
	if err != nil {
		t.Fatalf("NewRequest: %v", err)
	}
	if req.JSONRPC != "2.0" {
		t.Errorf("JSONRPC = %q", req.JSONRPC)
	}
	if req.ID != 7 {
		t.Errorf("ID = %d", req.ID)
	}
	if string(req.Params) != `["hi"]` {
		t.Errorf("Params = %s", req.Params)
	}
}

func TestNewRequestNilParams(t *testing.T) {
	req, err := NewRequest(1, "ping", nil)
	if err != nil {
		t.Fatalf("NewRequest: %v", err)
	}
	if len(req.Params) != 0 {
		t.Errorf("Params should be empty: %s", req.Params)
	}
}

func TestNewResponse(t *testing.T) {
	resp, err := NewResponse(3, map[string]int{"x": 1})
	if err != nil {
		t.Fatalf("NewResponse: %v", err)
	}
	if resp.JSONRPC != "2.0" {
		t.Errorf("JSONRPC = %q", resp.JSONRPC)
	}
	if string(resp.Result) != `{"x":1}` {
		t.Errorf("Result = %s", resp.Result)
	}
	if resp.Error != nil {
		t.Errorf("Error should be nil: %+v", resp.Error)
	}
}

func TestNewErrorResponse(t *testing.T) {
	resp, err := NewErrorResponse(3, CodeMethodNotFound, "no such method", map[string]string{"hint": "spelled wrong"})
	if err != nil {
		t.Fatalf("NewErrorResponse: %v", err)
	}
	if resp.Error == nil {
		t.Fatal("Error must be populated")
	}
	if resp.Error.Code != CodeMethodNotFound || resp.Error.Message != "no such method" {
		t.Errorf("Error wrong: %+v", resp.Error)
	}
	if string(resp.Error.Data) != `{"hint":"spelled wrong"}` {
		t.Errorf("Error.Data = %s", resp.Error.Data)
	}
}

func TestRPCErrorErrorString(t *testing.T) {
	var nilErr *RPCError
	if nilErr.Error() != "" {
		t.Errorf("nil RPCError.Error should be empty, got %q", nilErr.Error())
	}
	e := &RPCError{Code: CodeInternalError, Message: "boom"}
	if e.Error() != "boom" {
		t.Errorf("Error string = %q", e.Error())
	}
}

func TestRequestRoundTripJSON(t *testing.T) {
	req, _ := NewRequest(11, "add", []int{1, 2})
	raw, err := json.Marshal(req)
	if err != nil {
		t.Fatalf("Marshal: %v", err)
	}
	if !strings.Contains(string(raw), `"jsonrpc":"2.0"`) {
		t.Errorf("missing jsonrpc field: %s", raw)
	}
	var back Request
	if err := json.Unmarshal(raw, &back); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if back.ID != 11 || back.Method != "add" {
		t.Errorf("round-trip lost data: %+v", back)
	}
}

func TestStandardErrorCodes(t *testing.T) {
	cases := map[int]int{
		CodeParseError:     -32700,
		CodeInvalidRequest: -32600,
		CodeMethodNotFound: -32601,
		CodeInvalidParams:  -32602,
		CodeInternalError:  -32603,
	}
	for got, want := range cases {
		if got != want {
			t.Errorf("error code mismatch: got %d, want %d", got, want)
		}
	}
}
