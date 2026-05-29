package subproc

import (
	"bytes"
	"strings"
	"testing"
)

func TestCodecWriteThenReadRequest(t *testing.T) {
	var buf bytes.Buffer
	c := NewCodec(&buf, &buf)
	req, _ := NewRequest(1, "echo", []string{"x"})
	if err := c.WriteRequest(req); err != nil {
		t.Fatalf("WriteRequest: %v", err)
	}
	// new codec to read so we don't reuse the bufio that already
	// drained 0 bytes.
	c2 := NewCodec(&buf, &bytes.Buffer{})
	got, err := c2.ReadRequest()
	if err != nil {
		t.Fatalf("ReadRequest: %v", err)
	}
	if got.Method != "echo" || got.ID != 1 {
		t.Errorf("round-trip lost data: %+v", got)
	}
}

func TestCodecWriteThenReadResponse(t *testing.T) {
	var buf bytes.Buffer
	c := NewCodec(&buf, &buf)
	resp, _ := NewResponse(7, map[string]int{"sum": 3})
	if err := c.WriteResponse(resp); err != nil {
		t.Fatalf("WriteResponse: %v", err)
	}
	c2 := NewCodec(&buf, &bytes.Buffer{})
	got, err := c2.ReadResponse()
	if err != nil {
		t.Fatalf("ReadResponse: %v", err)
	}
	if got.ID != 7 || string(got.Result) != `{"sum":3}` {
		t.Errorf("round-trip lost data: %+v", got)
	}
}

func TestCodecRejectsBadJSONRPC(t *testing.T) {
	var buf bytes.Buffer
	c := NewCodec(&buf, &buf)
	bad := Request{JSONRPC: "1.0", ID: 1, Method: "x"}
	if err := c.WriteRequest(bad); err == nil {
		t.Fatal("WriteRequest should reject jsonrpc != 2.0")
	}
	badResp := Response{JSONRPC: "1.0", ID: 1}
	if err := c.WriteResponse(badResp); err == nil {
		t.Fatal("WriteResponse should reject jsonrpc != 2.0")
	}
}

func TestCodecReadRequestRejectsMissingMethod(t *testing.T) {
	in := strings.NewReader(`{"jsonrpc":"2.0","id":1}` + "\n")
	c := NewCodec(in, &bytes.Buffer{})
	_, err := c.ReadRequest()
	if err == nil || !strings.Contains(err.Error(), "method") {
		t.Errorf("expected missing-method error, got %v", err)
	}
}

func TestCodecReadResponseRejectsBadVersion(t *testing.T) {
	in := strings.NewReader(`{"jsonrpc":"1.0","id":1,"result":null}` + "\n")
	c := NewCodec(in, &bytes.Buffer{})
	_, err := c.ReadResponse()
	if err == nil || !strings.Contains(err.Error(), "jsonrpc") {
		t.Errorf("expected version error, got %v", err)
	}
}

func TestCodecReadRequestRejectsMalformedJSON(t *testing.T) {
	in := strings.NewReader("{not json\n")
	c := NewCodec(in, &bytes.Buffer{})
	_, err := c.ReadRequest()
	if err == nil {
		t.Fatal("expected JSON decode error")
	}
}
