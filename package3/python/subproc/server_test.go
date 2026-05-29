package subproc

import (
	"bytes"
	"encoding/json"
	"strings"
	"testing"
)

func TestServeCodecRejectsNilHandler(t *testing.T) {
	c := NewCodec(strings.NewReader(""), &bytes.Buffer{})
	if err := ServeCodec(c, nil); err == nil {
		t.Fatal("ServeCodec must reject nil handler")
	}
}

func TestServeCodecParseError(t *testing.T) {
	in := strings.NewReader("{not json\n")
	out := &bytes.Buffer{}
	c := NewCodec(in, out)
	if err := ServeCodec(c, func(string, json.RawMessage) (any, error) {
		t.Fatal("handler should not run on parse error")
		return nil, nil
	}); err == nil {
		t.Fatal("expected error")
	}
	if !strings.Contains(out.String(), `"code":-32700`) {
		t.Errorf("response missing parse error code: %s", out.String())
	}
}

func TestServeCodecInternalError(t *testing.T) {
	req, _ := NewRequest(1, "any", nil)
	raw, _ := json.Marshal(req)
	in := bytes.NewReader(append(raw, '\n'))
	out := &bytes.Buffer{}
	c := NewCodec(in, out)
	_ = ServeCodec(c, func(string, json.RawMessage) (any, error) {
		return nil, &RPCError{Code: CodeInvalidParams, Message: "bad"}
	})
	if !strings.Contains(out.String(), `"code":-32602`) {
		t.Errorf("response missing invalid params code: %s", out.String())
	}
}

func TestServeCodecSuccessRoundTrip(t *testing.T) {
	req, _ := NewRequest(42, "add", []int{1, 2})
	raw, _ := json.Marshal(req)
	in := bytes.NewReader(append(raw, '\n'))
	out := &bytes.Buffer{}
	c := NewCodec(in, out)
	_ = ServeCodec(c, func(method string, params json.RawMessage) (any, error) {
		var args []int
		_ = json.Unmarshal(params, &args)
		sum := 0
		for _, a := range args {
			sum += a
		}
		return sum, nil
	})
	if !strings.Contains(out.String(), `"result":3`) {
		t.Errorf("response missing result: %s", out.String())
	}
	if !strings.Contains(out.String(), `"id":42`) {
		t.Errorf("response missing id: %s", out.String())
	}
}
