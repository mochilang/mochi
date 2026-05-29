package subproc

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"sync"
)

// Codec sits above the framer and below the Client / Server message
// loops. It owns one bufio.Reader and one writer mutex so concurrent
// callers can safely serialise into the same wire.
type Codec struct {
	br *bufio.Reader
	w  io.Writer

	writeMu sync.Mutex
}

// NewCodec wraps r and w with the line framer + JSON marshalling.
// Callers must not share r/w with another Codec; the bufio.Reader's
// internal buffer would be corrupted by an outside read.
func NewCodec(r io.Reader, w io.Writer) *Codec {
	return &Codec{br: bufio.NewReaderSize(r, 64*1024), w: w}
}

// WriteRequest marshals req and emits one frame.
func (c *Codec) WriteRequest(req Request) error {
	if req.JSONRPC != JSONRPCVersion {
		return fmt.Errorf("subproc: Codec.WriteRequest requires JSONRPC == %q", JSONRPCVersion)
	}
	raw, err := json.Marshal(req)
	if err != nil {
		return fmt.Errorf("subproc: marshal request: %w", err)
	}
	c.writeMu.Lock()
	defer c.writeMu.Unlock()
	return WriteFrame(c.w, raw)
}

// WriteResponse marshals resp and emits one frame.
func (c *Codec) WriteResponse(resp Response) error {
	if resp.JSONRPC != JSONRPCVersion {
		return fmt.Errorf("subproc: Codec.WriteResponse requires JSONRPC == %q", JSONRPCVersion)
	}
	raw, err := json.Marshal(resp)
	if err != nil {
		return fmt.Errorf("subproc: marshal response: %w", err)
	}
	c.writeMu.Lock()
	defer c.writeMu.Unlock()
	return WriteFrame(c.w, raw)
}

// ReadRequest reads one frame and decodes it as a Request.
func (c *Codec) ReadRequest() (Request, error) {
	raw, err := ReadFrame(c.br)
	if err != nil {
		return Request{}, err
	}
	var req Request
	if err := json.Unmarshal(raw, &req); err != nil {
		return Request{}, fmt.Errorf("subproc: decode request: %w", err)
	}
	if req.JSONRPC != JSONRPCVersion {
		return Request{}, fmt.Errorf("subproc: request has jsonrpc=%q, want %q", req.JSONRPC, JSONRPCVersion)
	}
	if req.Method == "" {
		return Request{}, fmt.Errorf("subproc: request missing method")
	}
	return req, nil
}

// ReadResponse reads one frame and decodes it as a Response.
func (c *Codec) ReadResponse() (Response, error) {
	raw, err := ReadFrame(c.br)
	if err != nil {
		return Response{}, err
	}
	var resp Response
	if err := json.Unmarshal(raw, &resp); err != nil {
		return Response{}, fmt.Errorf("subproc: decode response: %w", err)
	}
	if resp.JSONRPC != JSONRPCVersion {
		return Response{}, fmt.Errorf("subproc: response has jsonrpc=%q, want %q", resp.JSONRPC, JSONRPCVersion)
	}
	return resp, nil
}
