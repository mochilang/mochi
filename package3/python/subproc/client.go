package subproc

import (
	"encoding/json"
	"fmt"
	"io"
	"sync"
)

// Client is the host-side handle for a JSON-RPC worker. It is
// goroutine-safe: concurrent Call invocations are serialised on the
// codec writer mutex; per-request synchronicity is enforced by waiting
// for the matching Response before returning.
//
// Pipelining (multiple in-flight requests demultiplexed by ID) is
// sub-phase 14.2. For now Call holds the read lock end-to-end so the
// next request blocks until the previous response arrives.
type Client struct {
	codec  *Codec
	closer io.Closer

	mu     sync.Mutex
	nextID int
	closed bool
}

// NewClient wraps a Codec. closer (typically the subprocess
// io.Closer) is invoked by Client.Close. It may be nil for tests that
// want the in-process pipe lifetime managed by the caller.
func NewClient(codec *Codec, closer io.Closer) *Client {
	return &Client{codec: codec, closer: closer}
}

// Call issues one method invocation and waits for the response. The
// returned bytes are the raw Result; the caller is responsible for
// unmarshalling.
//
// On RPC-level errors (Response.Error non-nil), Call returns the
// RPCError directly so callers can switch on its Code field.
func (c *Client) Call(method string, params any) (json.RawMessage, error) {
	c.mu.Lock()
	if c.closed {
		c.mu.Unlock()
		return nil, fmt.Errorf("subproc: Client is closed")
	}
	c.nextID++
	id := c.nextID
	c.mu.Unlock()

	req, err := NewRequest(id, method, params)
	if err != nil {
		return nil, fmt.Errorf("subproc: build request: %w", err)
	}
	if err := c.codec.WriteRequest(req); err != nil {
		return nil, fmt.Errorf("subproc: write request: %w", err)
	}
	resp, err := c.codec.ReadResponse()
	if err != nil {
		return nil, fmt.Errorf("subproc: read response: %w", err)
	}
	if resp.ID != id {
		return nil, fmt.Errorf("subproc: response id=%d does not match request id=%d", resp.ID, id)
	}
	if resp.Error != nil {
		return nil, resp.Error
	}
	return resp.Result, nil
}

// Close closes the underlying closer (if any). Further Call attempts
// after Close return a closed-client error.
func (c *Client) Close() error {
	c.mu.Lock()
	c.closed = true
	c.mu.Unlock()
	if c.closer == nil {
		return nil
	}
	return c.closer.Close()
}
