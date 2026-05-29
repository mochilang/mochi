package subproc

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
)

// Handler dispatches one method invocation. Implementations decode
// params into a typed value, run the work, and return the result (or
// an error) for serialisation back to the caller.
//
// When the returned error is a *RPCError, ServeCodec surfaces its
// Code + Message + Data verbatim. Other errors map to a generic
// CodeInternalError response.
type Handler func(method string, params json.RawMessage) (any, error)

// ServeCodec runs the message loop on c: read Request, dispatch via h,
// write Response. It returns when the codec hits io.EOF (clean
// peer-close) or any other framing/decoding error.
func ServeCodec(c *Codec, h Handler) error {
	if h == nil {
		return errors.New("subproc: ServeCodec requires a non-nil Handler")
	}
	for {
		req, err := c.ReadRequest()
		if errors.Is(err, io.EOF) {
			return nil
		}
		if err != nil {
			resp, _ := NewErrorResponse(0, CodeParseError, err.Error(), nil)
			_ = c.WriteResponse(resp)
			return err
		}
		resp := dispatch(req, h)
		if err := c.WriteResponse(resp); err != nil {
			return fmt.Errorf("subproc: write response: %w", err)
		}
	}
}

func dispatch(req Request, h Handler) Response {
	result, err := h(req.Method, req.Params)
	if err != nil {
		var rerr *RPCError
		if errors.As(err, &rerr) {
			return Response{JSONRPC: JSONRPCVersion, ID: req.ID, Error: rerr}
		}
		resp, _ := NewErrorResponse(req.ID, CodeInternalError, err.Error(), nil)
		return resp
	}
	resp, mErr := NewResponse(req.ID, result)
	if mErr != nil {
		fail, _ := NewErrorResponse(req.ID, CodeInternalError, mErr.Error(), nil)
		return fail
	}
	return resp
}
