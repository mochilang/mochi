package subproc

import "encoding/json"

// JSONRPCVersion is the protocol version string every Request and
// Response carries. The subproc package only speaks 2.0; 1.0 (with
// its `id`/`method`/`params` triple but no `jsonrpc` field) is not
// accepted.
const JSONRPCVersion = "2.0"

// Request is one JSON-RPC 2.0 method call. ID is the caller-assigned
// sequence number the matching Response will echo. Params is the raw
// JSON payload the method handler will decode (typed payloads belong
// at the application layer, not the wire layer).
type Request struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      int             `json:"id"`
	Method  string          `json:"method"`
	Params  json.RawMessage `json:"params,omitempty"`
}

// Response is the reply to a Request. Exactly one of Result or Error
// is populated; the other is omitted via the omitempty tag. The ID
// echoes the matching Request.ID so the Client can pair concurrent
// requests when sub-phase 14.2 lands pipelining.
type Response struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      int             `json:"id"`
	Result  json.RawMessage `json:"result,omitempty"`
	Error   *RPCError       `json:"error,omitempty"`
}

// RPCError is the structured error returned in Response.Error.
// Code follows the JSON-RPC 2.0 reserved-range conventions; Data is
// optional structured payload the handler attached for the caller to
// decode at the application layer.
type RPCError struct {
	Code    int             `json:"code"`
	Message string          `json:"message"`
	Data    json.RawMessage `json:"data,omitempty"`
}

func (e *RPCError) Error() string {
	if e == nil {
		return ""
	}
	return e.Message
}

// Standard JSON-RPC 2.0 error codes (https://www.jsonrpc.org/specification#error_object).
const (
	CodeParseError     = -32700
	CodeInvalidRequest = -32600
	CodeMethodNotFound = -32601
	CodeInvalidParams  = -32602
	CodeInternalError  = -32603
)

// NewRequest builds a Request and marshals params into its Params
// field. NewRequest panics only on a programming error inside
// encoding/json (typed payloads that can never be marshalled).
func NewRequest(id int, method string, params any) (Request, error) {
	req := Request{JSONRPC: JSONRPCVersion, ID: id, Method: method}
	if params != nil {
		raw, err := json.Marshal(params)
		if err != nil {
			return Request{}, err
		}
		req.Params = raw
	}
	return req, nil
}

// NewResponse builds a successful Response.
func NewResponse(id int, result any) (Response, error) {
	resp := Response{JSONRPC: JSONRPCVersion, ID: id}
	if result != nil {
		raw, err := json.Marshal(result)
		if err != nil {
			return Response{}, err
		}
		resp.Result = raw
	}
	return resp, nil
}

// NewErrorResponse builds a failure Response with the given code +
// message + optional data payload.
func NewErrorResponse(id int, code int, message string, data any) (Response, error) {
	rerr := &RPCError{Code: code, Message: message}
	if data != nil {
		raw, err := json.Marshal(data)
		if err != nil {
			return Response{}, err
		}
		rerr.Data = raw
	}
	return Response{JSONRPC: JSONRPCVersion, ID: id, Error: rerr}, nil
}
