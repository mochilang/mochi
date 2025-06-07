package http

import (
	"bytes"
	"encoding/json"
	"io"
	nethttp "net/http"
)

// Fetch retrieves JSON from the given URL and unmarshals it into an
// arbitrary Go value. It is used by the interpreter to implement the
// `fetch` expression.
func Fetch(url string) (any, error) {
	return FetchWith(url, nil)
}

func FetchWith(url string, opts map[string]any) (any, error) {
	method := "GET"
	if opts != nil {
		if m, ok := opts["method"].(string); ok {
			method = m
		}
	}

	var body io.Reader
	if opts != nil {
		if b, ok := opts["body"]; ok {
			data, err := json.Marshal(b)
			if err != nil {
				return nil, err
			}
			body = bytes.NewReader(data)
		}
	}

	req, err := nethttp.NewRequest(method, url, body)
	if err != nil {
		return nil, err
	}
	if opts != nil {
		if hs, ok := opts["headers"]; ok {
			for k, v := range toAnyMap(hs) {
				if s, ok := v.(string); ok {
					req.Header.Set(k, s)
				}
			}
		}
	}

	resp, err := nethttp.DefaultClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	data, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}
	var out any
	if err := json.Unmarshal(data, &out); err != nil {
		return nil, err
	}
	return out, nil
}

func toAnyMap(m any) map[string]any {
	switch v := m.(type) {
	case map[string]any:
		return v
	case map[string]string:
		out := make(map[string]any, len(v))
		for k, vv := range v {
			out[k] = vv
		}
		return out
	default:
		return nil
	}
}
