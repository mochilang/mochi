package http

import (
	"encoding/json"
	"io"
	nethttp "net/http"
)

// Fetch retrieves JSON from the given URL and unmarshals it into an
// arbitrary Go value. It is used by the interpreter to implement the
// `fetch` expression.
func Fetch(url string) (any, error) {
	resp, err := nethttp.Get(url)
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
