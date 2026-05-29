package sparse

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"net/http"
	"strings"
)

// getWithUA issues an HTTP GET via client.HTTP (or http.DefaultClient
// when nil), attaching the configured User-Agent. Callers own the
// returned body.
func getWithUA(ctx context.Context, c *Client, target string) (*http.Response, error) {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, target, nil)
	if err != nil {
		return nil, fmt.Errorf("sparse: build request: %w", err)
	}
	if c.UserAgent != "" {
		req.Header.Set("User-Agent", c.UserAgent)
	}
	httpClient := c.HTTP
	if httpClient == nil {
		httpClient = http.DefaultClient
	}
	resp, err := httpClient.Do(req)
	if err != nil {
		return nil, fmt.Errorf("sparse: GET %s: %w", target, err)
	}
	if resp.StatusCode == http.StatusNotFound {
		body, _ := io.ReadAll(io.LimitReader(resp.Body, 256))
		resp.Body.Close()
		return nil, fmt.Errorf("%w: %s: %s", ErrCrateNotFound, target, strings.TrimSpace(string(body)))
	}
	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(io.LimitReader(resp.Body, 1024))
		resp.Body.Close()
		return nil, fmt.Errorf("sparse: GET %s: status %d: %s", target, resp.StatusCode, strings.TrimSpace(string(body)))
	}
	return resp, nil
}

// bytesReader is a thin wrapper kept here so callers don't have to
// import bytes.NewReader directly. Keeps the public surface focused.
func bytesReader(b []byte) io.Reader { return bytes.NewReader(b) }
