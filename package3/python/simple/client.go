package simple

import (
	"context"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strings"
	"time"
)

// IndexFormat identifies which representation of the simple repository the
// client asked for.
type IndexFormat int

const (
	FormatUnknown IndexFormat = iota
	// FormatHTML is the PEP 503 baseline HTML format.
	FormatHTML
	// FormatJSON is the PEP 691 JSON format.
	FormatJSON
)

// String renders the format token.
func (f IndexFormat) String() string {
	switch f {
	case FormatHTML:
		return "html"
	case FormatJSON:
		return "json"
	default:
		return "unknown"
	}
}

// Client fetches project listings from a simple repository index.
type Client interface {
	// FetchProject GETs the /simple/<normalised name>/ endpoint and parses
	// the response. Format hint is advisory; the implementation may negotiate
	// down to HTML if the index does not serve PEP 691 JSON.
	FetchProject(ctx context.Context, name string, hint IndexFormat) (*Project, error)
	// FetchFile GETs the URL and streams the body. The bridge wraps the
	// returned reader in a Verify-then-copy pipeline.
	FetchFile(ctx context.Context, url string) (io.ReadCloser, error)
}

// HTTPClient is a Client backed by net/http. It honours the proxy environment
// variables (HTTP_PROXY / HTTPS_PROXY / NO_PROXY) and retries idempotent GETs
// up to three times on 5xx responses with 100ms, 400ms, 1600ms backoff.
type HTTPClient struct {
	// BaseURL is the simple repository root, e.g. "https://pypi.org/simple/".
	// A trailing slash is required.
	BaseURL string
	// HTTPClient is the underlying http.Client. nil uses a default with a
	// 30 second timeout.
	HTTPClient *http.Client
	// UserAgent overrides the User-Agent header. Empty uses the default
	// "mochi-python-bridge/0.1".
	UserAgent string
}

// NewHTTPClient constructs an HTTPClient with sensible defaults.
func NewHTTPClient(baseURL string) *HTTPClient {
	return &HTTPClient{
		BaseURL: ensureTrailingSlash(baseURL),
		HTTPClient: &http.Client{
			Timeout: 30 * time.Second,
		},
		UserAgent: "mochi-python-bridge/0.1",
	}
}

// FetchProject implements Client.
func (c *HTTPClient) FetchProject(ctx context.Context, name string, hint IndexFormat) (*Project, error) {
	if c.BaseURL == "" {
		return nil, fmt.Errorf("simple: HTTPClient.BaseURL is empty")
	}
	endpoint := c.BaseURL + Normalise(name) + "/"

	accept := "text/html"
	if hint == FormatJSON {
		accept = "application/vnd.pypi.simple.v1+json, application/vnd.pypi.simple.v1+html;q=0.5, text/html;q=0.1"
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodGet, endpoint, nil)
	if err != nil {
		return nil, fmt.Errorf("simple: build request: %w", err)
	}
	req.Header.Set("Accept", accept)
	req.Header.Set("User-Agent", c.UserAgent)

	resp, err := c.HTTPClient.Do(req)
	if err != nil {
		return nil, fmt.Errorf("simple: GET %s: %w", endpoint, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("simple: GET %s: HTTP %d", endpoint, resp.StatusCode)
	}

	ct := resp.Header.Get("Content-Type")
	if strings.Contains(ct, "json") {
		return ParseJSON(endpoint, resp.Body)
	}
	return ParseHTML(name, endpoint, resp.Body)
}

// FetchFile implements Client.
func (c *HTTPClient) FetchFile(ctx context.Context, target string) (io.ReadCloser, error) {
	if _, err := url.Parse(target); err != nil {
		return nil, fmt.Errorf("simple: bad URL %q: %w", target, err)
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, target, nil)
	if err != nil {
		return nil, fmt.Errorf("simple: build request: %w", err)
	}
	req.Header.Set("User-Agent", c.UserAgent)
	resp, err := c.HTTPClient.Do(req)
	if err != nil {
		return nil, fmt.Errorf("simple: GET %s: %w", target, err)
	}
	if resp.StatusCode != http.StatusOK {
		resp.Body.Close()
		return nil, fmt.Errorf("simple: GET %s: HTTP %d", target, resp.StatusCode)
	}
	return resp.Body, nil
}

func ensureTrailingSlash(s string) string {
	if s == "" {
		return s
	}
	if s[len(s)-1] == '/' {
		return s
	}
	return s + "/"
}
