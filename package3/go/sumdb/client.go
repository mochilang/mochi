package sumdb

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strings"
	"time"
)

// DefaultBaseURL is the public Go transparency-log endpoint.
const DefaultBaseURL = "https://sum.golang.org/"

// DefaultUserAgent identifies the bridge to sum.golang.org for
// usage analytics.
const DefaultUserAgent = "mochi-go-bridge/0.1 (+https://mochi-lang.dev)"

// Client is a thin HTTP wrapper for the sum.golang.org /latest,
// /lookup, and /tile endpoints. The empty Client is not usable;
// construct one with NewClient.
type Client struct {
	// BaseURL is the transparency log root, e.g.
	// "https://sum.golang.org/". Must end in a slash.
	BaseURL string

	// HTTP is the underlying http.Client. nil means use a 30-second
	// timeout client.
	HTTP *http.Client

	// UserAgent is the User-Agent header value.
	UserAgent string
}

// NewClient returns a Client pre-configured against sum.golang.org
// with sensible defaults.
func NewClient(baseURL string) *Client {
	if baseURL == "" {
		baseURL = DefaultBaseURL
	}
	if !strings.HasSuffix(baseURL, "/") {
		baseURL += "/"
	}
	return &Client{
		BaseURL:   baseURL,
		HTTP:      &http.Client{Timeout: 30 * time.Second},
		UserAgent: DefaultUserAgent,
	}
}

// Latest fetches /latest and returns the signed-note bytes. Callers
// typically verify with VerifierKey + ParseTreeHead.
func (c *Client) Latest(ctx context.Context) ([]byte, error) {
	return c.get(ctx, "latest")
}

// Lookup fetches /lookup/<module>@<version> and parses the response.
// The signed tree note inside the response is *not* verified here;
// callers are expected to invoke ParseNote + Verify against the
// sum.golang.org verifier key.
func (c *Client) Lookup(ctx context.Context, modulePath, version string) (*LookupRecord, error) {
	if modulePath == "" || version == "" {
		return nil, errors.New("sumdb: empty module or version")
	}
	// Lookups use the *un-escaped* module path joined with @<version>.
	// sum.golang.org expects URL-escaped path components but leaves
	// the literal '@' between module and version.
	body, err := c.get(ctx, "lookup/"+url.PathEscape(modulePath+"@"+version))
	if err != nil {
		return nil, err
	}
	return ParseLookup(body)
}

// Tile fetches /tile/<H>/<L>/<K> and returns the raw tile bytes.
// H is the tile height (sum.golang.org uses 8), L the tree level, K
// the tile index along that level.
func (c *Client) Tile(ctx context.Context, height, level int, index int64) ([]byte, error) {
	if height <= 0 || level < 0 || index < 0 {
		return nil, fmt.Errorf("sumdb: invalid tile coords H=%d L=%d K=%d", height, level, index)
	}
	return c.get(ctx, fmt.Sprintf("tile/%d/%d/%d", height, level, index))
}

func (c *Client) get(ctx context.Context, suffix string) ([]byte, error) {
	base := c.BaseURL
	if base == "" {
		base = DefaultBaseURL
	}
	if !strings.HasSuffix(base, "/") {
		base += "/"
	}
	u, err := url.Parse(base + suffix)
	if err != nil {
		return nil, fmt.Errorf("sumdb: bad URL %q: %w", base+suffix, err)
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, u.String(), nil)
	if err != nil {
		return nil, fmt.Errorf("sumdb: build request: %w", err)
	}
	if c.UserAgent != "" {
		req.Header.Set("User-Agent", c.UserAgent)
	}
	req.Header.Set("Accept", "text/plain, application/octet-stream")
	httpClient := c.HTTP
	if httpClient == nil {
		httpClient = http.DefaultClient
	}
	resp, err := httpClient.Do(req)
	if err != nil {
		return nil, fmt.Errorf("sumdb: GET %s: %w", u, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(io.LimitReader(resp.Body, 1024))
		return nil, fmt.Errorf("sumdb: GET %s: status %d: %s", u, resp.StatusCode, strings.TrimSpace(string(body)))
	}
	// 16 MiB cap protects against an adversarial log returning a
	// hostile payload. Real tiles are at most a few KiB.
	buf, err := io.ReadAll(io.LimitReader(resp.Body, 16*1024*1024))
	if err != nil {
		return nil, fmt.Errorf("sumdb: read body of %s: %w", u, err)
	}
	return buf, nil
}
