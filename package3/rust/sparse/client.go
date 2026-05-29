package sparse

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

// DefaultIndexURL is crates.io's sparse-index endpoint as documented in
// the cargo book §"Sparse protocol". The trailing slash is mandatory
// because every fetch is BaseURL + BucketPath(name).
const DefaultIndexURL = "https://index.crates.io/"

// DefaultDownloadURL is the canonical cargo download endpoint for the
// crates.io registry. The path template substitutes {crate} for the
// crate name and {version} for the semver string.
const DefaultDownloadURL = "https://static.crates.io/crates/{crate}/{crate}-{version}.crate"

// DefaultUserAgent is the User-Agent header attached to outbound HTTP
// requests. Cargo registries log User-Agent and may rate-limit unknown
// clients, so we identify ourselves as Mochi.
const DefaultUserAgent = "mochi-rust-bridge/0.1 (+https://mochi-lang.dev)"

// Client is a thin sparse-index client. It is safe for concurrent use
// because the underlying http.Client is. The empty Client is not usable
// because BaseURL is required; use NewClient.
type Client struct {
	// BaseURL is the sparse-index root, e.g. "https://index.crates.io/".
	// Must end in a slash.
	BaseURL string

	// DownloadURL is the .crate tarball template. The placeholders
	// {crate} and {version} are substituted at fetch time. Leave empty
	// to use DefaultDownloadURL.
	DownloadURL string

	// HTTP is the underlying transport. nil means use http.DefaultClient
	// with a 30s timeout.
	HTTP *http.Client

	// UserAgent is sent as the User-Agent header.
	UserAgent string
}

// NewClient returns a Client pre-configured against crates.io with a
// sane default HTTP timeout. Pass the empty string for the default base
// URL.
func NewClient(baseURL string) *Client {
	if baseURL == "" {
		baseURL = DefaultIndexURL
	}
	if !strings.HasSuffix(baseURL, "/") {
		baseURL += "/"
	}
	return &Client{
		BaseURL:     baseURL,
		DownloadURL: DefaultDownloadURL,
		HTTP:        &http.Client{Timeout: 30 * time.Second},
		UserAgent:   DefaultUserAgent,
	}
}

// IndexURL returns the absolute URL of the index file for crate name.
func (c *Client) IndexURL(name string) (string, error) {
	bucket, err := BucketPath(name)
	if err != nil {
		return "", err
	}
	base := c.BaseURL
	if base == "" {
		base = DefaultIndexURL
	}
	if !strings.HasSuffix(base, "/") {
		base += "/"
	}
	u, err := url.Parse(base)
	if err != nil {
		return "", fmt.Errorf("sparse: bad base url %q: %w", base, err)
	}
	rel, err := url.Parse(bucket)
	if err != nil {
		return "", fmt.Errorf("sparse: bad bucket %q: %w", bucket, err)
	}
	return u.ResolveReference(rel).String(), nil
}

// FetchIndex retrieves and parses the sparse-index file for the named
// crate. Returns ErrCrateNotFound if the registry responds 404.
func (c *Client) FetchIndex(ctx context.Context, name string) ([]CrateEntry, error) {
	target, err := c.IndexURL(name)
	if err != nil {
		return nil, err
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, target, nil)
	if err != nil {
		return nil, fmt.Errorf("sparse: build request: %w", err)
	}
	if c.UserAgent != "" {
		req.Header.Set("User-Agent", c.UserAgent)
	}
	req.Header.Set("Accept", "text/plain")
	httpClient := c.HTTP
	if httpClient == nil {
		httpClient = http.DefaultClient
	}
	resp, err := httpClient.Do(req)
	if err != nil {
		return nil, fmt.Errorf("sparse: GET %s: %w", target, err)
	}
	defer resp.Body.Close()
	switch resp.StatusCode {
	case http.StatusOK:
		// fall through
	case http.StatusNotFound:
		return nil, fmt.Errorf("%w: %s", ErrCrateNotFound, name)
	default:
		body, _ := io.ReadAll(io.LimitReader(resp.Body, 1024))
		return nil, fmt.Errorf("sparse: GET %s: status %d: %s", target, resp.StatusCode, strings.TrimSpace(string(body)))
	}
	return ParseIndex(resp.Body)
}

// DownloadURLFor builds the .crate URL for a given name/version using
// the configured DownloadURL template.
func (c *Client) DownloadURLFor(name, version string) (string, error) {
	tmpl := c.DownloadURL
	if tmpl == "" {
		tmpl = DefaultDownloadURL
	}
	if err := ValidateCrateName(name); err != nil {
		return "", err
	}
	if version == "" {
		return "", fmt.Errorf("sparse: empty version for %q", name)
	}
	url := strings.ReplaceAll(tmpl, "{crate}", name)
	url = strings.ReplaceAll(url, "{version}", version)
	return url, nil
}

// FetchCrate retrieves the .crate tarball bytes for name@version into
// the provided writer. Returns the number of bytes written and any
// error. Does not verify the checksum; the caller is expected to use
// Cache.Store which verifies as it writes.
func (c *Client) FetchCrate(ctx context.Context, name, version string, w io.Writer) (int64, error) {
	target, err := c.DownloadURLFor(name, version)
	if err != nil {
		return 0, err
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, target, nil)
	if err != nil {
		return 0, fmt.Errorf("sparse: build request: %w", err)
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
		return 0, fmt.Errorf("sparse: GET %s: %w", target, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode == http.StatusNotFound {
		return 0, fmt.Errorf("%w: %s@%s", ErrCrateVersionNotFound, name, version)
	}
	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(io.LimitReader(resp.Body, 1024))
		return 0, fmt.Errorf("sparse: GET %s: status %d: %s", target, resp.StatusCode, strings.TrimSpace(string(body)))
	}
	return io.Copy(w, resp.Body)
}

// ErrCrateNotFound is returned when the index file for a crate name is
// absent (HTTP 404). Wrapped via fmt.Errorf %w; check with errors.Is.
var ErrCrateNotFound = errors.New("sparse: crate not found")

// ErrCrateVersionNotFound is returned when the .crate tarball for a
// name@version is absent. Wrapped via fmt.Errorf %w.
var ErrCrateVersionNotFound = errors.New("sparse: crate version not found")
