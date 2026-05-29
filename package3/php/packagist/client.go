package packagist

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strings"
	"time"
)

// DefaultBaseURL is the Packagist v2 sparse-index API root. Every fetch is
// BaseURL + "p2/" + vendor + "/" + package + ".json".
const DefaultBaseURL = "https://packagist.org/"

// DefaultUserAgent identifies outbound requests to Packagist. Packagist logs
// User-Agent and may rate-limit unknown clients.
const DefaultUserAgent = "mochi-php-bridge/0.1 (+https://mochi-lang.dev)"

// Client is a Packagist v2 sparse-index client. It is safe for concurrent use.
// Use NewClient rather than constructing directly; the zero value is unusable
// because BaseURL is required.
type Client struct {
	// BaseURL is the Packagist root URL, e.g. "https://packagist.org/".
	// Must end in a slash.
	BaseURL string
	// HTTP is the underlying transport. nil means a 30-second default client.
	HTTP *http.Client
	// UserAgent is sent as the User-Agent header.
	UserAgent string
}

// NewClient returns a Client pre-configured against packagist.org.
// Pass the empty string for baseURL to use the default.
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

// PackageURL returns the absolute Packagist v2 JSON URL for vendor/name.
func (c *Client) PackageURL(vendor, name string) (string, error) {
	if vendor == "" {
		return "", fmt.Errorf("packagist: empty vendor")
	}
	if name == "" {
		return "", fmt.Errorf("packagist: empty name")
	}
	base := c.BaseURL
	if base == "" {
		base = DefaultBaseURL
	}
	return base + "p2/" + vendor + "/" + name + ".json", nil
}

// FetchPackage retrieves and parses the Packagist v2 index document for
// vendor/package. Returns ErrPackageNotFound when the registry responds 404.
func (c *Client) FetchPackage(ctx context.Context, vendor, name string) (*PackageResponse, error) {
	target, err := c.PackageURL(vendor, name)
	if err != nil {
		return nil, err
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, target, nil)
	if err != nil {
		return nil, fmt.Errorf("packagist: build request: %w", err)
	}
	if c.UserAgent != "" {
		req.Header.Set("User-Agent", c.UserAgent)
	}
	req.Header.Set("Accept", "application/json")
	httpClient := c.HTTP
	if httpClient == nil {
		httpClient = http.DefaultClient
	}
	resp, err := httpClient.Do(req)
	if err != nil {
		return nil, fmt.Errorf("packagist: GET %s: %w", target, err)
	}
	defer resp.Body.Close()
	switch resp.StatusCode {
	case http.StatusOK:
		// fall through
	case http.StatusNotFound:
		return nil, fmt.Errorf("%w: %s/%s", ErrPackageNotFound, vendor, name)
	default:
		body, _ := io.ReadAll(io.LimitReader(resp.Body, 1024))
		return nil, fmt.Errorf("packagist: GET %s: status %d: %s", target, resp.StatusCode, strings.TrimSpace(string(body)))
	}
	var pr PackageResponse
	if err := json.NewDecoder(resp.Body).Decode(&pr); err != nil {
		return nil, fmt.Errorf("packagist: decode %s/%s: %w", vendor, name, err)
	}
	return &pr, nil
}

// ErrPackageNotFound is returned when the index document for a Composer
// package name is absent (HTTP 404). Wrapped via fmt.Errorf %w.
var ErrPackageNotFound = errors.New("packagist: package not found")
