package moduleproxy

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"sort"
	"strings"
	"time"
)

// DefaultProxyURL is the public Go module proxy operated by the Go team.
// The trailing slash is mandatory because every URL is BaseURL + suffix.
const DefaultProxyURL = "https://proxy.golang.org/"

// DefaultUserAgent is the User-Agent header attached to outbound HTTP
// requests. Module proxies log User-Agent for usage analytics; we
// identify ourselves as Mochi.
const DefaultUserAgent = "mochi-go-bridge/0.1 (+https://mochi-lang.dev)"

// Client is a thin Go module proxy client. It is safe for concurrent
// use because the underlying http.Client is. The empty Client is not
// usable because BaseURL is required; use NewClient.
type Client struct {
	// BaseURL is the proxy root, e.g. "https://proxy.golang.org/". Must
	// end in a slash. GOPROXY-style comma-separated lists are not
	// expanded by Client itself; callers wishing to fall back across
	// multiple proxies wrap a sequence of Client instances.
	BaseURL string

	// HTTP is the underlying transport. nil means use http.DefaultClient
	// with a 30s timeout.
	HTTP *http.Client

	// UserAgent is sent as the User-Agent header.
	UserAgent string
}

// NewClient returns a Client pre-configured against proxy.golang.org
// with a sane default HTTP timeout. Pass the empty string for the
// default base URL.
func NewClient(baseURL string) *Client {
	if baseURL == "" {
		baseURL = DefaultProxyURL
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

// VersionInfo is the JSON shape returned by the .info endpoint.
type VersionInfo struct {
	// Version is the canonical semantic version (e.g. "v1.2.3").
	Version string `json:"Version"`
	// Time is the RFC-3339 timestamp the proxy associates with the
	// version. Most public proxies use the upstream git-tag time.
	Time time.Time `json:"Time"`
}

// List retrieves the list of known versions for the given module. The
// returned slice is sorted lexicographically (which matches the
// proxy's own output ordering). An empty list with no error means the
// module has been seen by the proxy but has no tagged versions yet
// (rare but legal).
//
// Returns ErrModuleNotFound (wrapped) when the proxy responds 404.
func (c *Client) List(ctx context.Context, modulePath string) ([]string, error) {
	target, err := c.urlFor(modulePath, "@v/list")
	if err != nil {
		return nil, err
	}
	body, err := c.fetch(ctx, target)
	if err != nil {
		return nil, c.wrapHTTPErr(err, modulePath, "")
	}
	defer body.Close()
	versions, err := parseVersionList(body)
	if err != nil {
		return nil, fmt.Errorf("moduleproxy: parse list of %s: %w", modulePath, err)
	}
	return versions, nil
}

// Info fetches the .info endpoint for module@version.
func (c *Client) Info(ctx context.Context, modulePath, version string) (*VersionInfo, error) {
	escVer, err := EscapeVersion(version)
	if err != nil {
		return nil, err
	}
	target, err := c.urlFor(modulePath, "@v/"+escVer+".info")
	if err != nil {
		return nil, err
	}
	body, err := c.fetch(ctx, target)
	if err != nil {
		return nil, c.wrapHTTPErr(err, modulePath, version)
	}
	defer body.Close()
	var info VersionInfo
	if err := json.NewDecoder(body).Decode(&info); err != nil {
		return nil, fmt.Errorf("moduleproxy: decode .info of %s@%s: %w", modulePath, version, err)
	}
	return &info, nil
}

// Mod fetches the .mod endpoint for module@version and returns the raw
// go.mod bytes. Callers wanting to hash via HashGoMod feed these bytes
// directly.
func (c *Client) Mod(ctx context.Context, modulePath, version string) ([]byte, error) {
	escVer, err := EscapeVersion(version)
	if err != nil {
		return nil, err
	}
	target, err := c.urlFor(modulePath, "@v/"+escVer+".mod")
	if err != nil {
		return nil, err
	}
	body, err := c.fetch(ctx, target)
	if err != nil {
		return nil, c.wrapHTTPErr(err, modulePath, version)
	}
	defer body.Close()
	// Hard cap to keep an adversarial proxy from exhausting memory. A
	// typical go.mod is < 4 KB; 4 MB is well above any reasonable
	// upper bound.
	buf, err := io.ReadAll(io.LimitReader(body, 4*1024*1024))
	if err != nil {
		return nil, fmt.Errorf("moduleproxy: read .mod of %s@%s: %w", modulePath, version, err)
	}
	return buf, nil
}

// Zip streams the .zip endpoint for module@version into w. Returns the
// number of bytes copied. Callers wanting to hash via HashZip wrap w in
// an io.MultiWriter with a sha256 sink.
func (c *Client) Zip(ctx context.Context, modulePath, version string, w io.Writer) (int64, error) {
	escVer, err := EscapeVersion(version)
	if err != nil {
		return 0, err
	}
	target, err := c.urlFor(modulePath, "@v/"+escVer+".zip")
	if err != nil {
		return 0, err
	}
	body, err := c.fetch(ctx, target)
	if err != nil {
		return 0, c.wrapHTTPErr(err, modulePath, version)
	}
	defer body.Close()
	return io.Copy(w, body)
}

// urlFor builds the full proxy URL for a given module + suffix.
func (c *Client) urlFor(modulePath, suffix string) (string, error) {
	esc, err := EscapePath(modulePath)
	if err != nil {
		return "", err
	}
	base := c.BaseURL
	if base == "" {
		base = DefaultProxyURL
	}
	if !strings.HasSuffix(base, "/") {
		base += "/"
	}
	u, err := url.Parse(base)
	if err != nil {
		return "", fmt.Errorf("moduleproxy: bad base url %q: %w", base, err)
	}
	rel := esc + "/" + suffix
	relURL, err := url.Parse(rel)
	if err != nil {
		return "", fmt.Errorf("moduleproxy: bad relative %q: %w", rel, err)
	}
	return u.ResolveReference(relURL).String(), nil
}

// fetch is the common GET helper. It returns the body on success and
// classifies HTTP errors into typed errors (ErrModuleNotFound /
// ErrVersionNotFound / generic httpStatusError) on failure.
func (c *Client) fetch(ctx context.Context, target string) (io.ReadCloser, error) {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, target, nil)
	if err != nil {
		return nil, fmt.Errorf("moduleproxy: build request: %w", err)
	}
	if c.UserAgent != "" {
		req.Header.Set("User-Agent", c.UserAgent)
	}
	req.Header.Set("Accept", "application/octet-stream, application/json, text/plain;q=0.9")
	httpClient := c.HTTP
	if httpClient == nil {
		httpClient = http.DefaultClient
	}
	resp, err := httpClient.Do(req)
	if err != nil {
		return nil, fmt.Errorf("moduleproxy: GET %s: %w", target, err)
	}
	if resp.StatusCode == http.StatusOK {
		return resp.Body, nil
	}
	// Read up to 1 KB of body for the diagnostic.
	body, _ := io.ReadAll(io.LimitReader(resp.Body, 1024))
	resp.Body.Close()
	return nil, &httpStatusError{
		URL:    target,
		Status: resp.StatusCode,
		Body:   strings.TrimSpace(string(body)),
	}
}

// wrapHTTPErr classifies a generic httpStatusError into the typed
// errors callers may switch on.
func (c *Client) wrapHTTPErr(err error, modulePath, version string) error {
	var hse *httpStatusError
	if !errors.As(err, &hse) {
		return err
	}
	if hse.Status == http.StatusNotFound || hse.Status == http.StatusGone {
		if version != "" {
			return fmt.Errorf("%w: %s@%s", ErrVersionNotFound, modulePath, version)
		}
		return fmt.Errorf("%w: %s", ErrModuleNotFound, modulePath)
	}
	return err
}

// parseVersionList parses the @v/list response body, which is
// newline-delimited semver strings. Empty lines are ignored.
func parseVersionList(r io.Reader) ([]string, error) {
	buf, err := io.ReadAll(io.LimitReader(r, 8*1024*1024))
	if err != nil {
		return nil, err
	}
	lines := strings.Split(string(buf), "\n")
	out := make([]string, 0, len(lines))
	for _, line := range lines {
		v := strings.TrimSpace(line)
		if v == "" {
			continue
		}
		out = append(out, v)
	}
	sort.Strings(out)
	return out, nil
}

// httpStatusError carries the status code plus a snippet of body for
// diagnostics. It is the unwrapped form errors.As targets in
// wrapHTTPErr.
type httpStatusError struct {
	URL    string
	Status int
	Body   string
}

func (e *httpStatusError) Error() string {
	return fmt.Sprintf("moduleproxy: GET %s: status %d: %s", e.URL, e.Status, e.Body)
}

// ErrModuleNotFound is returned when the proxy responds 404 / 410 for a
// module path. Wrapped via fmt.Errorf %w.
var ErrModuleNotFound = errors.New("moduleproxy: module not found")

// ErrVersionNotFound is returned when the proxy responds 404 / 410 for
// a specific module@version. Wrapped via fmt.Errorf %w.
var ErrVersionNotFound = errors.New("moduleproxy: version not found")
