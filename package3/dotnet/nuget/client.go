package nuget

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strings"
	"time"

	"mochi/package3/dotnet/semver"
)

// DefaultServiceIndexURL is the canonical NuGet v3 service index endpoint.
const DefaultServiceIndexURL = "https://api.nuget.org/v3/index.json"

// DefaultFlatContainerBaseURL is the NuGet v3 flat-container base URL.
// Package IDs are lower-cased when appended, per NuGet flat-container spec.
const DefaultFlatContainerBaseURL = "https://api.nuget.org/v3-flatcontainer/"

// DefaultDownloadURLTemplate is the .nupkg download URL template.
// The placeholders {id} and {version} are substituted at fetch time.
const DefaultDownloadURLTemplate = "https://api.nuget.org/v3-flatcontainer/{id}/{version}/{id}.{version}.nupkg"

// DefaultUserAgent is the User-Agent header sent on outbound HTTP requests.
const DefaultUserAgent = "mochi-dotnet-bridge/0.1 (+https://mochi-lang.dev)"

// Client is a thin NuGet v3 API client. It is safe for concurrent use
// because the underlying http.Client is. Use NewClient rather than
// constructing a Client directly.
type Client struct {
	// ServiceIndexURL is the NuGet v3 service index endpoint.
	ServiceIndexURL string

	// FlatContainerBaseURL is the flat-container root URL. Must end in '/'.
	FlatContainerBaseURL string

	// DownloadURLTemplate is the .nupkg URL template. Placeholders {id} and
	// {version} are replaced at download time.
	DownloadURLTemplate string

	// HTTP is the underlying transport. nil means use http.DefaultClient
	// with a 30 s timeout.
	HTTP *http.Client

	// UserAgent is sent as the User-Agent header.
	UserAgent string
}

// NewClient returns a Client pre-configured against api.nuget.org with a
// sensible default HTTP timeout. Pass the empty string for the default
// service index URL.
func NewClient(serviceIndexURL string) *Client {
	if serviceIndexURL == "" {
		serviceIndexURL = DefaultServiceIndexURL
	}
	return &Client{
		ServiceIndexURL:      serviceIndexURL,
		FlatContainerBaseURL: DefaultFlatContainerBaseURL,
		DownloadURLTemplate:  DefaultDownloadURLTemplate,
		HTTP:                 &http.Client{Timeout: 30 * time.Second},
		UserAgent:            DefaultUserAgent,
	}
}

// FetchVersions retrieves the list of all published versions for the NuGet
// package with the given id. Returns ErrPackageNotFound if the registry
// responds 404.
func (c *Client) FetchVersions(ctx context.Context, id string) ([]string, error) {
	if id == "" {
		return nil, fmt.Errorf("nuget: empty package id")
	}
	base := c.FlatContainerBaseURL
	if base == "" {
		base = DefaultFlatContainerBaseURL
	}
	if !strings.HasSuffix(base, "/") {
		base += "/"
	}
	target := base + strings.ToLower(id) + "/index.json"
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, target, nil)
	if err != nil {
		return nil, fmt.Errorf("nuget: build request: %w", err)
	}
	c.setUA(req)
	resp, err := c.httpClient().Do(req)
	if err != nil {
		return nil, fmt.Errorf("nuget: GET %s: %w", target, err)
	}
	defer resp.Body.Close()
	switch resp.StatusCode {
	case http.StatusOK:
		// fall through
	case http.StatusNotFound:
		return nil, fmt.Errorf("%w: %s", ErrPackageNotFound, id)
	default:
		body, _ := io.ReadAll(io.LimitReader(resp.Body, 1024))
		return nil, fmt.Errorf("nuget: GET %s: status %d: %s", target, resp.StatusCode, strings.TrimSpace(string(body)))
	}
	var idx VersionsIndex
	if err := json.NewDecoder(resp.Body).Decode(&idx); err != nil {
		return nil, fmt.Errorf("nuget: decode versions index for %q: %w", id, err)
	}
	return idx.Versions, nil
}

// LatestVersion returns the highest published version of the package id that
// satisfies req. Returns ErrVersionNotFound when no published version matches.
func (c *Client) LatestVersion(ctx context.Context, id string, req semver.Req) (string, error) {
	raw, err := c.FetchVersions(ctx, id)
	if err != nil {
		return "", err
	}
	var versions []semver.Version
	for _, s := range raw {
		v, err := semver.Parse(s)
		if err != nil {
			continue // skip malformed entries from the registry
		}
		versions = append(versions, v)
	}
	best, ok := semver.MaxSatisfying(req, versions)
	if !ok {
		return "", fmt.Errorf("%w: %s satisfying %s", ErrVersionNotFound, id, req)
	}
	return best.String(), nil
}

// DownloadURLFor builds the .nupkg download URL for the given package id and
// version using the configured DownloadURLTemplate.
func (c *Client) DownloadURLFor(id, version string) (string, error) {
	if id == "" {
		return "", fmt.Errorf("nuget: empty package id")
	}
	if version == "" {
		return "", fmt.Errorf("nuget: empty version for %q", id)
	}
	tmpl := c.DownloadURLTemplate
	if tmpl == "" {
		tmpl = DefaultDownloadURLTemplate
	}
	lowID := strings.ToLower(id)
	lowVer := strings.ToLower(version)
	url := strings.ReplaceAll(tmpl, "{id}", lowID)
	url = strings.ReplaceAll(url, "{version}", lowVer)
	return url, nil
}

// FetchPackage downloads the .nupkg for the given id and version into w.
// Returns the number of bytes written. Returns ErrVersionNotFound on HTTP 404.
func (c *Client) FetchPackage(ctx context.Context, id, version string, w io.Writer) (int64, error) {
	target, err := c.DownloadURLFor(id, version)
	if err != nil {
		return 0, err
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, target, nil)
	if err != nil {
		return 0, fmt.Errorf("nuget: build request: %w", err)
	}
	c.setUA(req)
	resp, err := c.httpClient().Do(req)
	if err != nil {
		return 0, fmt.Errorf("nuget: GET %s: %w", target, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode == http.StatusNotFound {
		return 0, fmt.Errorf("%w: %s@%s", ErrVersionNotFound, id, version)
	}
	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(io.LimitReader(resp.Body, 1024))
		return 0, fmt.Errorf("nuget: GET %s: status %d: %s", target, resp.StatusCode, strings.TrimSpace(string(body)))
	}
	return io.Copy(w, resp.Body)
}

// ErrPackageNotFound is returned when the NuGet registry has no package with
// the given id (HTTP 404 on the versions index). Check with errors.Is.
var ErrPackageNotFound = errors.New("nuget: package not found")

// ErrVersionNotFound is returned when no published version of a package
// satisfies the requested version range. Check with errors.Is.
var ErrVersionNotFound = errors.New("nuget: version not found")

// httpClient returns the configured HTTP client, falling back to
// http.DefaultClient.
func (c *Client) httpClient() *http.Client {
	if c.HTTP != nil {
		return c.HTTP
	}
	return http.DefaultClient
}

// setUA sets the User-Agent header on req if a user agent is configured.
func (c *Client) setUA(req *http.Request) {
	ua := c.UserAgent
	if ua == "" {
		ua = DefaultUserAgent
	}
	req.Header.Set("User-Agent", ua)
}
