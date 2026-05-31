// Package nuget implements a NuGet V3 flat-container client for the MEP-68 bridge.
// Protocol reference: https://learn.microsoft.com/en-us/nuget/api/overview
package nuget

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"strings"
	"time"

	"mochi/package3/dotnet/semver"
)

const (
	// DefaultServiceIndexURL is the nuget.org V3 service index URL.
	DefaultServiceIndexURL = "https://api.nuget.org/v3/index.json"

	// DefaultFlatContainerBaseURL is the default flat-container base URL.
	DefaultFlatContainerBaseURL = "https://api.nuget.org/v3-flatcontainer/"

	// DefaultDownloadURLTemplate is the default .nupkg download URL template.
	DefaultDownloadURLTemplate = "https://api.nuget.org/v3-flatcontainer/{id}/{version}/{id}.{version}.nupkg"

	// DefaultUserAgent is sent in every outbound HTTP request.
	DefaultUserAgent = "mochi/package3-dotnet (MEP-68)"

	// cacheMaxAge is how long version lists are cached in memory.
	cacheMaxAge = 10 * time.Minute
)

// Sentinel errors.
var (
	ErrPackageNotFound = fmt.Errorf("nuget: package not found")
	ErrVersionNotFound = fmt.Errorf("nuget: no version matches constraint")
)

// Client is a NuGet V3 flat-container client.
type Client struct {
	// ServiceIndexURL is the nuget.org V3 service index URL (informational; not
	// used for flat-container requests).
	ServiceIndexURL string
	// FlatContainerBaseURL is the base URL for the flat-container endpoint.
	// Defaults to DefaultFlatContainerBaseURL.
	FlatContainerBaseURL string
	// DownloadURLTemplate is the URL template for .nupkg downloads.
	// Use {id} and {version} as placeholders (both lower-cased).
	DownloadURLTemplate string
	// HTTP is the HTTP client used for all requests.
	HTTP *http.Client
	// UserAgent is sent in every outbound request.
	UserAgent string

	// versionCache caches version lists by lowercase package ID.
	versionCache    map[string]versionCacheEntry
}

type versionCacheEntry struct {
	versions  []string
	fetchedAt time.Time
}

// NewClient creates a NuGet client. Pass an empty serviceIndexURL to use the
// nuget.org default.
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
		versionCache:         make(map[string]versionCacheEntry),
	}
}

// FetchVersions returns all known versions for packageID from the flat-container
// endpoint. Results are cached for cacheMaxAge.
func (c *Client) FetchVersions(ctx context.Context, packageID string) ([]string, error) {
	if packageID == "" {
		return nil, fmt.Errorf("nuget: FetchVersions: empty package id")
	}
	id := strings.ToLower(packageID)
	if e, ok := c.versionCache[id]; ok && time.Since(e.fetchedAt) < cacheMaxAge {
		return e.versions, nil
	}
	base := strings.TrimSuffix(c.FlatContainerBaseURL, "/")
	url := base + "/" + id + "/index.json"
	body, status, err := c.get(ctx, url)
	if err != nil {
		return nil, fmt.Errorf("nuget: FetchVersions %s: %w", packageID, err)
	}
	defer body.Close()
	if status == http.StatusNotFound {
		return nil, ErrPackageNotFound
	}
	if status != http.StatusOK {
		return nil, fmt.Errorf("nuget: FetchVersions %s: HTTP %d", packageID, status)
	}
	var idx VersionsIndex
	if err := json.NewDecoder(body).Decode(&idx); err != nil {
		return nil, fmt.Errorf("nuget: FetchVersions %s: decode: %w", packageID, err)
	}
	if c.versionCache == nil {
		c.versionCache = make(map[string]versionCacheEntry)
	}
	c.versionCache[id] = versionCacheEntry{versions: idx.Versions, fetchedAt: time.Now()}
	return idx.Versions, nil
}

// LatestVersion returns the highest version of packageID that satisfies req.
func (c *Client) LatestVersion(ctx context.Context, packageID string, req semver.Req) (string, error) {
	versions, err := c.FetchVersions(ctx, packageID)
	if err != nil {
		return "", err
	}
	var best semver.Version
	found := false
	for _, vs := range versions {
		v, err := semver.Parse(vs)
		if err != nil {
			continue
		}
		if !req.Satisfies(v) {
			continue
		}
		if !found || best.Compare(v) < 0 {
			best = v
			found = true
		}
	}
	if !found {
		return "", ErrVersionNotFound
	}
	return best.String(), nil
}

// DownloadURLFor returns the .nupkg download URL for packageID at version.
func (c *Client) DownloadURLFor(packageID, version string) (string, error) {
	if packageID == "" {
		return "", fmt.Errorf("nuget: DownloadURLFor: empty package id")
	}
	if version == "" {
		return "", fmt.Errorf("nuget: DownloadURLFor: empty version")
	}
	id := strings.ToLower(packageID)
	ver := strings.ToLower(version)
	tmpl := c.DownloadURLTemplate
	if tmpl == "" {
		tmpl = DefaultDownloadURLTemplate
	}
	url := strings.ReplaceAll(tmpl, "{id}", id)
	url = strings.ReplaceAll(url, "{version}", ver)
	return url, nil
}

// FetchPackage downloads the .nupkg for packageID at version and writes it to w.
// Returns the number of bytes written.
func (c *Client) FetchPackage(ctx context.Context, packageID, version string, w io.Writer) (int64, error) {
	url, err := c.DownloadURLFor(packageID, version)
	if err != nil {
		return 0, err
	}
	body, status, err := c.get(ctx, url)
	if err != nil {
		return 0, fmt.Errorf("nuget: FetchPackage %s %s: %w", packageID, version, err)
	}
	defer body.Close()
	if status == http.StatusNotFound {
		return 0, ErrVersionNotFound
	}
	if status != http.StatusOK {
		return 0, fmt.Errorf("nuget: FetchPackage %s %s: HTTP %d", packageID, version, status)
	}
	return io.Copy(w, body)
}

func (c *Client) get(ctx context.Context, url string) (io.ReadCloser, int, error) {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return nil, 0, err
	}
	ua := c.UserAgent
	if ua == "" {
		ua = DefaultUserAgent
	}
	req.Header.Set("User-Agent", ua)
	req.Header.Set("Accept", "application/json")
	hc := c.HTTP
	if hc == nil {
		hc = http.DefaultClient
	}
	resp, err := hc.Do(req)
	if err != nil {
		return nil, 0, err
	}
	return resp.Body, resp.StatusCode, nil
}
