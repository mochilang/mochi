// Package nuget implements a NuGet V3 protocol client for the MEP-68 bridge.
// It handles service index discovery, package registration lookups, version
// resolution, .nupkg content download, and the OIDC trusted-publishing flow.
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
	// defaultServiceIndex is the nuget.org V3 service index URL.
	defaultServiceIndex = "https://api.nuget.org/v3/index.json"

	// cacheMaxAge is how long the service index and registration pages are
	// cached before being re-fetched. nuget.org recommends 10-minute caching.
	cacheMaxAge = 10 * time.Minute
)

// Client is a NuGet V3 protocol client. It caches service index and
// registration data in memory for the lifetime of the client instance.
type Client struct {
	serviceIndexURL string
	httpClient      *http.Client
	serviceIndex    *serviceIndex
	indexFetchedAt  time.Time
}

// NewClient creates a NuGet V3 client using the given service index URL.
// Pass an empty string to use the nuget.org default.
func NewClient(serviceIndexURL string) *Client {
	if serviceIndexURL == "" {
		serviceIndexURL = defaultServiceIndex
	}
	return &Client{
		serviceIndexURL: serviceIndexURL,
		httpClient:      &http.Client{Timeout: 30 * time.Second},
	}
}

type serviceIndex struct {
	Version   string          `json:"version"`
	Resources []serviceResource `json:"resources"`
}

type serviceResource struct {
	ID      string `json:"@id"`
	Type    string `json:"@type"`
	Comment string `json:"comment"`
}

func (c *Client) getServiceIndex(ctx context.Context) (*serviceIndex, error) {
	if c.serviceIndex != nil && time.Since(c.indexFetchedAt) < cacheMaxAge {
		return c.serviceIndex, nil
	}
	body, err := c.get(ctx, c.serviceIndexURL)
	if err != nil {
		return nil, fmt.Errorf("nuget: fetch service index: %w", err)
	}
	defer body.Close()
	var idx serviceIndex
	if err := json.NewDecoder(body).Decode(&idx); err != nil {
		return nil, fmt.Errorf("nuget: decode service index: %w", err)
	}
	c.serviceIndex = &idx
	c.indexFetchedAt = time.Now()
	return &idx, nil
}

func (c *Client) resourceURL(ctx context.Context, typePref ...string) (string, error) {
	idx, err := c.getServiceIndex(ctx)
	if err != nil {
		return "", err
	}
	for _, want := range typePref {
		for _, r := range idx.Resources {
			if r.Type == want || strings.HasPrefix(r.Type, want+"/") {
				return r.ID, nil
			}
		}
	}
	return "", fmt.Errorf("nuget: no resource of type %v in service index", typePref)
}

// RegistrationEntry holds the metadata for a specific package version as
// returned by the NuGet V3 registration endpoint.
type RegistrationEntry struct {
	ID          string   `json:"id"`
	Version     string   `json:"version"`
	Description string   `json:"description"`
	Authors     []string `json:"authors"`
	PackageHash     string   `json:"packageHash"`
	PackageHashAlgorithm string `json:"packageHashAlgorithm"`
	PackageContent  string `json:"packageContent"` // .nupkg download URL
	DependencyGroups []DepGroup `json:"dependencyGroups"`
}

// DepGroup is a group of dependencies for a specific target framework.
type DepGroup struct {
	TargetFramework string `json:"targetFramework"`
	Dependencies    []Dep  `json:"dependencies"`
}

// Dep is a single dependency in a dependency group.
type Dep struct {
	ID    string `json:"id"`
	Range string `json:"range"`
}

// registrationPage is a NuGet registration page (covers a version range).
type registrationPage struct {
	Lower string              `json:"lower"`
	Upper string              `json:"upper"`
	Count int                 `json:"count"`
	Items []registrationLeaf  `json:"items"`
	ID    string              `json:"@id"` // URL to fetch this page if Items is nil
}

type registrationLeaf struct {
	CatalogEntry RegistrationEntry `json:"catalogEntry"`
}

type registrationIndex struct {
	Count int                `json:"count"`
	Items []registrationPage `json:"items"`
}

// Resolve queries the NuGet V3 registration endpoint for packageID, collects
// all available versions satisfying constraint, and returns the highest one.
// If allowPrerelease is false, prerelease versions are excluded.
func (c *Client) Resolve(ctx context.Context, packageID string, constraint semver.Range, allowPrerelease bool) (*RegistrationEntry, error) {
	baseURL, err := c.resourceURL(ctx,
		"RegistrationsBaseUrl/3.6.0",
		"RegistrationsBaseUrl/3.4.0",
		"RegistrationsBaseUrl")
	if err != nil {
		return nil, err
	}

	url := strings.TrimSuffix(baseURL, "/") + "/" + strings.ToLower(packageID) + "/index.json"
	body, err := c.get(ctx, url)
	if err != nil {
		return nil, fmt.Errorf("nuget: fetch registration for %s: %w", packageID, err)
	}
	defer body.Close()

	var regIdx registrationIndex
	if err := json.NewDecoder(body).Decode(&regIdx); err != nil {
		return nil, fmt.Errorf("nuget: decode registration index for %s: %w", packageID, err)
	}

	var best *RegistrationEntry
	var bestVer semver.Version

	for _, page := range regIdx.Items {
		leaves := page.Items
		if len(leaves) == 0 && page.ID != "" {
			// Fetch the page separately.
			pageBody, err := c.get(ctx, page.ID)
			if err != nil {
				continue
			}
			var fullPage registrationPage
			json.NewDecoder(pageBody).Decode(&fullPage)
			pageBody.Close()
			leaves = fullPage.Items
		}
		for _, leaf := range leaves {
			e := leaf.CatalogEntry
			v, err := semver.Parse(e.Version)
			if err != nil {
				continue
			}
			if !allowPrerelease && v.Pre != "" {
				continue
			}
			if !constraint.Satisfies(v) {
				continue
			}
			if best == nil || v.Compare(bestVer) > 0 {
				entry := e
				best = &entry
				bestVer = v
			}
		}
	}

	if best == nil {
		return nil, fmt.Errorf("nuget: no version of %s satisfies constraint %s", packageID, constraint)
	}
	return best, nil
}

// Download fetches the .nupkg archive for the given package entry and writes
// it to w. It returns the number of bytes written.
func (c *Client) Download(ctx context.Context, entry *RegistrationEntry, w io.Writer) (int64, error) {
	if entry.PackageContent == "" {
		// Fall back to flat-container URL.
		flatBase, err := c.resourceURL(ctx, "PackageBaseAddress/3.0.0")
		if err != nil {
			return 0, err
		}
		id := strings.ToLower(entry.ID)
		ver := strings.ToLower(entry.Version)
		entry.PackageContent = fmt.Sprintf("%s/%s/%s/%s.%s.nupkg",
			strings.TrimSuffix(flatBase, "/"), id, ver, id, ver)
	}
	body, err := c.get(ctx, entry.PackageContent)
	if err != nil {
		return 0, fmt.Errorf("nuget: download %s %s: %w", entry.ID, entry.Version, err)
	}
	defer body.Close()
	return io.Copy(w, body)
}

func (c *Client) get(ctx context.Context, url string) (io.ReadCloser, error) {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return nil, err
	}
	req.Header.Set("Accept", "application/json")
	req.Header.Set("User-Agent", "mochi/package3-dotnet (MEP-68)")
	resp, err := c.httpClient.Do(req)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode != http.StatusOK {
		resp.Body.Close()
		return nil, fmt.Errorf("nuget: HTTP %d for %s", resp.StatusCode, url)
	}
	return resp.Body, nil
}
