package maven

import (
	"context"
	"fmt"
	"net/http"
	"strings"
	"time"

	"github.com/mochilang/mochi/package3/kotlin/semver"
)

// Client is a Maven repository client that can fetch POM files, metadata, and
// Gradle Module Metadata from one or more registries.
type Client struct {
	registries []Registry
	http       *http.Client
}

// ClientOption is a functional option for configuring a Client.
type ClientOption func(*Client)

// WithRegistry adds a registry to the client's registry list.
// Registries are checked in order; the first matching one is used.
func WithRegistry(r Registry) ClientOption {
	return func(c *Client) {
		c.registries = append(c.registries, r)
	}
}

// WithHTTPClient sets a custom HTTP client.
func WithHTTPClient(hc *http.Client) ClientOption {
	return func(c *Client) {
		c.http = hc
	}
}

// WithTimeout sets the HTTP client timeout.
func WithTimeout(d time.Duration) ClientOption {
	return func(c *Client) {
		if c.http == nil {
			c.http = &http.Client{}
		}
		c.http.Timeout = d
	}
}

// NewClient creates a new Maven registry client with the given options.
// If no registry is specified, MavenCentral is used as the default fallback.
func NewClient(opts ...ClientOption) *Client {
	c := &Client{
		http: &http.Client{Timeout: 30 * time.Second},
	}
	for _, opt := range opts {
		opt(c)
	}
	return c
}

// registryFor returns the best registry for a coordinate.
func (c *Client) registryFor(coord Coordinate) Registry {
	if len(c.registries) == 0 {
		return RegistryMavenCentral
	}
	return RegistryFor(c.registries, coord)
}

// ArtifactURL returns the full URL for an artifact with the given extension.
func (c *Client) ArtifactURL(coord Coordinate, ext string) string {
	reg := c.registryFor(coord)
	base := strings.TrimRight(reg.BaseURL, "/")
	groupPath := strings.ReplaceAll(coord.GroupID, ".", "/")
	name := coord.ArtifactID + "-" + coord.Version
	if coord.Classifier != "" {
		name += "-" + coord.Classifier
	}
	return fmt.Sprintf("%s/%s/%s/%s/%s.%s",
		base, groupPath, coord.ArtifactID, coord.Version, name, ext)
}

// metadataURL returns the URL for maven-metadata.xml for a group:artifact.
func (c *Client) metadataURL(groupID, artifactID string) string {
	coord := Coordinate{GroupID: groupID, ArtifactID: artifactID}
	reg := c.registryFor(coord)
	base := strings.TrimRight(reg.BaseURL, "/")
	groupPath := strings.ReplaceAll(groupID, ".", "/")
	return fmt.Sprintf("%s/%s/%s/maven-metadata.xml", base, groupPath, artifactID)
}

// FetchPOM fetches and parses the POM for the given coordinate.
func (c *Client) FetchPOM(ctx context.Context, coord Coordinate) (*POM, error) {
	url := c.ArtifactURL(coord, "pom")
	resp, err := c.get(ctx, url)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	pom, err := ParsePOM(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("client: parse POM %s: %w", url, err)
	}
	return pom, nil
}

// FetchMavenMetadata fetches and parses maven-metadata.xml for a group:artifact pair.
func (c *Client) FetchMavenMetadata(ctx context.Context, groupID, artifactID string) (*MavenMetadata, error) {
	url := c.metadataURL(groupID, artifactID)
	resp, err := c.get(ctx, url)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	meta, err := ParseMavenMetadata(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("client: parse maven-metadata %s: %w", url, err)
	}
	return meta, nil
}

// FetchGradleModule fetches and parses the Gradle Module Metadata for a coordinate.
func (c *Client) FetchGradleModule(ctx context.Context, coord Coordinate) (*GradleModule, error) {
	url := c.ArtifactURL(coord, "module")
	resp, err := c.get(ctx, url)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	gm, err := ParseGradleModule(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("client: parse gradle module %s: %w", url, err)
	}
	return gm, nil
}

// ResolveVersion resolves a version range expression to a concrete version by
// fetching maven-metadata.xml and selecting the best matching version.
// Special values: "LATEST" returns the latest version, "RELEASE" returns the release version.
func (c *Client) ResolveVersion(ctx context.Context, groupID, artifactID, versionRange string) (string, error) {
	r, err := semver.ParseRange(versionRange)
	if err != nil {
		return "", fmt.Errorf("client: invalid version range %q: %w", versionRange, err)
	}

	meta, err := c.FetchMavenMetadata(ctx, groupID, artifactID)
	if err != nil {
		return "", err
	}

	switch r.Kind {
	case semver.RangeLatest:
		if meta.Versioning.Latest != "" {
			return meta.Versioning.Latest, nil
		}
		if len(meta.Versioning.Versions) > 0 {
			return meta.Versioning.Versions[len(meta.Versioning.Versions)-1], nil
		}
		return "", &artifactNotFoundError{groupID: groupID, artifactID: artifactID}

	case semver.RangeRelease:
		if meta.Versioning.Release != "" {
			return meta.Versioning.Release, nil
		}
		// Fall back to latest non-SNAPSHOT version
		for i := len(meta.Versioning.Versions) - 1; i >= 0; i-- {
			v := meta.Versioning.Versions[i]
			if !strings.Contains(v, "SNAPSHOT") {
				return v, nil
			}
		}
		return "", &artifactNotFoundError{groupID: groupID, artifactID: artifactID}

	case semver.RangeExact:
		// For exact versions, return directly without checking metadata
		return versionRange, nil
	}

	// For range constraints, find the highest matching version
	var best *semver.Version
	var bestStr string
	for _, vs := range meta.Versioning.Versions {
		v, err := semver.Parse(vs)
		if err != nil {
			continue
		}
		if !r.Matches(v) {
			continue
		}
		if best == nil || semver.Compare(v, *best) > 0 {
			v2 := v
			best = &v2
			bestStr = vs
		}
	}
	if best == nil {
		return "", fmt.Errorf("client: no version matching %q found for %s:%s", versionRange, groupID, artifactID)
	}
	return bestStr, nil
}

// get performs an HTTP GET request and returns the response.
func (c *Client) get(ctx context.Context, url string) (*http.Response, error) {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return nil, fmt.Errorf("client: build request %s: %w", url, err)
	}
	resp, err := c.http.Do(req)
	if err != nil {
		return nil, fmt.Errorf("client: GET %s: %w", url, err)
	}
	if resp.StatusCode == http.StatusNotFound {
		resp.Body.Close()
		return nil, &notFoundError{url: url, status: resp.StatusCode}
	}
	if resp.StatusCode != http.StatusOK {
		resp.Body.Close()
		return nil, fmt.Errorf("client: GET %s returned HTTP %d", url, resp.StatusCode)
	}
	return resp, nil
}

// notFoundError is returned when an artifact is not found (HTTP 404).
type notFoundError struct {
	url    string
	status int
}

func (e *notFoundError) Error() string {
	return fmt.Sprintf("artifact not found: %s (HTTP %d)", e.url, e.status)
}

// artifactNotFoundError is returned when no version matches.
type artifactNotFoundError struct {
	groupID    string
	artifactID string
}

func (e *artifactNotFoundError) Error() string {
	return fmt.Sprintf("no versions found for %s:%s", e.groupID, e.artifactID)
}
