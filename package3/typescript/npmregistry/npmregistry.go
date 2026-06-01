// Package npmregistry is the registry.npmjs.org client for the TypeScript bridge.
// Lands in MEP-72 Phase 1.
package npmregistry

import (
	"context"
	"crypto/sha512"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"strings"

	"mochi/package3/typescript/semver"
)

const defaultBaseURL = "https://registry.npmjs.org/"

// Client is an npm registry HTTP client.
type Client struct {
	BaseURL   string
	HTTP      *http.Client
	UserAgent string
}

// PackageMetadata is the full registry metadata document for a package.
type PackageMetadata struct {
	Name     string                     `json:"name"`
	DistTags map[string]string          `json:"dist-tags"`
	Versions map[string]VersionMetadata `json:"versions"`
}

// VersionMetadata is per-version metadata from the npm registry.
type VersionMetadata struct {
	Version string `json:"version"`
	Dist    Dist   `json:"dist"`
}

// Dist contains download URLs and integrity hashes.
type Dist struct {
	Tarball   string `json:"tarball"`
	Shasum    string `json:"shasum"`
	Integrity string `json:"integrity"` // SRI: sha512-<base64>
}

func (c *Client) base() string {
	if c.BaseURL != "" {
		return strings.TrimSuffix(c.BaseURL, "/")
	}
	return strings.TrimSuffix(defaultBaseURL, "/")
}

func (c *Client) httpClient() *http.Client {
	if c.HTTP != nil {
		return c.HTTP
	}
	return http.DefaultClient
}

func (c *Client) userAgent() string {
	if c.UserAgent != "" {
		return c.UserAgent
	}
	return "mochi-ts-bridge/1.0"
}

// GetMetadata returns the full registry metadata document for a package.
func (c *Client) GetMetadata(ctx context.Context, name string) (*PackageMetadata, error) {
	url := c.base() + "/" + name
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return nil, fmt.Errorf("npmregistry: build request: %w", err)
	}
	req.Header.Set("Accept", "application/json")
	req.Header.Set("User-Agent", c.userAgent())

	resp, err := c.httpClient().Do(req)
	if err != nil {
		return nil, fmt.Errorf("npmregistry: GET %s: %w", url, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("npmregistry: GET %s: status %d", url, resp.StatusCode)
	}
	var meta PackageMetadata
	if err := json.NewDecoder(resp.Body).Decode(&meta); err != nil {
		return nil, fmt.Errorf("npmregistry: decode metadata: %w", err)
	}
	return &meta, nil
}

// ResolveVersion finds the highest version satisfying req.
func (c *Client) ResolveVersion(ctx context.Context, name string, req semver.Range) (string, error) {
	meta, err := c.GetMetadata(ctx, name)
	if err != nil {
		return "", err
	}
	var versions []string
	for v := range meta.Versions {
		versions = append(versions, v)
	}
	best, ok := req.MaxSatisfying(versions)
	if !ok {
		return "", fmt.Errorf("npmregistry: no version of %s satisfies %s", name, req)
	}
	return best, nil
}

// DownloadTarball downloads the .tgz for name@version and writes it to w.
// Verifies the SHA-512 integrity if present.
func (c *Client) DownloadTarball(ctx context.Context, name, version string, w io.Writer) error {
	meta, err := c.GetMetadata(ctx, name)
	if err != nil {
		return err
	}
	vm, ok := meta.Versions[version]
	if !ok {
		return fmt.Errorf("npmregistry: version %s@%s not found", name, version)
	}
	url := vm.Dist.Tarball
	if url == "" {
		return fmt.Errorf("npmregistry: no tarball URL for %s@%s", name, version)
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return fmt.Errorf("npmregistry: build tarball request: %w", err)
	}
	req.Header.Set("User-Agent", c.userAgent())

	resp, err := c.httpClient().Do(req)
	if err != nil {
		return fmt.Errorf("npmregistry: GET tarball: %w", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("npmregistry: GET tarball: status %d", resp.StatusCode)
	}

	// Verify integrity if SRI sha512 is present.
	integrity := vm.Dist.Integrity
	if strings.HasPrefix(integrity, "sha512-") {
		h := sha512.New()
		body, err := io.ReadAll(resp.Body)
		if err != nil {
			return fmt.Errorf("npmregistry: read tarball: %w", err)
		}
		h.Write(body)
		got := base64.StdEncoding.EncodeToString(h.Sum(nil))
		want := strings.TrimPrefix(integrity, "sha512-")
		if got != want {
			return fmt.Errorf("npmregistry: integrity mismatch for %s@%s", name, version)
		}
		_, err = w.Write(body)
		return err
	}

	_, err = io.Copy(w, resp.Body)
	return err
}
