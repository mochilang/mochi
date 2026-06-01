// Package jsrregistry is the jsr.io registry client for the TypeScript bridge.
// Lands in MEP-72 Phase 2.
package jsrregistry

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
)

const defaultBaseURL = "https://api.jsr.io"

// Client is a JSR registry HTTP client.
type Client struct {
	BaseURL string
	HTTP    *http.Client
}

// JsrPackageInfo is per-version metadata from the JSR registry.
type JsrPackageInfo struct {
	Scope   string            `json:"scope"`
	Package string            `json:"package"`
	Version string            `json:"version"`
	Exports map[string]string `json:"exports"`
}

func (c *Client) base() string {
	if c.BaseURL != "" {
		return strings.TrimSuffix(c.BaseURL, "/")
	}
	return defaultBaseURL
}

func (c *Client) httpClient() *http.Client {
	if c.HTTP != nil {
		return c.HTTP
	}
	return http.DefaultClient
}

func (c *Client) get(ctx context.Context, path string, out interface{}) error {
	url := c.base() + path
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return fmt.Errorf("jsrregistry: build request: %w", err)
	}
	req.Header.Set("Accept", "application/json")
	resp, err := c.httpClient().Do(req)
	if err != nil {
		return fmt.Errorf("jsrregistry: GET %s: %w", url, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("jsrregistry: GET %s: status %d", url, resp.StatusCode)
	}
	return json.NewDecoder(resp.Body).Decode(out)
}

type versionsResponse struct {
	Versions []struct {
		Version string `json:"version"`
	} `json:"versions"`
}

// ListVersions returns all published versions of @scope/package.
func (c *Client) ListVersions(ctx context.Context, scope, pkg string) ([]string, error) {
	var resp versionsResponse
	if err := c.get(ctx, fmt.Sprintf("/scopes/%s/packages/%s/versions", scope, pkg), &resp); err != nil {
		return nil, err
	}
	vs := make([]string, 0, len(resp.Versions))
	for _, v := range resp.Versions {
		vs = append(vs, v.Version)
	}
	return vs, nil
}

// GetPackageInfo returns metadata for @scope/package@version.
func (c *Client) GetPackageInfo(ctx context.Context, scope, pkg, version string) (*JsrPackageInfo, error) {
	var info JsrPackageInfo
	if err := c.get(ctx, fmt.Sprintf("/scopes/%s/packages/%s/versions/%s", scope, pkg, version), &info); err != nil {
		return nil, err
	}
	info.Scope = scope
	info.Package = pkg
	info.Version = version
	return &info, nil
}

// DownloadExports downloads the package's TypeScript export files to outDir.
// JSR provides TypeScript sources directly at cdn.jsr.io.
func (c *Client) DownloadExports(ctx context.Context, scope, pkg, version, outDir string) error {
	info, err := c.GetPackageInfo(ctx, scope, pkg, version)
	if err != nil {
		return err
	}
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return fmt.Errorf("jsrregistry: mkdir %s: %w", outDir, err)
	}
	cdnBase := "https://cdn.jsr.io"
	for _, exportPath := range info.Exports {
		relPath := strings.TrimPrefix(exportPath, "./")
		url := fmt.Sprintf("%s/@%s/%s/%s/%s", cdnBase, scope, pkg, version, relPath)
		req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
		if err != nil {
			return fmt.Errorf("jsrregistry: build download request: %w", err)
		}
		resp, err := c.httpClient().Do(req)
		if err != nil {
			return fmt.Errorf("jsrregistry: download %s: %w", url, err)
		}
		defer resp.Body.Close()
		if resp.StatusCode != http.StatusOK {
			return fmt.Errorf("jsrregistry: download %s: status %d", url, resp.StatusCode)
		}
		outPath := filepath.Join(outDir, filepath.FromSlash(relPath))
		if err := os.MkdirAll(filepath.Dir(outPath), 0o755); err != nil {
			return fmt.Errorf("jsrregistry: mkdir for export: %w", err)
		}
		f, err := os.Create(outPath)
		if err != nil {
			return fmt.Errorf("jsrregistry: create export file: %w", err)
		}
		_, copyErr := io.Copy(f, resp.Body)
		f.Close()
		if copyErr != nil {
			return fmt.Errorf("jsrregistry: write export %s: %w", relPath, copyErr)
		}
	}
	return nil
}
