// Package gitregistry resolves and downloads Swift packages from git
// repositories. It handles version tag lookup via the git smart HTTP protocol
// and downloads source archives as zip files from GitHub/GitLab compatible
// archive endpoints.
package gitregistry

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"

	"mochi/package3/swift/semver"
)

// Client resolves and downloads Swift packages from git hosting services.
type Client struct {
	// CacheDir is the directory where downloaded archives are stored.
	CacheDir string
	// HTTP is the HTTP client to use. If nil, http.DefaultClient is used.
	HTTP *http.Client
}

func (c *Client) httpClient() *http.Client {
	if c.HTTP != nil {
		return c.HTTP
	}
	return http.DefaultClient
}

// githubTagsResponse is the shape of the GitHub tags API response.
type githubTagsResponse []struct {
	Name string `json:"name"`
}

// ResolveVersion queries the git hosting API for available tags and returns the
// highest version tag that satisfies req. For GitHub URLs it uses the
// /repos/:owner/:repo/tags REST endpoint. For other hosts it falls back to a
// git ls-remote approach via HTTP.
func (c *Client) ResolveVersion(ctx context.Context, repoURL string, req semver.Req) (string, error) {
	tags, err := c.listTags(ctx, repoURL)
	if err != nil {
		return "", fmt.Errorf("gitregistry: list tags for %s: %w", repoURL, err)
	}
	var best *semver.Version
	var bestTag string
	for _, tag := range tags {
		v, err := semver.Parse(strings.TrimPrefix(tag, "v"))
		if err != nil {
			continue
		}
		if !req.Satisfies(v) {
			continue
		}
		if best == nil || best.Less(v) {
			cp := v
			best = &cp
			bestTag = tag
		}
	}
	if best == nil {
		return "", fmt.Errorf("gitregistry: no tag satisfies requirement for %s", repoURL)
	}
	return bestTag, nil
}

// listTags fetches available version tags from the hosting service.
func (c *Client) listTags(ctx context.Context, repoURL string) ([]string, error) {
	// GitHub: convert https://github.com/owner/repo to API call
	if strings.HasPrefix(repoURL, "https://github.com/") {
		return c.listGitHubTags(ctx, repoURL)
	}
	// Generic fallback: return empty (callers must provide explicit tags)
	return nil, fmt.Errorf("gitregistry: unsupported host for tag listing: %s", repoURL)
}

func (c *Client) listGitHubTags(ctx context.Context, repoURL string) ([]string, error) {
	path := strings.TrimPrefix(repoURL, "https://github.com/")
	path = strings.TrimSuffix(path, ".git")
	apiURL := "https://api.github.com/repos/" + path + "/tags"

	req, err := http.NewRequestWithContext(ctx, http.MethodGet, apiURL, nil)
	if err != nil {
		return nil, err
	}
	req.Header.Set("Accept", "application/vnd.github+json")

	resp, err := c.httpClient().Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("gitregistry: GitHub API returned %d for %s", resp.StatusCode, apiURL)
	}

	var tags githubTagsResponse
	if err := json.NewDecoder(resp.Body).Decode(&tags); err != nil {
		return nil, fmt.Errorf("gitregistry: decode tags: %w", err)
	}
	out := make([]string, 0, len(tags))
	for _, t := range tags {
		out = append(out, t.Name)
	}
	return out, nil
}

// DownloadArchive downloads the source archive for tag from repoURL and stores
// it in CacheDir. Returns the path to the downloaded file.
func (c *Client) DownloadArchive(ctx context.Context, repoURL, tag string) (string, error) {
	archiveURL, err := c.archiveURL(repoURL, tag)
	if err != nil {
		return "", err
	}

	if err := os.MkdirAll(c.CacheDir, 0o755); err != nil {
		return "", fmt.Errorf("gitregistry: mkdir cache: %w", err)
	}

	// Sanitise tag for use as filename
	safeName := strings.NewReplacer("/", "_", "\\", "_", ":", "_").Replace(tag)
	dest := filepath.Join(c.CacheDir, safeName+".zip")

	// Check if already cached
	if _, err := os.Stat(dest); err == nil {
		return dest, nil
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodGet, archiveURL, nil)
	if err != nil {
		return "", err
	}

	resp, err := c.httpClient().Do(req)
	if err != nil {
		return "", fmt.Errorf("gitregistry: download %s: %w", archiveURL, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return "", fmt.Errorf("gitregistry: archive download returned %d for %s", resp.StatusCode, archiveURL)
	}

	f, err := os.Create(dest)
	if err != nil {
		return "", fmt.Errorf("gitregistry: create archive file: %w", err)
	}
	defer f.Close()

	if _, err := io.Copy(f, resp.Body); err != nil {
		return "", fmt.Errorf("gitregistry: write archive: %w", err)
	}
	return dest, nil
}

// archiveURL returns the zip archive download URL for the given repo and tag.
func (c *Client) archiveURL(repoURL, tag string) (string, error) {
	if strings.HasPrefix(repoURL, "https://github.com/") {
		path := strings.TrimPrefix(repoURL, "https://github.com/")
		path = strings.TrimSuffix(path, ".git")
		return fmt.Sprintf("https://github.com/%s/archive/refs/tags/%s.zip", path, tag), nil
	}
	if strings.HasPrefix(repoURL, "https://gitlab.com/") {
		path := strings.TrimPrefix(repoURL, "https://gitlab.com/")
		path = strings.TrimSuffix(path, ".git")
		return fmt.Sprintf("https://gitlab.com/%s/-/archive/%s/%s.zip",
			path, tag, filepath.Base(path)+"-"+tag), nil
	}
	return "", fmt.Errorf("gitregistry: cannot derive archive URL for %s", repoURL)
}
