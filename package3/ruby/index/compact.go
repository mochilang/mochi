// Package index implements a client for the RubyGems compact index protocol
// used by Bundler. The compact index serves gem metadata (versions, checksums,
// dependencies) from https://index.rubygems.org/ without requiring the
// full rubygems.org REST API.
//
// Phase 1 of MEP-76 implements this package.
// See [website/docs/research/0076/11-version-resolution.md] for the format.
package index

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strings"

	"mochi/package3/ruby/semver"
)

const defaultIndexBase = "https://index.rubygems.org"

// ErrGemVersionNotFound is returned by SelectBestVersion when no version
// in the list satisfies the given requirement.
var ErrGemVersionNotFound = errors.New("gem version not found")

// Client fetches gem metadata from the RubyGems compact index.
type Client struct {
	BaseURL   string
	HTTP      *http.Client
	UserAgent string
}

// NewClient returns a Client pointing at the official RubyGems compact index.
// If baseURL is empty the default index URL is used.
func NewClient(baseURL string) *Client {
	if baseURL == "" {
		baseURL = defaultIndexBase
	}
	return &Client{
		BaseURL: baseURL,
		HTTP:    http.DefaultClient,
	}
}

// GemVersion is one version entry from the compact index /info/<gem> endpoint.
type GemVersion struct {
	Version      string
	Platform     string   // "" for ruby-universal, "x86_64-linux", "arm64-darwin", etc.
	SHA256       string   // hex-encoded SHA-256 of the .gem tarball
	Dependencies []GemDep
}

// GemDep is one runtime dependency declared by a gem version.
type GemDep struct {
	Name       string
	Constraint string // e.g. ">= 1.0", "~> 2.1"
}

func (c *Client) httpClient() *http.Client {
	if c.HTTP != nil {
		return c.HTTP
	}
	return http.DefaultClient
}

func (c *Client) do(req *http.Request) (*http.Response, error) {
	if c.UserAgent != "" {
		req.Header.Set("User-Agent", c.UserAgent)
	}
	return c.httpClient().Do(req)
}

// FetchVersions fetches all versions of a gem from the compact index
// /info/<gem> endpoint and returns them newest-first.
func (c *Client) FetchVersions(ctx context.Context, gem string) ([]GemVersion, error) {
	url := fmt.Sprintf("%s/info/%s", c.BaseURL, gem)
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return nil, err
	}
	resp, err := c.do(req)
	if err != nil {
		return nil, fmt.Errorf("compact index fetch %s: %w", url, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode == http.StatusNotFound {
		return nil, fmt.Errorf("gem not found: %q", gem)
	}
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("compact index %s: HTTP %d", url, resp.StatusCode)
	}
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}
	return parseInfoBody(string(body))
}

// VerifyAndDownload downloads a .gem tarball, verifies its SHA-256, and
// optionally writes it to destDir. Returns an error on SHA-256 mismatch.
func (c *Client) VerifyAndDownload(ctx context.Context, gem, version, platform, wantSHA256, destDir string) error {
	gemFile := gem + "-" + version
	if platform != "" {
		gemFile += "-" + platform
	}
	gemFile += ".gem"
	url := fmt.Sprintf("%s/gems/%s", c.BaseURL, gemFile)
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return err
	}
	resp, err := c.do(req)
	if err != nil {
		return fmt.Errorf("download %s: %w", url, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("download %s: HTTP %d", url, resp.StatusCode)
	}
	data, err := io.ReadAll(resp.Body)
	if err != nil {
		return err
	}
	if wantSHA256 != "" {
		sum := sha256.Sum256(data)
		got := hex.EncodeToString(sum[:])
		if got != wantSHA256 {
			return fmt.Errorf("gem %s@%s SHA-256 mismatch: got %s want %s", gem, version, got, wantSHA256)
		}
	}
	_ = destDir // phase 8: write to content-addressed cache
	return nil
}

// parseInfoBody parses the /info/<gem> compact index response body.
// Format per line: <version>[-<platform>] [dep:<name>:<constraint>,...] |checksum:<sha256>
func parseInfoBody(body string) ([]GemVersion, error) {
	var versions []GemVersion
	for _, line := range strings.Split(strings.TrimSpace(body), "\n") {
		if line == "" || strings.HasPrefix(line, "---") {
			continue
		}
		v, err := parseInfoLine(line)
		if err != nil {
			return nil, err
		}
		versions = append(versions, v)
	}
	// Reverse so newest is first (compact index lists oldest-first).
	for i, j := 0, len(versions)-1; i < j; i, j = i+1, j-1 {
		versions[i], versions[j] = versions[j], versions[i]
	}
	return versions, nil
}

func parseInfoLine(line string) (GemVersion, error) {
	// Format: <ver-platform> <deps> |checksum:<sha256>
	// Example: 1.2.3 dep:rack:~> 2.0,dep:activerecord:>= 6.0 |checksum:abc123
	parts := strings.SplitN(line, " ", 2)
	vp := parts[0]
	var v GemVersion
	if idx := platformSplit(vp); idx >= 0 {
		v.Version = vp[:idx]
		v.Platform = vp[idx+1:]
	} else {
		v.Version = vp
	}
	if len(parts) < 2 {
		return v, nil
	}
	rest := parts[1]
	// Extract checksum.
	if i := strings.Index(rest, "|checksum:"); i >= 0 {
		sha := rest[i+10:]
		if end := strings.Index(sha, "|"); end >= 0 {
			sha = sha[:end]
		}
		v.SHA256 = sha
		rest = rest[:i]
	}
	// Parse deps: dep:name:constraint,dep:name:constraint,...
	for _, part := range strings.Split(strings.TrimSpace(rest), ",") {
		part = strings.TrimSpace(part)
		if !strings.HasPrefix(part, "dep:") {
			continue
		}
		part = part[4:]
		colonIdx := strings.Index(part, ":")
		if colonIdx < 0 {
			continue
		}
		v.Dependencies = append(v.Dependencies, GemDep{
			Name:       part[:colonIdx],
			Constraint: part[colonIdx+1:],
		})
	}
	return v, nil
}

// platformSplit returns the index of the first '-' that begins a platform suffix,
// or -1 if the string has no platform suffix.
// In RubyGems, '-' is only used for platform suffixes (e.g. "x86_64-linux");
// pre-release segments use dots, not dashes. Walking forward keeps the full
// platform string like "x86_64-linux" intact rather than splitting on the last dash.
func platformSplit(vp string) int {
	for i := 0; i < len(vp); i++ {
		if vp[i] == '-' {
			rest := vp[i+1:]
			if len(rest) > 0 && rest[0] >= 'a' && rest[0] <= 'z' {
				return i
			}
		}
	}
	return -1
}

// SelectBestVersion picks the highest-versioned GemVersion from versions that
// satisfies req. If platform is non-empty, only versions matching that platform
// (or the universal platform "") are considered; when no platform-specific
// version exists, universal versions are returned as fallback. Returns
// ErrGemVersionNotFound if no version satisfies req.
func SelectBestVersion(versions []GemVersion, req semver.Req, platform string) (*GemVersion, error) {
	var candidates []GemVersion
	for _, v := range versions {
		if platform != "" && v.Platform != platform && v.Platform != "" {
			continue
		}
		parsed, err := semver.ParseVersion(v.Version)
		if err != nil {
			continue
		}
		if semver.Satisfies(parsed, req) {
			candidates = append(candidates, v)
		}
	}
	// Prefer platform-specific over universal.
	if platform != "" {
		var platSpecific []GemVersion
		for _, v := range candidates {
			if v.Platform == platform {
				platSpecific = append(platSpecific, v)
			}
		}
		if len(platSpecific) > 0 {
			candidates = platSpecific
		}
	}
	if len(candidates) == 0 {
		return nil, fmt.Errorf("%w: no version satisfying %v", ErrGemVersionNotFound, req)
	}
	best := &candidates[0]
	for i := 1; i < len(candidates); i++ {
		a, _ := semver.ParseVersion(best.Version)
		b, _ := semver.ParseVersion(candidates[i].Version)
		if semver.CompareVersions(b, a) > 0 {
			best = &candidates[i]
		}
	}
	return best, nil
}
