// Package index implements a client for the RubyGems compact index protocol
// used by Bundler. The compact index serves gem metadata (versions, checksums,
// dependencies) from https://index.rubygems.org/ without requiring the
// full rubygems.org REST API.
//
// Phase 1 of MEP-76 implements this package.
// See [website/docs/research/0076/11-version-resolution.md] for the format.
package index

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io"
	"net/http"
	"strings"
)

const defaultIndexBase = "https://index.rubygems.org"

// Client fetches gem metadata from the RubyGems compact index.
type Client struct {
	BaseURL    string
	HTTPClient *http.Client
}

// NewClient returns a Client pointing at the official RubyGems compact index.
func NewClient() *Client {
	return &Client{
		BaseURL:    defaultIndexBase,
		HTTPClient: http.DefaultClient,
	}
}

// GemVersion is one version entry from the compact index /info/<gem> endpoint.
type GemVersion struct {
	Version      string
	Platform     string // "" for ruby-universal, "x86_64-linux", "arm64-darwin", etc.
	SHA256       string // hex-encoded SHA-256 of the .gem tarball
	Dependencies []GemDep
}

// GemDep is one runtime dependency declared by a gem version.
type GemDep struct {
	Name       string
	Constraint string // e.g. ">= 1.0", "~> 2.1"
}

// FetchVersions fetches all versions of a gem from the compact index
// /info/<gem> endpoint and returns them newest-first.
func (c *Client) FetchVersions(gem string) ([]GemVersion, error) {
	url := fmt.Sprintf("%s/info/%s", c.BaseURL, gem)
	resp, err := c.HTTPClient.Get(url)
	if err != nil {
		return nil, fmt.Errorf("compact index fetch %s: %w", url, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode == http.StatusNotFound {
		return nil, fmt.Errorf("gem %q not found on index", gem)
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

// VerifyGem downloads a .gem tarball and asserts its SHA-256 matches want.
// Returns the tarball bytes on success.
func (c *Client) VerifyGem(gem, version, platform, wantSHA256 string) ([]byte, error) {
	gemFile := gem + "-" + version
	if platform != "" {
		gemFile += "-" + platform
	}
	gemFile += ".gem"
	url := fmt.Sprintf("https://rubygems.org/gems/%s", gemFile)
	resp, err := c.HTTPClient.Get(url)
	if err != nil {
		return nil, fmt.Errorf("download %s: %w", url, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("download %s: HTTP %d", url, resp.StatusCode)
	}
	data, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}
	sum := sha256.Sum256(data)
	got := hex.EncodeToString(sum[:])
	if got != wantSHA256 {
		return nil, fmt.Errorf("gem %s@%s SHA-256 mismatch: got %s want %s", gem, version, got, wantSHA256)
	}
	return data, nil
}

// parseInfoBody parses the /info/<gem> compact index response body.
// Format per line: <version>[-<platform>] |checksum:<sha256>|dep:<name>:<constraint>,...
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
	// Format: <ver-platform> <deps> |<checksum>
	// Example: 1.16.2-x86_64-linux  nokogiri:>= 0|checksum:abc123
	parts := strings.SplitN(line, " ", 2)
	vp := parts[0]
	var v GemVersion
	// Split version from platform: last segment after - that starts with a letter
	// is the platform (x86_64-linux, arm64-darwin, etc.).
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
		end := strings.Index(rest[i+10:], "|")
		if end < 0 {
			v.SHA256 = rest[i+10:]
		} else {
			v.SHA256 = rest[i+10 : i+10+end]
		}
	}
	return v, nil
}

// platformSplit returns the index of the '-' separator between version and
// platform, or -1 if the string has no platform suffix.
func platformSplit(vp string) int {
	// A platform suffix starts with a letter; a prerelease suffix starts with
	// a digit or dot. Walk backwards to find the split point.
	for i := len(vp) - 1; i >= 0; i-- {
		if vp[i] == '-' {
			rest := vp[i+1:]
			if len(rest) > 0 && (rest[0] >= 'a' && rest[0] <= 'z') {
				return i
			}
		}
	}
	return -1
}
