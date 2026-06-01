// Package se0292 implements an HTTP client for Swift SE-0292 package
// registries. The SE-0292 specification defines a REST API for Swift package
// registries with endpoints for listing versions, downloading source archives,
// and fetching package manifests.
package se0292

import (
	"context"
	"fmt"
	"io"
	"net/http"
	"strings"
)

// Registry is an SE-0292 compliant Swift package registry client.
type Registry struct {
	// BaseURL is the registry base URL, e.g. "https://packages.example.com".
	BaseURL string
	// HTTP is the HTTP client to use. If nil, http.DefaultClient is used.
	HTTP *http.Client
	// UserAgent is the User-Agent header value. If empty, a default is used.
	UserAgent string
}

func (r *Registry) httpClient() *http.Client {
	if r.HTTP != nil {
		return r.HTTP
	}
	return http.DefaultClient
}

func (r *Registry) userAgent() string {
	if r.UserAgent != "" {
		return r.UserAgent
	}
	return "mochi-swift-bridge/1.0"
}

// ListVersions returns the available versions for the package identified by id.
// id must be in the form "scope.package-name" per SE-0292 §4.1.
func (r *Registry) ListVersions(ctx context.Context, id string) ([]string, error) {
	scope, name, err := splitID(id)
	if err != nil {
		return nil, fmt.Errorf("se0292: %w", err)
	}
	url := fmt.Sprintf("%s/%s/%s", strings.TrimRight(r.BaseURL, "/"), scope, name)

	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return nil, fmt.Errorf("se0292: build request: %w", err)
	}
	req.Header.Set("Accept", "application/vnd.swift.registry.v1+json")
	req.Header.Set("User-Agent", r.userAgent())

	resp, err := r.httpClient().Do(req)
	if err != nil {
		return nil, fmt.Errorf("se0292: list versions for %s: %w", id, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("se0292: list versions returned HTTP %d for %s", resp.StatusCode, id)
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("se0292: read list response: %w", err)
	}

	return parseVersionList(body)
}

// DownloadSourceArchive downloads the source archive for the given package id
// and version. Returns an io.ReadCloser the caller must close.
func (r *Registry) DownloadSourceArchive(ctx context.Context, id, version string) (io.ReadCloser, error) {
	scope, name, err := splitID(id)
	if err != nil {
		return nil, fmt.Errorf("se0292: %w", err)
	}
	url := fmt.Sprintf("%s/%s/%s/%s.zip",
		strings.TrimRight(r.BaseURL, "/"), scope, name, version)

	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return nil, fmt.Errorf("se0292: build request: %w", err)
	}
	req.Header.Set("Accept", "application/zip")
	req.Header.Set("User-Agent", r.userAgent())

	resp, err := r.httpClient().Do(req)
	if err != nil {
		return nil, fmt.Errorf("se0292: download %s@%s: %w", id, version, err)
	}

	if resp.StatusCode != http.StatusOK {
		resp.Body.Close()
		return nil, fmt.Errorf("se0292: download returned HTTP %d for %s@%s", resp.StatusCode, id, version)
	}

	return resp.Body, nil
}

// GetManifest retrieves the Package.swift manifest for the given package id
// and version. Returns the raw manifest bytes.
func (r *Registry) GetManifest(ctx context.Context, id, version string) ([]byte, error) {
	scope, name, err := splitID(id)
	if err != nil {
		return nil, fmt.Errorf("se0292: %w", err)
	}
	url := fmt.Sprintf("%s/%s/%s/%s/Package.swift",
		strings.TrimRight(r.BaseURL, "/"), scope, name, version)

	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return nil, fmt.Errorf("se0292: build request: %w", err)
	}
	req.Header.Set("Accept", "text/x-swift")
	req.Header.Set("User-Agent", r.userAgent())

	resp, err := r.httpClient().Do(req)
	if err != nil {
		return nil, fmt.Errorf("se0292: get manifest %s@%s: %w", id, version, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("se0292: manifest returned HTTP %d for %s@%s", resp.StatusCode, id, version)
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("se0292: read manifest: %w", err)
	}
	return body, nil
}

// splitID splits a SE-0292 package identifier "scope.package-name" into
// (scope, name). Returns an error if the id is malformed.
func splitID(id string) (scope, name string, err error) {
	parts := strings.SplitN(id, ".", 2)
	if len(parts) != 2 || parts[0] == "" || parts[1] == "" {
		return "", "", fmt.Errorf("invalid SE-0292 package id %q: must be scope.name", id)
	}
	return parts[0], parts[1], nil
}

// parseVersionList extracts version strings from an SE-0292 registry JSON
// response. The registry returns JSON with a "releases" object.
// Example: {"releases": {"1.0.0": {...}, "1.1.0": {...}}}
func parseVersionList(data []byte) ([]string, error) {
	// Simple JSON extraction without a full decode to avoid dependencies.
	s := string(data)
	relIdx := strings.Index(s, `"releases"`)
	if relIdx < 0 {
		return nil, fmt.Errorf("se0292: no releases key in response")
	}
	// Find the opening brace of the releases object
	braceIdx := strings.Index(s[relIdx:], "{")
	if braceIdx < 0 {
		return nil, fmt.Errorf("se0292: malformed releases value")
	}
	start := relIdx + braceIdx + 1
	// Find matching closing brace
	depth := 1
	end := -1
	for i := start; i < len(s); i++ {
		switch s[i] {
		case '{':
			depth++
		case '}':
			depth--
			if depth == 0 {
				end = i
			}
		}
		if end >= 0 {
			break
		}
	}
	if end < 0 {
		return nil, fmt.Errorf("se0292: unterminated releases object")
	}

	inner := s[start:end]
	var versions []string
	for {
		q1 := strings.Index(inner, `"`)
		if q1 < 0 {
			break
		}
		q2 := strings.Index(inner[q1+1:], `"`)
		if q2 < 0 {
			break
		}
		ver := inner[q1+1 : q1+1+q2]
		if ver != "" {
			versions = append(versions, ver)
		}
		inner = inner[q1+1+q2+1:]
	}
	return versions, nil
}
