// Package publish implements the two Swift package publish paths:
// git-tag publish (standard SPM) and SE-0292 registry publish.
package publish

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"mime/multipart"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// Registry describes an SE-0292 registry endpoint.
type Registry struct {
	BaseURL   string
	AuthToken string
	HTTP      *http.Client
}

// PublishGitTag creates a git tag v<version> in repoDir and pushes it to the remote.
// This is the standard SPM publish path: the package registry is the git remote itself.
func PublishGitTag(ctx context.Context, repoDir, version string) error {
	if repoDir == "" {
		return fmt.Errorf("publish: repoDir is empty")
	}
	if version == "" {
		return fmt.Errorf("publish: version is empty")
	}
	tag := "v" + strings.TrimPrefix(version, "v")

	// Create annotated tag.
	tagCmd := exec.CommandContext(ctx, "git", "tag", "-a", tag, "-m", "Release "+tag)
	tagCmd.Dir = repoDir
	if out, err := tagCmd.CombinedOutput(); err != nil {
		return fmt.Errorf("publish: git tag %s: %w\n%s", tag, err, out)
	}

	// Push tag to remote.
	pushCmd := exec.CommandContext(ctx, "git", "push", "origin", tag)
	pushCmd.Dir = repoDir
	if out, err := pushCmd.CombinedOutput(); err != nil {
		return fmt.Errorf("publish: git push %s: %w\n%s", tag, err, out)
	}
	return nil
}

// PublishSE0292 uploads a source archive to an SE-0292 registry.
// Calls PUT /registry/<scope>/<name>/<version> with the .zip archive.
// Authenticates with a Bearer token (GitHub Actions OIDC or static API key).
func PublishSE0292(ctx context.Context, registry Registry, id, version, archivePath string) error {
	if registry.BaseURL == "" {
		return fmt.Errorf("publish: registry BaseURL is empty")
	}
	if id == "" || version == "" || archivePath == "" {
		return fmt.Errorf("publish: id, version, and archivePath are required")
	}

	// SE-0292: id is "scope.package-name"
	parts := strings.SplitN(id, ".", 2)
	if len(parts) != 2 {
		return fmt.Errorf("publish: id %q must be in format scope.package-name", id)
	}
	scope, name := parts[0], parts[1]

	f, err := os.Open(archivePath)
	if err != nil {
		return fmt.Errorf("publish: open archive %s: %w", archivePath, err)
	}
	defer f.Close()

	var body bytes.Buffer
	mw := multipart.NewWriter(&body)
	part, err := mw.CreateFormFile("source-archive", filepath.Base(archivePath))
	if err != nil {
		return fmt.Errorf("publish: create form file: %w", err)
	}
	if _, err := io.Copy(part, f); err != nil {
		return fmt.Errorf("publish: copy archive: %w", err)
	}
	if err := mw.Close(); err != nil {
		return fmt.Errorf("publish: close multipart: %w", err)
	}

	url := fmt.Sprintf("%s/registry/%s/%s/%s", strings.TrimRight(registry.BaseURL, "/"), scope, name, version)
	req, err := http.NewRequestWithContext(ctx, http.MethodPut, url, &body)
	if err != nil {
		return fmt.Errorf("publish: build request: %w", err)
	}
	req.Header.Set("Content-Type", mw.FormDataContentType())
	if registry.AuthToken != "" {
		req.Header.Set("Authorization", "Bearer "+registry.AuthToken)
	}

	client := registry.HTTP
	if client == nil {
		client = http.DefaultClient
	}
	resp, err := client.Do(req)
	if err != nil {
		return fmt.Errorf("publish: PUT %s: %w", url, err)
	}
	defer resp.Body.Close()
	respBody, _ := io.ReadAll(resp.Body)
	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		return fmt.Errorf("publish: registry returned %d: %s", resp.StatusCode, string(respBody))
	}
	return nil
}

// OIDCMintToken retrieves a GitHub Actions OIDC JWT for Swift registry authentication.
// Reads ACTIONS_ID_TOKEN_REQUEST_URL and ACTIONS_ID_TOKEN_REQUEST_TOKEN from env.
func OIDCMintToken(ctx context.Context, audience string) (string, error) {
	tokenURL := os.Getenv("ACTIONS_ID_TOKEN_REQUEST_URL")
	if tokenURL == "" {
		return "", fmt.Errorf("publish: ACTIONS_ID_TOKEN_REQUEST_URL not set")
	}
	requestToken := os.Getenv("ACTIONS_ID_TOKEN_REQUEST_TOKEN")
	if audience != "" {
		sep := "?"
		if strings.Contains(tokenURL, "?") {
			sep = "&"
		}
		tokenURL += sep + "audience=" + audience
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodGet, tokenURL, nil)
	if err != nil {
		return "", fmt.Errorf("publish: build OIDC request: %w", err)
	}
	if requestToken != "" {
		req.Header.Set("Authorization", "Bearer "+requestToken)
	}
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return "", fmt.Errorf("publish: fetch OIDC token: %w", err)
	}
	defer resp.Body.Close()
	body, _ := io.ReadAll(resp.Body)
	if resp.StatusCode != http.StatusOK {
		return "", fmt.Errorf("publish: OIDC endpoint returned %d", resp.StatusCode)
	}
	var tok struct {
		Value string `json:"value"`
	}
	if err := json.Unmarshal(body, &tok); err != nil {
		return "", fmt.Errorf("publish: decode OIDC response: %w", err)
	}
	return tok.Value, nil
}
