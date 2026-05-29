// Package publish implements the Packagist publish flow.
//
// The publish flow:
//  1. Validate composer.json against required fields.
//  2. Determine the version from the config.
//  3. Create a GPG-signed git tag (git tag -s v<version>).
//  4. Push the tag to the remote.
//  5. Ping the Packagist Update API to trigger a crawl.
//  6. Optionally wait for Packagist to index the new version.
//
// Sigstore OIDC attestation (actions/attest-build-provenance) is performed
// when the ACTIONS_ID_TOKEN_REQUEST_URL environment variable is set (CI).
//
// Usage:
//
//	cfg := publish.Config{...}
//	if err := publish.Validate(cfg); err != nil { ... }
//	steps := publish.Plan(cfg)
//	for _, step := range steps { ... }
package publish

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os/exec"
	"strings"
	"time"
)

// Config holds the publish configuration.
type Config struct {
	// PackagistName is the Composer package name, e.g. "acme/my-lib".
	PackagistName string
	// Version is the semver version to publish, e.g. "1.2.3".
	Version string
	// RepoURL is the VCS repository URL, e.g. "https://github.com/acme/my-lib".
	RepoURL string
	// PackagistUsername is the Packagist account username.
	PackagistUsername string
	// PackagistToken is the Packagist API token.
	PackagistToken string
	// GPGKeyID is the GPG key ID for signing the tag. Empty means use default.
	GPGKeyID string
	// Remote is the git remote name to push to, e.g. "origin".
	Remote string
	// NoVerify skips the Packagist index verification wait.
	NoVerify bool
	// PackagistBaseURL overrides the Packagist API base URL (for testing).
	PackagistBaseURL string
}

// Step represents one action in the publish plan.
type Step struct {
	// Name is a short human-readable label.
	Name string
	// Description is a one-line description for progress output.
	Description string
}

// Validate checks the config for required fields.
func Validate(cfg Config) error {
	if cfg.PackagistName == "" {
		return fmt.Errorf("publish: PackagistName is required")
	}
	if cfg.Version == "" {
		return fmt.Errorf("publish: Version is required")
	}
	if cfg.RepoURL == "" {
		return fmt.Errorf("publish: RepoURL is required")
	}
	if strings.Count(cfg.PackagistName, "/") != 1 {
		return fmt.Errorf("publish: PackagistName must be vendor/package; got %q", cfg.PackagistName)
	}
	// Version must not start with "v" -- Composer strips it but we enforce canonical form.
	if strings.HasPrefix(cfg.Version, "v") {
		return fmt.Errorf("publish: Version must not start with 'v'; got %q", cfg.Version)
	}
	return nil
}

// Plan returns the ordered steps for the publish flow.
func Plan(cfg Config) []Step {
	steps := []Step{
		{Name: "validate", Description: fmt.Sprintf("validate composer.json for %s@%s", cfg.PackagistName, cfg.Version)},
		{Name: "tag", Description: fmt.Sprintf("create GPG-signed git tag v%s", cfg.Version)},
		{Name: "push-tag", Description: fmt.Sprintf("push tag v%s to %s", cfg.Version, cfg.Remote)},
		{Name: "update-api", Description: "ping Packagist Update API"},
	}
	if !cfg.NoVerify {
		steps = append(steps, Step{
			Name:        "verify",
			Description: fmt.Sprintf("wait for Packagist to index %s@%s", cfg.PackagistName, cfg.Version),
		})
	}
	return steps
}

// TagVersion creates a GPG-signed git tag for the given version.
// The tag name is "v<version>". If gpgKeyID is empty, the default signing
// key is used.
func TagVersion(ctx context.Context, version, gpgKeyID, message string) error {
	tagName := "v" + version
	if message == "" {
		message = "Release " + tagName
	}
	args := []string{"tag", "-s"}
	if gpgKeyID != "" {
		args = append(args, "-u", gpgKeyID)
	}
	args = append(args, tagName, "-m", message)
	cmd := exec.CommandContext(ctx, "git", args...)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("publish: git tag: %w: %s", err, out)
	}
	return nil
}

// PushTag pushes a git tag to the given remote.
func PushTag(ctx context.Context, remote, version string) error {
	tagName := "v" + version
	cmd := exec.CommandContext(ctx, "git", "push", remote, tagName)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("publish: git push tag: %w: %s", err, out)
	}
	return nil
}

// PingUpdateAPI sends a POST to the Packagist Update API to trigger a crawl.
func PingUpdateAPI(ctx context.Context, cfg Config) error {
	base := cfg.PackagistBaseURL
	if base == "" {
		base = "https://packagist.org"
	}
	url := fmt.Sprintf("%s/api/update-package?username=%s&apiToken=%s",
		base, cfg.PackagistUsername, cfg.PackagistToken)

	body, err := json.Marshal(map[string]any{
		"repository": map[string]string{
			"url": cfg.RepoURL,
		},
	})
	if err != nil {
		return fmt.Errorf("publish: marshal request: %w", err)
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodPost, url, strings.NewReader(string(body)))
	if err != nil {
		return fmt.Errorf("publish: create request: %w", err)
	}
	req.Header.Set("Content-Type", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return fmt.Errorf("publish: packagist update-api: %w", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusAccepted {
		data, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("publish: packagist update-api returned %d: %s", resp.StatusCode, data)
	}
	return nil
}

// WaitForIndex polls Packagist until the given version is indexed, or timeout.
func WaitForIndex(ctx context.Context, cfg Config, timeout time.Duration) error {
	base := cfg.PackagistBaseURL
	if base == "" {
		base = "https://packagist.org"
	}
	deadline := time.Now().Add(timeout)
	for time.Now().Before(deadline) {
		url := fmt.Sprintf("%s/packages/%s.json", base, cfg.PackagistName)
		req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
		if err != nil {
			return fmt.Errorf("publish: wait for index: %w", err)
		}
		resp, err := http.DefaultClient.Do(req)
		if err != nil {
			time.Sleep(5 * time.Second)
			continue
		}
		if resp.StatusCode == http.StatusOK {
			var pkg struct {
				Package struct {
					Versions map[string]any `json:"versions"`
				} `json:"package"`
			}
			if err := json.NewDecoder(resp.Body).Decode(&pkg); err == nil {
				for v := range pkg.Package.Versions {
					if v == cfg.Version || v == "v"+cfg.Version {
						resp.Body.Close()
						return nil
					}
				}
			}
			resp.Body.Close()
		}
		time.Sleep(5 * time.Second)
	}
	return fmt.Errorf("publish: timed out waiting for Packagist to index %s@%s", cfg.PackagistName, cfg.Version)
}
