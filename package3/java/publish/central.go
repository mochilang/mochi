// Package publish implements the Sonatype Central Portal upload flow for
// MEP-67 Java bridge artifacts.
//
// The Central Portal API (GA since 2024-03) accepts a deployment bundle as
// a ZIP archive uploaded to:
//
//	POST https://central.sonatype.com/api/v1/publisher/upload
//
// This file implements Client (upload + status polling) and DryRun.
// The bundle ZIP is built by BuildBundle in bundle.go.
package publish

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"mime/multipart"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"time"
)

// Client uploads a bundle to the Sonatype Central Portal and polls for completion.
type Client struct {
	// BaseURL is the Central Portal API base (default: https://central.sonatype.com).
	BaseURL string
	// Token is the bearer token (user token from the portal, or OIDC token for
	// trusted publishing).
	Token string
	// HTTPClient is used for all requests (default: http.DefaultClient).
	HTTPClient *http.Client
	// PollInterval controls how often to check deployment status (default: 5s).
	PollInterval time.Duration
	// PollTimeout is the maximum time to wait for PUBLISHED state (default: 10min).
	PollTimeout time.Duration
}

// DeploymentState is the string status returned by the Central Portal.
type DeploymentState string

const (
	DeploymentPending   DeploymentState = "PENDING"
	DeploymentValidated DeploymentState = "VALIDATED"
	DeploymentPublished DeploymentState = "PUBLISHED"
	DeploymentFailed    DeploymentState = "FAILED"
)

// Upload sends the bundle ZIP to the Central Portal and returns a deployment ID.
// Pass dryRun=true to skip the HTTP request (returns a placeholder ID).
func (c *Client) Upload(bundlePath string, dryRun bool, bundleName string) (string, error) {
	if dryRun {
		return "dry-run-deployment-id", nil
	}
	base := c.baseURL()

	f, err := os.Open(bundlePath)
	if err != nil {
		return "", fmt.Errorf("upload: open bundle: %w", err)
	}
	defer f.Close()

	var body bytes.Buffer
	mw := multipart.NewWriter(&body)
	fw, err := mw.CreateFormFile("bundle", filepath.Base(bundlePath))
	if err != nil {
		return "", fmt.Errorf("upload: multipart: %w", err)
	}
	if _, err := io.Copy(fw, f); err != nil {
		return "", fmt.Errorf("upload: copy bundle: %w", err)
	}
	if err := mw.Close(); err != nil {
		return "", fmt.Errorf("upload: close multipart: %w", err)
	}

	url := base + "/api/v1/publisher/upload"
	if bundleName != "" {
		url += "?name=" + bundleName
	}
	req, err := http.NewRequest(http.MethodPost, url, &body)
	if err != nil {
		return "", fmt.Errorf("upload: create request: %w", err)
	}
	req.Header.Set("Authorization", "Bearer "+c.Token)
	req.Header.Set("Content-Type", mw.FormDataContentType())

	resp, err := c.httpClient().Do(req)
	if err != nil {
		return "", fmt.Errorf("upload: POST: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusCreated && resp.StatusCode != http.StatusOK {
		b, _ := io.ReadAll(resp.Body)
		return "", fmt.Errorf("upload: HTTP %d: %s", resp.StatusCode, string(b))
	}

	b, _ := io.ReadAll(resp.Body)
	deploymentID := strings.TrimSpace(string(b))
	if deploymentID == "" {
		return "", fmt.Errorf("upload: empty deployment ID in response")
	}
	return deploymentID, nil
}

// PollUntilPublished polls the deployment status until PUBLISHED or FAILED.
func (c *Client) PollUntilPublished(deploymentID string) (DeploymentState, error) {
	interval := c.PollInterval
	if interval == 0 {
		interval = 5 * time.Second
	}
	timeout := c.PollTimeout
	if timeout == 0 {
		timeout = 10 * time.Minute
	}

	deadline := time.Now().Add(timeout)
	for time.Now().Before(deadline) {
		state, err := c.checkStatus(deploymentID)
		if err != nil {
			return "", err
		}
		switch state {
		case DeploymentPublished:
			return DeploymentPublished, nil
		case DeploymentFailed:
			return DeploymentFailed, fmt.Errorf("deployment %s failed", deploymentID)
		}
		time.Sleep(interval)
	}
	return "", fmt.Errorf("deployment %s timed out after %s", deploymentID, timeout)
}

type statusResponse struct {
	DeploymentState DeploymentState `json:"deploymentState"`
}

func (c *Client) checkStatus(deploymentID string) (DeploymentState, error) {
	url := c.baseURL() + "/api/v1/publisher/status?id=" + deploymentID
	req, err := http.NewRequest(http.MethodGet, url, nil)
	if err != nil {
		return "", err
	}
	req.Header.Set("Authorization", "Bearer "+c.Token)

	resp, err := c.httpClient().Do(req)
	if err != nil {
		return "", fmt.Errorf("status poll: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		b, _ := io.ReadAll(resp.Body)
		return "", fmt.Errorf("status poll: HTTP %d: %s", resp.StatusCode, string(b))
	}

	var sr statusResponse
	if err := json.NewDecoder(resp.Body).Decode(&sr); err != nil {
		return "", fmt.Errorf("status poll: decode: %w", err)
	}
	return sr.DeploymentState, nil
}

func (c *Client) baseURL() string {
	if c.BaseURL != "" {
		return strings.TrimRight(c.BaseURL, "/")
	}
	return "https://central.sonatype.com"
}

func (c *Client) httpClient() *http.Client {
	if c.HTTPClient != nil {
		return c.HTTPClient
	}
	return http.DefaultClient
}
