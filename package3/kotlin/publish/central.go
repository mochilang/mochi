// Package publish — Sonatype Central Portal client for Maven Central uploads.
//
// The Sonatype Central Portal API (GA since 2024-03) accepts a deployment
// bundle as a ZIP archive uploaded to:
//
//	POST https://central.sonatype.com/api/v1/publisher/upload
//
// The bundle must contain, for each artifact:
//
//	<group-path>/<artifact>/<version>/<artifact>-<version>.jar
//	<group-path>/<artifact>/<version>/<artifact>-<version>.pom
//	<group-path>/<artifact>/<version>/<artifact>-<version>.jar.asc   (GPG)
//	<group-path>/<artifact>/<version>/<artifact>-<version>.pom.asc   (GPG)
//	<group-path>/<artifact>/<version>/<artifact>-<version>.jar.sha1
//	<group-path>/<artifact>/<version>/<artifact>-<version>.pom.sha1
//	<group-path>/<artifact>/<version>/<artifact>-<version>.jar.md5
//	<group-path>/<artifact>/<version>/<artifact>-<version>.pom.md5
//
// The upload returns a deploymentId; the caller polls
// GET /api/v1/publisher/status?id=<deploymentId> until state is PUBLISHED.
//
// This file implements:
//   - BuildBundle: assemble the ZIP from JAR + POM paths
//   - Client: upload + poll the Central Portal
//   - DryRun: validate the bundle without uploading
package publish

import (
	"archive/zip"
	"bytes"
	"crypto/md5"
	"crypto/sha1"
	"encoding/hex"
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

// BundleSpec describes the artifact files to bundle for upload.
type BundleSpec struct {
	// GroupID is the Maven groupId (e.g. "com.example").
	GroupID string
	// ArtifactID is the Maven artifactId (e.g. "my-lib").
	ArtifactID string
	// Version is the release version (e.g. "1.0.0").
	Version string
	// JARPath is the absolute path to the compiled JAR.
	JARPath string
	// POMPath is the absolute path to the POM XML.
	POMPath string
	// SourcesJARPath is the optional -sources.jar (empty = skip).
	SourcesJARPath string
	// GPGKeyID is the key fingerprint to sign with (requires gpg on PATH).
	// If empty, signing is skipped (bundle will be rejected by Maven Central
	// unless the portal is in OIDC trusted-publishing mode).
	GPGKeyID string
}

// BundleResult is the path to the produced ZIP bundle.
type BundleResult struct {
	BundlePath string // absolute path to the upload ZIP
}

// BuildBundle assembles the Sonatype Central Portal upload ZIP for one artifact.
// It includes the JAR, POM, their SHA-1/MD5 checksums, and GPG .asc signatures
// when GPGKeyID is set.
func BuildBundle(spec BundleSpec, outDir string) (*BundleResult, error) {
	if err := validateBundleSpec(spec); err != nil {
		return nil, err
	}
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return nil, fmt.Errorf("bundle: mkdir: %w", err)
	}

	bundleName := fmt.Sprintf("%s-%s-bundle.zip", spec.ArtifactID, spec.Version)
	bundlePath := filepath.Join(outDir, bundleName)

	f, err := os.Create(bundlePath)
	if err != nil {
		return nil, fmt.Errorf("bundle: create zip: %w", err)
	}
	defer f.Close()

	zw := zip.NewWriter(f)

	groupPath := strings.ReplaceAll(spec.GroupID, ".", "/")
	prefix := fmt.Sprintf("%s/%s/%s", groupPath, spec.ArtifactID, spec.Version)

	base := fmt.Sprintf("%s-%s", spec.ArtifactID, spec.Version)

	type entry struct {
		src  string
		name string
	}
	entries := []entry{
		{spec.JARPath, base + ".jar"},
		{spec.POMPath, base + ".pom"},
	}
	if spec.SourcesJARPath != "" {
		if _, err := os.Stat(spec.SourcesJARPath); err == nil {
			entries = append(entries, entry{spec.SourcesJARPath, base + "-sources.jar"})
		}
	}

	for _, e := range entries {
		data, err := os.ReadFile(e.src)
		if err != nil {
			return nil, fmt.Errorf("bundle: read %s: %w", e.src, err)
		}
		// primary file
		if err := addToZip(zw, prefix+"/"+e.name, data); err != nil {
			return nil, err
		}
		// SHA-1
		sha1sum := sha1hex(data)
		if err := addToZip(zw, prefix+"/"+e.name+".sha1", []byte(sha1sum)); err != nil {
			return nil, err
		}
		// MD5
		md5sum := md5hex(data)
		if err := addToZip(zw, prefix+"/"+e.name+".md5", []byte(md5sum)); err != nil {
			return nil, err
		}
		// GPG signature
		if spec.GPGKeyID != "" {
			asc, err := gpgSign(data, spec.GPGKeyID)
			if err != nil {
				return nil, fmt.Errorf("bundle: gpg sign %s: %w", e.name, err)
			}
			if err := addToZip(zw, prefix+"/"+e.name+".asc", asc); err != nil {
				return nil, err
			}
		}
	}

	if err := zw.Close(); err != nil {
		return nil, fmt.Errorf("bundle: close zip: %w", err)
	}
	return &BundleResult{BundlePath: bundlePath}, nil
}

// DryRun validates a bundle ZIP without uploading: checks it can be opened,
// all required entries are present, and checksums match.
func DryRun(bundlePath, groupID, artifactID, version string) error {
	r, err := zip.OpenReader(bundlePath)
	if err != nil {
		return fmt.Errorf("dry-run: open zip: %w", err)
	}
	defer r.Close()

	groupPath := strings.ReplaceAll(groupID, ".", "/")
	prefix := fmt.Sprintf("%s/%s/%s", groupPath, artifactID, version)
	base := fmt.Sprintf("%s-%s", artifactID, version)

	required := []string{
		prefix + "/" + base + ".jar",
		prefix + "/" + base + ".pom",
		prefix + "/" + base + ".jar.sha1",
		prefix + "/" + base + ".pom.sha1",
	}

	index := map[string]*zip.File{}
	for _, f := range r.File {
		index[f.Name] = f
	}

	for _, name := range required {
		if _, ok := index[name]; !ok {
			return fmt.Errorf("dry-run: missing required entry %q", name)
		}
	}

	// Verify SHA-1 checksums.
	for _, suffix := range []string{".jar", ".pom"} {
		dataFile := index[prefix+"/"+base+suffix]
		sha1File := index[prefix+"/"+base+suffix+".sha1"]
		if dataFile == nil || sha1File == nil {
			continue
		}
		data, err := readZipEntry(dataFile)
		if err != nil {
			return fmt.Errorf("dry-run: read %s: %w", dataFile.Name, err)
		}
		sha1Stored, err := readZipEntry(sha1File)
		if err != nil {
			return fmt.Errorf("dry-run: read %s: %w", sha1File.Name, err)
		}
		if sha1hex(data) != strings.TrimSpace(string(sha1Stored)) {
			return fmt.Errorf("dry-run: SHA-1 mismatch for %s%s", base, suffix)
		}
	}
	return nil
}

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
// Set dryRun=true to call DryRun validation only (no HTTP request).
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

	var deploymentID string
	b, _ := io.ReadAll(resp.Body)
	// The response is just the deployment UUID as a plain string.
	deploymentID = strings.TrimSpace(string(b))
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

// ─── helpers ──────────────────────────────────────────────────────────────────

func addToZip(zw *zip.Writer, name string, data []byte) error {
	w, err := zw.Create(name)
	if err != nil {
		return fmt.Errorf("zip add %s: %w", name, err)
	}
	_, err = w.Write(data)
	return err
}

func readZipEntry(f *zip.File) ([]byte, error) {
	rc, err := f.Open()
	if err != nil {
		return nil, err
	}
	defer rc.Close()
	return io.ReadAll(rc)
}

func sha1hex(data []byte) string {
	h := sha1.Sum(data)
	return hex.EncodeToString(h[:])
}

func md5hex(data []byte) string {
	h := md5.Sum(data)
	return hex.EncodeToString(h[:])
}

// gpgSign runs `gpg --batch --yes --armor --detach-sign --local-user <keyID>`
// and returns the ASCII-armored signature.
func gpgSign(data []byte, keyID string) ([]byte, error) {
	import_cmd := []string{
		"--batch", "--yes", "--armor", "--detach-sign",
		"--local-user", keyID,
		"--output", "-",
		"-",
	}
	cmd := execGPG(import_cmd...)
	cmd.Stdin = bytes.NewReader(data)
	out, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("gpg sign: %w", err)
	}
	return out, nil
}

func validateBundleSpec(spec BundleSpec) error {
	if spec.GroupID == "" {
		return fmt.Errorf("bundle: GroupID is required")
	}
	if spec.ArtifactID == "" {
		return fmt.Errorf("bundle: ArtifactID is required")
	}
	if spec.Version == "" {
		return fmt.Errorf("bundle: Version is required")
	}
	if spec.JARPath == "" {
		return fmt.Errorf("bundle: JARPath is required")
	}
	if spec.POMPath == "" {
		return fmt.Errorf("bundle: POMPath is required")
	}
	return nil
}
