// Package publish implements npm and JSR trusted publish flows.
// Lands in MEP-72 Phase 12.
package publish

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
)

// NpmPublishSpec describes an npm publish operation.
type NpmPublishSpec struct {
	PackageDir string
	Version    string
	NpmBin     string
	Token      string // NODE_AUTH_TOKEN; reads from env if empty
	Provenance bool   // --provenance flag
}

// JsrPublishSpec describes a JSR publish operation.
type JsrPublishSpec struct {
	PackageDir string
	DenoBin    string
}

// PublishNpm packs and publishes a package to npm.
func PublishNpm(ctx context.Context, spec NpmPublishSpec) error {
	npmBin := spec.NpmBin
	if npmBin == "" {
		npmBin = "npm"
	}
	token := spec.Token
	if token == "" {
		token = os.Getenv("NODE_AUTH_TOKEN")
	}
	if token == "" {
		return fmt.Errorf("publish: NODE_AUTH_TOKEN not set")
	}

	// npm pack
	packCmd := exec.CommandContext(ctx, npmBin, "pack", "--json")
	packCmd.Dir = spec.PackageDir
	packOut, err := packCmd.Output()
	if err != nil {
		return fmt.Errorf("publish: npm pack: %w", err)
	}
	var packResults []struct {
		Filename string `json:"filename"`
	}
	if err := json.Unmarshal(packOut, &packResults); err != nil {
		return fmt.Errorf("publish: parse npm pack output: %w", err)
	}
	if len(packResults) == 0 {
		return fmt.Errorf("publish: npm pack produced no output")
	}
	tgzPath := filepath.Join(spec.PackageDir, packResults[0].Filename)

	// npm publish
	args := []string{"publish", tgzPath, "--access", "public"}
	if spec.Provenance {
		args = append(args, "--provenance")
	}
	pubCmd := exec.CommandContext(ctx, npmBin, args...)
	pubCmd.Dir = spec.PackageDir
	pubCmd.Env = append(os.Environ(), "NODE_AUTH_TOKEN="+token)
	out, err := pubCmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("publish: npm publish: %w\n%s", err, out)
	}

	// Verify published within 30 seconds
	pkgName, version, err := readPackageNameVersion(spec.PackageDir)
	if err != nil {
		return err
	}
	return waitForNpm(ctx, pkgName, version, 30*time.Second)
}

func readPackageNameVersion(pkgDir string) (string, string, error) {
	data, err := os.ReadFile(filepath.Join(pkgDir, "package.json"))
	if err != nil {
		return "", "", fmt.Errorf("publish: read package.json: %w", err)
	}
	var pkg struct {
		Name    string `json:"name"`
		Version string `json:"version"`
	}
	if err := json.Unmarshal(data, &pkg); err != nil {
		return "", "", fmt.Errorf("publish: parse package.json: %w", err)
	}
	return pkg.Name, pkg.Version, nil
}

func waitForNpm(ctx context.Context, name, version string, timeout time.Duration) error {
	deadline := time.Now().Add(timeout)
	url := fmt.Sprintf("https://registry.npmjs.org/%s/%s", strings.TrimPrefix(name, "@"), version)
	for time.Now().Before(deadline) {
		req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
		if err != nil {
			return err
		}
		resp, err := http.DefaultClient.Do(req)
		if err == nil && resp.StatusCode == http.StatusOK {
			io.Copy(io.Discard, resp.Body)
			resp.Body.Close()
			return nil
		}
		if resp != nil {
			resp.Body.Close()
		}
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-time.After(3 * time.Second):
		}
	}
	return fmt.Errorf("publish: %s@%s not visible in npm registry after %s", name, version, timeout)
}

// PublishJsr publishes a package to JSR using deno publish.
func PublishJsr(ctx context.Context, spec JsrPublishSpec) error {
	denoBin := spec.DenoBin
	if denoBin == "" {
		denoBin = "deno"
	}
	cmd := exec.CommandContext(ctx, denoBin, "publish", "--allow-dirty")
	cmd.Dir = spec.PackageDir
	out, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("publish: deno publish: %w\n%s", err, out)
	}
	return nil
}
