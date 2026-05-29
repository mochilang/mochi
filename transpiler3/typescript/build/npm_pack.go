// Phase 15: `npm pack` invocation and package-naming utilities.
//
// runNpmPack runs `npm pack --json` in pkgDir and parses the returned
// `[{ "filename": "<pkg>-<ver>.tgz", ... }]` array to find the
// emitted tarball path. The tarball is written into pkgDir (npm's
// default), and the function returns its absolute path.
//
// `npm pack`, not `npm publish --dry-run --pack-destination ...`,
// is used here because:
//
//   - `npm pack` requires no registry login and no network.
//   - The tarball it writes is bit-identical to what `npm publish`
//     uploads. Phase 18 reuses the same tarball with Sigstore +
//     OIDC + provenance.
//   - `--json` gives a stable, parseable filename without screen-
//     scraping npm's human-readable summary.
//
// The subprocess inherits the parent env (so MOCHI_NPM_PATH /
// PATH-resolved npm both work) and runs with `cwd = pkgDir` so
// the emitted package.json drives the tarball name.
package build

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// runNpmPack runs `npm pack --json` in pkgDir and returns the
// absolute path of the resulting .tgz tarball. The tarball is
// always written into pkgDir itself.
//
// On non-zero exit it returns an error including npm's stderr.
func runNpmPack(pkgDir string) (string, error) {
	npm, err := resolveNpm()
	if err != nil {
		return "", err
	}
	cmd := exec.Command(npm, "pack", "--json")
	cmd.Dir = pkgDir
	cmd.Env = os.Environ()
	out, err := cmd.Output()
	if err != nil {
		stderr := ""
		if ee, ok := err.(*exec.ExitError); ok {
			stderr = string(ee.Stderr)
		}
		return "", fmt.Errorf("npm pack in %s failed: %w\nstderr:\n%s", pkgDir, err, stderr)
	}
	var packed []struct {
		Filename string `json:"filename"`
	}
	if err := json.Unmarshal(out, &packed); err != nil {
		return "", fmt.Errorf("npm pack: parse --json output: %w\nraw:\n%s", err, string(out))
	}
	if len(packed) == 0 {
		return "", fmt.Errorf("npm pack: empty --json result (raw: %q)", string(out))
	}
	// npm 8+ writes the tarball into cwd (pkgDir). The filename
	// field is the basename; resolve to absolute.
	abs := filepath.Join(pkgDir, packed[0].Filename)
	if _, err := os.Stat(abs); err != nil {
		return "", fmt.Errorf("npm pack: claimed tarball %s missing: %w", abs, err)
	}
	return abs, nil
}

// resolveNpm finds the npm binary. Honours MOCHI_NPM_PATH first
// (CI sets it via actions/setup-node@v4 nodes-by-path matrix), then
// walks well-known install locations, then falls back to PATH.
func resolveNpm() (string, error) {
	if p := os.Getenv("MOCHI_NPM_PATH"); p != "" {
		return p, nil
	}
	for _, candidate := range []string{
		"/usr/bin/npm",
		"/usr/local/bin/npm",
		"/opt/homebrew/bin/npm",
	} {
		if _, err := os.Stat(candidate); err == nil {
			return candidate, nil
		}
	}
	// Resolve npm from the same dir as the resolved node binary.
	// nvm installs put npm and node in the same `bin/` so this
	// avoids surprises when MOCHI_NODE_PATH is set but PATH isn't.
	if nodePath, nerr := resolveNode(); nerr == nil {
		candidate := filepath.Join(filepath.Dir(nodePath), "npm")
		if _, err := os.Stat(candidate); err == nil {
			return candidate, nil
		}
	}
	p, err := exec.LookPath("npm")
	if err != nil {
		return "", fmt.Errorf("npm not found on PATH (set MOCHI_NPM_PATH or add npm to PATH): %w", err)
	}
	return p, nil
}

// npmPackageNameFromSrc derives a valid npm package name from a
// `.mochi` source path. The npm spec allows lowercase ASCII, digits,
// hyphens, and underscores (no spaces, dots, or uppercase). The
// emitted name is `mochi-<basename-lowercased>`.
func npmPackageNameFromSrc(src string) string {
	base := filepath.Base(src)
	base = strings.TrimSuffix(base, ".mochi")
	var b strings.Builder
	b.WriteString("mochi-")
	for _, r := range base {
		switch {
		case r >= 'a' && r <= 'z':
			b.WriteRune(r)
		case r >= 'A' && r <= 'Z':
			b.WriteRune(r + ('a' - 'A'))
		case r >= '0' && r <= '9':
			b.WriteRune(r)
		case r == '-' || r == '_':
			b.WriteRune(r)
		default:
			b.WriteRune('-')
		}
	}
	return b.String()
}
