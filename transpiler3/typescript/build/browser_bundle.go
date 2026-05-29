// Phase 17.2: browser bundle via `bun build` (preferred) or
// `esbuild` (fallback).
//
// The Phase 15 emit ships `dist/browser/index.ts` as the browser-
// targeted entry. Phase 17.2 bundles it into a single tree-shaken
// ESM file at `dist/bundle/index.js`. The bundler is invoked via
// `bun build` when bun is available (bun 1.1+ has a built-in
// bundler with the same target flags as esbuild), and falls back
// to a direct `esbuild` invocation when present.
//
// Why bun build (not webpack / rollup): bun's bundler is built on
// top of an esbuild-like architecture (parallel, single-pass,
// platform-conditional), ships with the host runtime, requires no
// `node_modules`, and matches esbuild's tree-shaking semantics for
// the `"browser"` export condition.
//
// runBrowserBundle returns the absolute path of the emitted
// bundle. It fails if neither bundler is on PATH; the test gate
// skips when both are missing.
package build

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// runBrowserBundle bundles <pkgDir>/dist/browser/index.ts into
// <pkgDir>/dist/bundle/index.js. Prefers `bun build`; falls back
// to `esbuild`. Returns the bundle path.
func runBrowserBundle(pkgDir string) (string, error) {
	entry := filepath.Join(pkgDir, "dist", "browser", "index.ts")
	if _, err := os.Stat(entry); err != nil {
		return "", fmt.Errorf("browser bundle entry missing: %s: %w", entry, err)
	}
	bundleDir := filepath.Join(pkgDir, "dist", "bundle")
	if err := os.MkdirAll(bundleDir, 0o755); err != nil {
		return "", fmt.Errorf("browser bundle: mkdir %s: %w", bundleDir, err)
	}
	bundlePath := filepath.Join(bundleDir, "index.js")

	if bun, err := resolveBun(); err == nil {
		cmd := exec.Command(
			bun, "build", entry,
			"--target=browser",
			"--format=esm",
			"--outfile="+bundlePath,
		)
		cmd.Dir = pkgDir
		cmd.Env = os.Environ()
		out, err := cmd.CombinedOutput()
		if err != nil {
			return "", fmt.Errorf("bun build failed: %w\noutput:\n%s", err, string(out))
		}
		return bundlePath, nil
	}
	if esbuild, err := exec.LookPath("esbuild"); err == nil {
		cmd := exec.Command(
			esbuild, entry,
			"--bundle",
			"--format=esm",
			"--target=es2024",
			"--platform=browser",
			"--tree-shaking=true",
			"--outfile="+bundlePath,
		)
		cmd.Dir = pkgDir
		cmd.Env = os.Environ()
		out, err := cmd.CombinedOutput()
		if err != nil {
			return "", fmt.Errorf("esbuild failed: %w\noutput:\n%s", err, string(out))
		}
		return bundlePath, nil
	}
	return "", fmt.Errorf("browser bundle: neither bun nor esbuild on PATH; install bun (https://bun.sh) or esbuild (https://esbuild.github.io)")
}
