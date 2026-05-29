// Phase 17.0: JSR (jsr.io) manifest emit + `deno publish --dry-run`.
//
// JSR is the Deno-native registry (GA September 2024). It accepts
// TypeScript source directly, transpiles server-side, and serves
// `jsr:@scope/pkg` specifiers to Deno + Node + Bun. The Mochi
// driver emits a `jsr.json` next to `package.json` and reuses
// the Phase 15 `src/index.ts` as the published entry point.
//
// Why source-not-dist: JSR's server transpiles `.ts` and emits
// the `.d.ts` automatically; uploading `dist/` would double the
// payload and the server would re-transpile anyway. The `dist/`
// tree stays npm-only.
//
// runDenoPublishDryRun runs `deno publish --dry-run --allow-dirty`
// in the package directory. The flag is structural-only: it
// validates `jsr.json`, walks the include list, and reports any
// errors without touching the network. The real `deno publish`
// (Phase 18) wires JSR OIDC + provenance on top.
package build

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// jsrManifestInfo controls the emitted jsr.json identity.
type jsrManifestInfo struct {
	Name    string // e.g. "@mochi/hello"
	Version string // semver, defaults to "0.0.0"
}

// emitJsrManifest writes <outDir>/jsr.json. The shape is the JSR
// minimal manifest: name + version + exports + publish.include.
//
// The exports map points at `src/index.ts` (the Phase 15 emit's
// canonical TS source). publish.include limits the uploaded
// payload to `src/**/*.ts` plus README + LICENSE so the server
// never sees `dist/`, `node_modules/`, or tooling config.
func emitJsrManifest(outDir string, info jsrManifestInfo) (string, error) {
	if info.Name == "" {
		return "", fmt.Errorf("ts emit jsr: empty package name")
	}
	if info.Version == "" {
		info.Version = "0.0.0"
	}
	if !strings.HasPrefix(info.Name, "@") {
		return "", fmt.Errorf("ts emit jsr: JSR name must be scoped (@scope/pkg), got %q", info.Name)
	}
	manifest := map[string]any{
		"name":    info.Name,
		"version": info.Version,
		"license": "MIT",
		"exports": "./src/index.ts",
		"publish": map[string]any{
			"include": []string{"src/**/*.ts", "README.md", "LICENSE"},
		},
	}
	body, err := json.MarshalIndent(manifest, "", "  ")
	if err != nil {
		return "", fmt.Errorf("ts emit jsr: marshal: %w", err)
	}
	body = append(body, '\n')
	abs := filepath.Join(outDir, "jsr.json")
	if err := os.WriteFile(abs, body, 0o644); err != nil {
		return "", fmt.Errorf("ts emit jsr: write %s: %w", abs, err)
	}
	return abs, nil
}

// jsrNameFromNpm converts the npm package name to JSR's scoped
// form. The Mochi project's reserved JSR scope is `@mochi`; the
// npm package name is `mochi-<basename>`, so the JSR form is
// `@mochi/<basename>`.
//
// When a future Mochi version supports user-published packages
// under a non-mochi scope, this helper accepts the scope from
// the build driver's PackageInfo.
func jsrNameFromNpm(npmName string) string {
	const prefix = "mochi-"
	if strings.HasPrefix(npmName, prefix) {
		return "@mochi/" + strings.TrimPrefix(npmName, prefix)
	}
	return "@mochi/" + npmName
}

// runDenoPublishDryRun invokes `deno publish --dry-run
// --allow-dirty` in pkgDir. The flag combination validates the
// jsr.json + include list + transpiles each .ts entry, without
// hitting the network. Skipped (no error) when deno is not on
// PATH; that path is taken on CI runners that have not yet wired
// the Deno setup action.
func runDenoPublishDryRun(pkgDir string) error {
	deno, err := resolveDeno()
	if err != nil {
		// no deno on host -> skip the gate. The manifest emit is
		// already structurally validated by phase17_test.go.
		return nil
	}
	cmd := exec.Command(deno, "publish", "--dry-run", "--allow-dirty")
	cmd.Dir = pkgDir
	cmd.Env = os.Environ()
	out, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("deno publish --dry-run in %s failed: %w\noutput:\n%s", pkgDir, err, string(out))
	}
	return nil
}
