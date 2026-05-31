// Phase 17: npm and Deno/JSR package emit.
//
// Driver.BuildNpmPackage(src, outDir) produces a complete npm package
// with package.json, tsconfig.json, and the transpiled source.
//
// Driver.BuildDenoModule(src, outDir) produces a Deno-compatible module
// with deno.json and the transpiled mod.ts.
//
// Driver.EmitCIWorkflow(outDir) writes a GitHub Actions workflow for
// npm publish (with provenance) or deno publish.
package build

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// TargetNpmPackage is a build target that produces a complete npm package.
const TargetNpmPackage Target = 4

// TargetDenoModule is a build target that produces a Deno/JSR module.
const TargetDenoModule Target = 5

// NpmPackageSpec carries metadata for npm package generation.
type NpmPackageSpec struct {
	Name        string
	Version     string
	Description string
}

// BuildNpmPackage transpiles src and emits a complete npm package structure
// under outDir:
//
//	out/
//	  src/index.ts   — transpiled Mochi program
//	  package.json   — name, version, type, exports
//	  tsconfig.json  — NodeNext module, ES2022 target
//	  .npmignore
func (d *Driver) BuildNpmPackage(src, outDir string, spec NpmPackageSpec) (string, error) {
	// Default name from filename if not provided.
	name := spec.Name
	if name == "" {
		base := filepath.Base(src)
		name = strings.TrimSuffix(base, filepath.Ext(base))
	}
	version := spec.Version
	if version == "" {
		version = "0.1.0"
	}

	srcDir := filepath.Join(outDir, "src")
	if err := os.MkdirAll(srcDir, 0o755); err != nil {
		return "", fmt.Errorf("ts build npm: mkdir src: %w", err)
	}

	// Transpile source to src/index.ts.
	tsPath, err := d.Build(src, srcDir, TargetTypeScriptSource)
	if err != nil {
		return "", fmt.Errorf("ts build npm: transpile: %w", err)
	}
	// Rename to index.ts.
	indexPath := filepath.Join(srcDir, "index.ts")
	if tsPath != indexPath {
		if err := os.Rename(tsPath, indexPath); err != nil {
			return "", fmt.Errorf("ts build npm: rename to index.ts: %w", err)
		}
	}

	// Write package.json.
	pkgJSON := map[string]interface{}{
		"name":    name,
		"version": version,
		"type":    "module",
		"main":    "./src/index.ts",
		"exports": map[string]string{".": "./src/index.ts"},
	}
	if spec.Description != "" {
		pkgJSON["description"] = spec.Description
	}
	pkgBytes, err := json.MarshalIndent(pkgJSON, "", "  ")
	if err != nil {
		return "", fmt.Errorf("ts build npm: marshal package.json: %w", err)
	}
	if err := os.WriteFile(filepath.Join(outDir, "package.json"), append(pkgBytes, '\n'), 0o644); err != nil {
		return "", fmt.Errorf("ts build npm: write package.json: %w", err)
	}

	// Write tsconfig.json.
	tscfg := `{
  "compilerOptions": {
    "module": "NodeNext",
    "moduleResolution": "NodeNext",
    "target": "ES2022",
    "strict": true,
    "outDir": "./dist"
  },
  "include": ["src/**/*.ts"]
}
`
	if err := os.WriteFile(filepath.Join(outDir, "tsconfig.json"), []byte(tscfg), 0o644); err != nil {
		return "", fmt.Errorf("ts build npm: write tsconfig.json: %w", err)
	}

	// Write .npmignore.
	npmIgnore := "*.ts\ntsconfig.json\n"
	if err := os.WriteFile(filepath.Join(outDir, ".npmignore"), []byte(npmIgnore), 0o644); err != nil {
		return "", fmt.Errorf("ts build npm: write .npmignore: %w", err)
	}

	return outDir, nil
}

// BuildDenoModule transpiles src and emits a Deno/JSR module structure:
//
//	out/
//	  mod.ts      — transpiled Mochi program
//	  deno.json   — name, version, exports
func (d *Driver) BuildDenoModule(src, outDir string, spec NpmPackageSpec) (string, error) {
	name := spec.Name
	if name == "" {
		base := filepath.Base(src)
		name = "@mochi/" + strings.TrimSuffix(base, filepath.Ext(base))
	}
	version := spec.Version
	if version == "" {
		version = "0.1.0"
	}

	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return "", fmt.Errorf("ts build deno: mkdir: %w", err)
	}

	// Transpile to mod.ts.
	tsPath, err := d.Build(src, outDir, TargetTypeScriptSource)
	if err != nil {
		return "", fmt.Errorf("ts build deno: transpile: %w", err)
	}
	modPath := filepath.Join(outDir, "mod.ts")
	if tsPath != modPath {
		if err := os.Rename(tsPath, modPath); err != nil {
			return "", fmt.Errorf("ts build deno: rename to mod.ts: %w", err)
		}
	}

	// Write deno.json.
	denoJSON := map[string]interface{}{
		"name":    name,
		"version": version,
		"exports": map[string]string{".": "./mod.ts"},
	}
	denoBytes, err := json.MarshalIndent(denoJSON, "", "  ")
	if err != nil {
		return "", fmt.Errorf("ts build deno: marshal deno.json: %w", err)
	}
	if err := os.WriteFile(filepath.Join(outDir, "deno.json"), append(denoBytes, '\n'), 0o644); err != nil {
		return "", fmt.Errorf("ts build deno: write deno.json: %w", err)
	}

	return outDir, nil
}

// EmitCIWorkflow writes a GitHub Actions npm publish workflow to
// outDir/.github/workflows/npm-publish.yml. When useJSR is true,
// emits a deno publish workflow instead.
func (d *Driver) EmitCIWorkflow(outDir string, useJSR bool) error {
	dir := filepath.Join(outDir, ".github", "workflows")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return fmt.Errorf("ts build ci: mkdir: %w", err)
	}
	var content string
	if useJSR {
		content = denoPublishWorkflow
	} else {
		content = npmPublishWorkflow
	}
	fname := "npm-publish.yml"
	if useJSR {
		fname = "jsr-publish.yml"
	}
	return os.WriteFile(filepath.Join(dir, fname), []byte(content), 0o644)
}

const npmPublishWorkflow = `name: Publish to npm
on:
  push:
    tags: ['v*']
jobs:
  publish:
    runs-on: ubuntu-latest
    permissions:
      id-token: write
      contents: read
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: '22'
          registry-url: 'https://registry.npmjs.org'
      - run: npm ci
      - run: npm publish --provenance
        env:
          NODE_AUTH_TOKEN: ${{ secrets.NPM_TOKEN }}
`

const denoPublishWorkflow = `name: Publish to JSR
on:
  push:
    tags: ['v*']
jobs:
  publish:
    runs-on: ubuntu-latest
    permissions:
      id-token: write
      contents: read
    steps:
      - uses: actions/checkout@v4
      - uses: denoland/setup-deno@v2
        with:
          deno-version: '2.x'
      - run: deno publish --token-source=github-actions
`
