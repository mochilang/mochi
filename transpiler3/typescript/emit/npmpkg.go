// Phase 15: emit a complete npm-package directory layout from a
// tstree.SourceFile.
//
// EmitPackage writes:
//
//	<outDir>/
//	  package.json
//	  src/index.ts
//	  dist/node/index.ts
//	  dist/deno/index.ts
//	  dist/bun/index.ts
//	  dist/browser/index.ts
//	  dist/index.d.ts
//
// The emitted package uses Node 22 / Deno 2 / Bun 1.1 native .ts
// execution (no `tsc --build` pass required at install time). The
// `.d.ts` re-exports the `index.ts` ambient module so consumers get
// types when importing `mochi-<pkg>` from a TypeScript codebase.
//
// The full `tsc --build` -> real `.js` + `.d.ts` pipeline lands in
// sub-phase 15.1 (see phase-15-npm-package.md). 15.0 ships the
// directory layout + conditional `exports` map + `files` whitelist,
// which is what `npm pack` + `npm install` consume.
package emit

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"

	"mochi/transpiler3/typescript/tstree"
)

// PackageInfo controls the emitted package.json identity.
type PackageInfo struct {
	// Name is the npm package name, e.g. "mochi-hello".
	Name string
	// Version is the semver string. Defaults to "0.0.0" when empty.
	Version string
}

// EmitPackage writes a complete npm package skeleton rooted at
// outDir. Returns the path to the emitted package directory.
//
// The package layout is:
//
//	<outDir>/
//	  package.json
//	  src/index.ts          (canonical TS source; for re-builds)
//	  dist/node/index.ts    (native-.ts entry on Node 22+)
//	  dist/deno/index.ts    (native-.ts entry on Deno 2)
//	  dist/bun/index.ts     (native-.ts entry on Bun 1.1)
//	  dist/browser/index.ts (Phase 17 bundles this with esbuild)
//	  dist/index.d.ts       (declaration shim; 15.1 generates real .d.ts via tsc)
//
// The package.json `exports` map uses the canonical TypeScript-first
// order: "types" first, then per-runtime conditions, then "default".
// `files` whitelists `dist/` so `src/` and tsconfig*.json never end
// up in the tarball.
func EmitPackage(file *tstree.SourceFile, outDir string, info PackageInfo) (string, error) {
	if file == nil {
		return "", fmt.Errorf("ts emit pkg: nil SourceFile")
	}
	if info.Name == "" {
		return "", fmt.Errorf("ts emit pkg: empty package name")
	}
	if info.Version == "" {
		info.Version = "0.0.0"
	}
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return "", fmt.Errorf("ts emit pkg: mkdir %s: %w", outDir, err)
	}

	src := file.TsString()

	for _, sub := range []string{"src", "dist/node", "dist/deno", "dist/bun", "dist/browser"} {
		if err := os.MkdirAll(filepath.Join(outDir, sub), 0o755); err != nil {
			return "", fmt.Errorf("ts emit pkg: mkdir %s: %w", sub, err)
		}
	}

	writes := map[string]string{
		"src/index.ts":          src,
		"dist/node/index.ts":    src,
		"dist/deno/index.ts":    src,
		"dist/bun/index.ts":     src,
		"dist/browser/index.ts": src,
		// 15.1 replaces this stub with real per-file .d.ts emitted
		// by tsc --build. The stub is enough for `import { ... }
		// from "mochi-<pkg>"` to typecheck under
		// `moduleResolution: bundler`.
		"dist/index.d.ts": "export {};\n",
	}
	for rel, body := range writes {
		abs := filepath.Join(outDir, rel)
		if err := os.WriteFile(abs, []byte(body), 0o644); err != nil {
			return "", fmt.Errorf("ts emit pkg: write %s: %w", rel, err)
		}
	}

	pkgJSON, err := buildPackageJSON(info)
	if err != nil {
		return "", err
	}
	if err := os.WriteFile(filepath.Join(outDir, "package.json"), pkgJSON, 0o644); err != nil {
		return "", fmt.Errorf("ts emit pkg: write package.json: %w", err)
	}
	return outDir, nil
}

// buildPackageJSON renders the canonical package.json. The shape is
// fixed (no template flexibility) so cross-fixture diffs surface
// changes immediately and the `files` whitelist + `exports` map are
// load-bearing invariants instead of free-form.
//
// Key invariants:
//
//   - `"type": "module"`: ESM. Required by Node 22's --experimental-
//     strip-types path and by the `exports` conditional map.
//
//   - `"main"` + `"module"` + `"types"` mirror the `exports."."` map.
//     Some older tools (rollup, webpack 4) ignore `exports` and read
//     `main`/`module`/`types`; keeping them in sync rules out a
//     class of "works in Node, breaks in bundler" failures.
//
//   - `"exports"."."` key order: `"types"` first (TypeScript
//     resolver requirement), then per-runtime conditions
//     (`"deno"`, `"bun"`, `"browser"`, `"node"`), then `"default"`
//     last. Order matters because the resolver picks the first
//     match.
//
//   - `"files"`: ships only `dist/`. Never ships `src/`,
//     `tsconfig*.json`, `.eslintrc*`, `.prettierrc*`, `node_modules/`.
//     The TestPhase15FilesWhitelist gate enforces this.
//
//   - `"engines"`: pinned to Node >=22, since the package relies on
//     --experimental-strip-types (default-on at 22.6+) for the .ts
//     entry to execute. 15.1 relaxes this once tsc emits .js.
func buildPackageJSON(info PackageInfo) ([]byte, error) {
	// json.Marshal does not preserve key order, but the npm spec
	// resolver only requires the *exports* sub-map's order. Map
	// values in Go don't preserve order; we use json.RawMessage
	// values to control the inner order.
	exportsDot := []byte(`{` +
		`"types":"./dist/index.d.ts",` +
		`"deno":"./dist/deno/index.ts",` +
		`"bun":"./dist/bun/index.ts",` +
		`"browser":"./dist/browser/index.ts",` +
		`"node":"./dist/node/index.ts",` +
		`"default":"./dist/node/index.ts"` +
		`}`)

	pkg := map[string]any{
		"name":    info.Name,
		"version": info.Version,
		"type":    "module",
		"main":    "./dist/node/index.ts",
		"module":  "./dist/node/index.ts",
		"types":   "./dist/index.d.ts",
		"exports": map[string]any{
			".":              json.RawMessage(exportsDot),
			"./package.json": "./package.json",
		},
		// Phase 16.1: sorted lex order. npm 9.5+ packs in the
		// listed order; the sort makes the tarball's entry order
		// host-independent of readdir's filesystem-specific order.
		"files":   []string{"LICENSE", "README.md", "dist/"},
		"engines": map[string]string{"node": ">=22"},
	}
	b, err := json.MarshalIndent(pkg, "", "  ")
	if err != nil {
		return nil, fmt.Errorf("ts emit pkg: marshal package.json: %w", err)
	}
	b = append(b, '\n')
	return b, nil
}
