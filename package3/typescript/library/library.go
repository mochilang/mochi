// Package library emits npm-publishable and JSR-publishable packages from the
// Mochi API surface. Lands in MEP-72 Phase 10.
package library

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// NpmLibrarySpec describes an npm library target.
type NpmLibrarySpec struct {
	Name        string
	Version     string
	Description string
	TypeScript  string // src/index.ts content (transpiled Mochi)
}

// JsrLibrarySpec describes a JSR library target.
type JsrLibrarySpec struct {
	Name    string // "@scope/package"
	Version string
	TypeScript string // src/index.ts content
}

// TargetNpmLibrary emits an npm-publishable package to outDir.
func TargetNpmLibrary(spec NpmLibrarySpec, outDir string) error {
	srcDir := filepath.Join(outDir, "src")
	distDir := filepath.Join(outDir, "dist")
	for _, d := range []string{srcDir, distDir} {
		if err := os.MkdirAll(d, 0o755); err != nil {
			return fmt.Errorf("library: mkdir %s: %w", d, err)
		}
	}

	// src/index.ts
	if err := os.WriteFile(filepath.Join(srcDir, "index.ts"), []byte(spec.TypeScript), 0o644); err != nil {
		return fmt.Errorf("library: write src/index.ts: %w", err)
	}

	// dist/index.d.ts (stub)
	dts := extractDeclarations(spec.TypeScript)
	if err := os.WriteFile(filepath.Join(distDir, "index.d.ts"), []byte(dts), 0o644); err != nil {
		return fmt.Errorf("library: write dist/index.d.ts: %w", err)
	}

	// dist/index.js (CJS stub)
	cjs := toCJS(spec.TypeScript)
	if err := os.WriteFile(filepath.Join(distDir, "index.js"), []byte(cjs), 0o644); err != nil {
		return fmt.Errorf("library: write dist/index.js: %w", err)
	}

	// dist/index.mjs (ESM, same as src)
	if err := os.WriteFile(filepath.Join(distDir, "index.mjs"), []byte(spec.TypeScript), 0o644); err != nil {
		return fmt.Errorf("library: write dist/index.mjs: %w", err)
	}

	// package.json
	pkgJSON := map[string]interface{}{
		"name":        spec.Name,
		"version":     spec.Version,
		"description": spec.Description,
		"type":        "module",
		"exports": map[string]interface{}{
			".": map[string]string{
				"import":  "./dist/index.mjs",
				"require": "./dist/index.js",
				"types":   "./dist/index.d.ts",
			},
		},
		"files": []string{"dist", "src"},
	}
	pkgData, err := json.MarshalIndent(pkgJSON, "", "  ")
	if err != nil {
		return fmt.Errorf("library: marshal package.json: %w", err)
	}
	if err := os.WriteFile(filepath.Join(outDir, "package.json"), pkgData, 0o644); err != nil {
		return fmt.Errorf("library: write package.json: %w", err)
	}

	// tsconfig.json
	tsCfg := `{
  "compilerOptions": {
    "target": "ES2022",
    "module": "ESNext",
    "moduleResolution": "bundler",
    "declaration": true,
    "outDir": "dist",
    "strict": true
  },
  "include": ["src"]
}
`
	if err := os.WriteFile(filepath.Join(outDir, "tsconfig.json"), []byte(tsCfg), 0o644); err != nil {
		return fmt.Errorf("library: write tsconfig.json: %w", err)
	}

	return nil
}

// TargetJsrLibrary emits a JSR-publishable package to outDir.
func TargetJsrLibrary(spec JsrLibrarySpec, outDir string) error {
	srcDir := filepath.Join(outDir, "src")
	if err := os.MkdirAll(srcDir, 0o755); err != nil {
		return fmt.Errorf("library: mkdir src: %w", err)
	}

	if err := os.WriteFile(filepath.Join(srcDir, "index.ts"), []byte(spec.TypeScript), 0o644); err != nil {
		return fmt.Errorf("library: write src/index.ts: %w", err)
	}

	// deno.json
	denoJSON := map[string]interface{}{
		"name":    spec.Name,
		"version": spec.Version,
		"exports": map[string]string{".": "./src/index.ts"},
	}
	denoData, err := json.MarshalIndent(denoJSON, "", "  ")
	if err != nil {
		return fmt.Errorf("library: marshal deno.json: %w", err)
	}
	if err := os.WriteFile(filepath.Join(outDir, "deno.json"), denoData, 0o644); err != nil {
		return fmt.Errorf("library: write deno.json: %w", err)
	}

	return nil
}

// extractDeclarations generates a minimal .d.ts from TypeScript source.
func extractDeclarations(src string) string {
	var b strings.Builder
	b.WriteString("// Auto-generated declarations\n")
	for _, line := range strings.Split(src, "\n") {
		line = strings.TrimSpace(line)
		if strings.HasPrefix(line, "export function ") || strings.HasPrefix(line, "export const ") || strings.HasPrefix(line, "export class ") {
			// Strip function body and add declaration.
			if idx := strings.IndexByte(line, '{'); idx > 0 {
				line = strings.TrimSpace(line[:idx]) + ";"
			}
			b.WriteString("declare " + line + "\n")
		}
	}
	return b.String()
}

// toCJS wraps ESM exports in a CommonJS module.
func toCJS(src string) string {
	var b strings.Builder
	b.WriteString(`"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
`)
	for _, line := range strings.Split(src, "\n") {
		line = strings.TrimSpace(line)
		if strings.HasPrefix(line, "export function ") {
			// export function foo(...) { ... } → exports.foo = function foo(...) { ... }
			rest := strings.TrimPrefix(line, "export ")
			b.WriteString("exports." + extractName(rest) + " = " + rest + "\n")
		}
	}
	return b.String()
}

func extractName(s string) string {
	// "function foo(" → "foo"
	parts := strings.Fields(s)
	if len(parts) >= 2 {
		name := parts[1]
		if idx := strings.IndexByte(name, '('); idx > 0 {
			return name[:idx]
		}
		return name
	}
	return "unknown"
}
