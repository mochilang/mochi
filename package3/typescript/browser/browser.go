// Package browser emits a browser-compatible bundle and enforces a size gate.
// Lands in MEP-72 Phase 11.
package browser

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
)

const defaultMaxBundleKB = 500

// BundleResult carries the output bundle path and size.
type BundleResult struct {
	BundlePath string
	SizeBytes  int64
}

// BundleSpec describes a browser bundle target.
type BundleSpec struct {
	Entrypoint string
	OutDir     string
	MaxKB      int    // 0 = use default 500 KB
	EsbuildBin string // path to esbuild; empty = "esbuild" from PATH
}

// BundleForBrowser bundles entrypoint for browser using esbuild and enforces the size gate.
func BundleForBrowser(ctx context.Context, spec BundleSpec) (BundleResult, error) {
	if err := os.MkdirAll(spec.OutDir, 0o755); err != nil {
		return BundleResult{}, fmt.Errorf("browser: mkdir %s: %w", spec.OutDir, err)
	}
	outFile := filepath.Join(spec.OutDir, "bundle.js")
	esbuild := spec.EsbuildBin
	if esbuild == "" {
		esbuild = "esbuild"
	}

	args := []string{
		spec.Entrypoint,
		"--bundle",
		"--format=esm",
		"--target=es2022",
		"--platform=browser",
		"--outfile=" + outFile,
	}
	cmd := exec.CommandContext(ctx, esbuild, args...)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return BundleResult{}, fmt.Errorf("browser: esbuild: %w\n%s", err, out)
	}

	info, err := os.Stat(outFile)
	if err != nil {
		return BundleResult{}, fmt.Errorf("browser: stat bundle: %w", err)
	}
	size := info.Size()
	maxKB := spec.MaxKB
	if maxKB <= 0 {
		maxKB = defaultMaxBundleKB
	}
	if size > int64(maxKB)*1024 {
		return BundleResult{BundlePath: outFile, SizeBytes: size},
			fmt.Errorf("browser: bundle size %s exceeds limit of %d KB",
				humanBytes(size), maxKB)
	}
	return BundleResult{BundlePath: outFile, SizeBytes: size}, nil
}

func humanBytes(n int64) string {
	kb := n / 1024
	if kb >= 1024 {
		return strconv.FormatInt(kb/1024, 10) + " MB"
	}
	return strconv.FormatInt(kb, 10) + " KB"
}

// ValidateBrowserCompatible checks that the bundle does not import Node.js built-ins.
func ValidateBrowserCompatible(bundlePath string) error {
	data, err := os.ReadFile(bundlePath)
	if err != nil {
		return fmt.Errorf("browser: read bundle: %w", err)
	}
	content := string(data)
	nodeBuiltins := []string{
		`"fs"`, `'fs'`, `"path"`, `'path'`, `"crypto"`, `'crypto'`,
		`"net"`, `'net'`, `"os"`, `'os'`, `"child_process"`, `'child_process'`,
		`"stream"`, `'stream'`, `"buffer"`, `'buffer'`,
	}
	for _, bi := range nodeBuiltins {
		if strings.Contains(content, "require("+bi+")") ||
			strings.Contains(content, "from "+bi) {
			return fmt.Errorf("browser: bundle imports Node.js built-in %s", bi)
		}
	}
	return nil
}
