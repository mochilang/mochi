// Package tsingest embeds the ts-ingest Node.js helper and exposes a Go runner.
// Lands in MEP-72 Phase 3.
package tsingest

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	_ "embed"
)

//go:embed index.mts
var indexMTS []byte

//go:embed package.json
var packageJSON []byte

//go:embed tsconfig.json
var tsconfigJSON []byte

// Extractor runs ts-ingest in a temp directory.
type Extractor struct {
	NodeBin string // empty = "node"
	NpmBin  string // empty = "npm"
}

// ExtractResult is the path to the generated ApiSurface JSON.
type ExtractResult struct {
	SurfacePath string
	TempDir     string // caller should os.RemoveAll when done
}

// Run extracts the API surface from dtsDir into a JSON file.
// The caller is responsible for removing result.TempDir.
func (e *Extractor) Run(ctx context.Context, dtsDir, pkgName, version string) (*ExtractResult, error) {
	tmpDir, err := os.MkdirTemp("", "mochi-ts-ingest-*")
	if err != nil {
		return nil, fmt.Errorf("tsingest: mktemp: %w", err)
	}

	// Write embedded files to temp dir.
	files := map[string][]byte{
		"index.mts":    indexMTS,
		"package.json": packageJSON,
		"tsconfig.json": tsconfigJSON,
	}
	for name, data := range files {
		if err := os.WriteFile(filepath.Join(tmpDir, name), data, 0o644); err != nil {
			os.RemoveAll(tmpDir)
			return nil, fmt.Errorf("tsingest: write %s: %w", name, err)
		}
	}

	// npm install in tmpDir to get typescript dependency.
	npmBin := e.NpmBin
	if npmBin == "" {
		npmBin = "npm"
	}
	installCmd := exec.CommandContext(ctx, npmBin, "install", "--silent")
	installCmd.Dir = tmpDir
	if out, err := installCmd.CombinedOutput(); err != nil {
		os.RemoveAll(tmpDir)
		return nil, fmt.Errorf("tsingest: npm install: %w\n%s", err, out)
	}

	// Run tsc to compile index.mts → index.mjs.
	tscPath := filepath.Join(tmpDir, "node_modules", ".bin", "tsc")
	tscCmd := exec.CommandContext(ctx, tscPath)
	tscCmd.Dir = tmpDir
	if out, err := tscCmd.CombinedOutput(); err != nil {
		os.RemoveAll(tmpDir)
		return nil, fmt.Errorf("tsingest: tsc: %w\n%s", err, out)
	}

	// Run the extractor.
	nodeBin := e.NodeBin
	if nodeBin == "" {
		nodeBin = "node"
	}
	surfacePath := filepath.Join(tmpDir, "api_surface.json")
	args := []string{
		filepath.Join(tmpDir, "index.mjs"),
		dtsDir,
		"--out", surfacePath,
		"--package", pkgName,
		"--version", version,
	}
	runCmd := exec.CommandContext(ctx, nodeBin, args...)
	if out, err := runCmd.CombinedOutput(); err != nil {
		os.RemoveAll(tmpDir)
		return nil, fmt.Errorf("tsingest: run: %w\n%s", err, out)
	}

	return &ExtractResult{SurfacePath: surfacePath, TempDir: tmpDir}, nil
}
