// Package emit writes a tstree.SourceFile to disk as a single
// .ts file. Entry point: Emit(file, outDir, baseName) → (path, error).
//
// Phase 1 ships a single file under outDir/<baseName>.ts. Phase 4
// (multi-file records) introduces a per-module file split; Phase
// 15 wires the dist/{node,deno,bun,browser}/ per-runtime layout
// driven by tsc --build.
package emit

import (
	"fmt"
	"os"
	"path/filepath"

	"mochi/transpiler3/typescript/tstree"
)

// Emit writes file to outDir/baseName.ts and returns the absolute
// path on disk. The parent directory is created if missing. The
// file is written with LF line endings and a single trailing
// newline (TsString already produces that shape).
func Emit(file *tstree.SourceFile, outDir, baseName string) (string, error) {
	if file == nil {
		return "", fmt.Errorf("ts emit: nil SourceFile")
	}
	if baseName == "" {
		return "", fmt.Errorf("ts emit: empty baseName")
	}
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return "", fmt.Errorf("ts emit: mkdir %s: %w", outDir, err)
	}
	abs, err := filepath.Abs(filepath.Join(outDir, baseName+".ts"))
	if err != nil {
		return "", fmt.Errorf("ts emit: abs path: %w", err)
	}
	src := file.TsString()
	if err := os.WriteFile(abs, []byte(src), 0o644); err != nil {
		return "", fmt.Errorf("ts emit: write %s: %w", abs, err)
	}
	return abs, nil
}
