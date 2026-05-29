package build

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
)

// Phase 17.0: build a Jupyter kernelspec directory under outDir.
//
// Layout:
//
//	<outDir>/kernels/mochi-<pkg>/
//	  kernel.json     -- argv pointing at `python -m mochi_runtime.kernel`
//	  logo-32x32.png  -- placeholder logo (1x1 PNG; real assets in Phase 17.5)
//	  logo-64x64.png  -- placeholder logo
//
// The user package source + bundled runtime are also copied next to
// the kernelspec dir so a tarball-style ship is self-contained:
//
//	<outDir>/
//	  kernels/mochi-<pkg>/...
//	  src/<pkg>/...
//	  src/mochi_runtime/...
//	  pyproject.toml
//
// `jupyter kernelspec install --user <outDir>/kernels/mochi-<pkg>`
// registers the kernel in the user's Jupyter config; the kernel
// launches `python -m mochi_runtime.kernel` and intercepts every
// cell via MochiKernel.do_execute.

// buildIpykernel emits the Phase 17.0 kernelspec directory plus a
// self-contained package tree under outDir. Returns the path to the
// emitted kernels/mochi-<pkg>/ directory.
func buildIpykernel(outDir, workDir, rtDir, pkgName string) (string, error) {
	kernelDir := filepath.Join(outDir, "kernels", "mochi-"+pkgName)
	if err := os.MkdirAll(kernelDir, 0o755); err != nil {
		return "", err
	}
	kernelJSON, err := renderKernelJSON(pkgName)
	if err != nil {
		return "", err
	}
	if err := os.WriteFile(filepath.Join(kernelDir, "kernel.json"), kernelJSON, 0o644); err != nil {
		return "", err
	}
	// Logos: ship a 1x1 transparent PNG placeholder; Phase 17.5
	// swaps real artwork in without touching the emit path.
	for _, name := range []string{"logo-32x32.png", "logo-64x64.png"} {
		if err := os.WriteFile(filepath.Join(kernelDir, name), placeholderPNG(), 0o644); err != nil {
			return "", err
		}
	}

	// Copy the user package + bundled runtime alongside the
	// kernelspec so the emitted dist directory is self-contained.
	if err := copyTreeFiltered(filepath.Join(outDir, "src", pkgName), filepath.Join(workDir, "src", pkgName), nil); err != nil {
		return "", fmt.Errorf("copy user package: %w", err)
	}
	externs := filepath.Join(workDir, "src", pkgName+"_externs.py")
	if _, err := os.Stat(externs); err == nil {
		if err := copyFile(filepath.Join(outDir, "src", pkgName+"_externs.py"), externs); err != nil {
			return "", fmt.Errorf("copy externs sidecar: %w", err)
		}
	}
	if err := copyTreeFiltered(filepath.Join(outDir, "src", "mochi_runtime"), filepath.Join(rtDir, "mochi_runtime"), pyOnlyFilter); err != nil {
		return "", fmt.Errorf("copy mochi_runtime: %w", err)
	}
	if err := copyFile(filepath.Join(outDir, "pyproject.toml"), filepath.Join(workDir, "pyproject.toml")); err != nil {
		return "", err
	}

	return kernelDir, nil
}

// renderKernelJSON returns the Jupyter kernel.json shape per the
// Jupyter Client docs (https://jupyter-client.readthedocs.io/en/stable/kernels.html).
// The `{python}` placeholder is resolved by jupyter_client to the
// absolute path of the Python interpreter that registered the
// kernel; this keeps the kernel hermetic with the project's venv.
func renderKernelJSON(pkgName string) ([]byte, error) {
	spec := map[string]any{
		"argv": []string{
			"{python}",
			"-m",
			"mochi_runtime.kernel",
			"-f",
			"{connection_file}",
		},
		"display_name":   "Mochi (" + pkgName + ")",
		"language":       "mochi",
		"interrupt_mode": "signal",
		"metadata": map[string]any{
			"mochi_version":      "0.1.0",
			"transpiler_version": "MEP-51",
			"python_version":     ">=3.12",
		},
	}
	return json.MarshalIndent(spec, "", "  ")
}

// placeholderPNG returns a 1x1 transparent PNG. Used for the logo
// slots until Phase 17.5 ships real artwork.
//
// Bytes derived from a hand-crafted minimal PNG: signature + IHDR
// declaring a 1x1 RGBA image + IDAT containing a zlib-deflated
// single transparent pixel + IEND. Constant on the call site so the
// reproducible-build gate continues to hold.
func placeholderPNG() []byte {
	return []byte{
		0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, // PNG signature
		0x00, 0x00, 0x00, 0x0d, // IHDR length
		0x49, 0x48, 0x44, 0x52, // IHDR
		0x00, 0x00, 0x00, 0x01, // width 1
		0x00, 0x00, 0x00, 0x01, // height 1
		0x08, 0x06, 0x00, 0x00, 0x00, // 8-bit RGBA, no interlace
		0x1f, 0x15, 0xc4, 0x89, // IHDR CRC
		0x00, 0x00, 0x00, 0x0d, // IDAT length
		0x49, 0x44, 0x41, 0x54, // IDAT
		0x78, 0x9c, 0x62, 0x00, 0x01, 0x00, 0x00, 0x05, 0x00, 0x01,
		0x0d, 0x0a, 0x2d, 0xb4, // zlib block + adler32
		0x12, 0x05, 0xfa, 0x9b, // IDAT CRC
		0x00, 0x00, 0x00, 0x00, // IEND length
		0x49, 0x45, 0x4e, 0x44, // IEND
		0xae, 0x42, 0x60, 0x82, // IEND CRC
	}
}
