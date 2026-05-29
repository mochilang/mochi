// Package build is the Mochi-to-Python transpiler pipeline driver.
//
// Entry point: Driver.Build(src, out string, target Target) error.
//
// Phase 1 supports TargetPythonSource: produces a `src/<pkg>/` tree
// with __init__.py, __main__.py, generated/<module>.py and a minimal
// pyproject.toml. Phase 15 adds TargetPythonWheel and TargetPythonSdist.
package build
