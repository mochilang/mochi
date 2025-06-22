//go:build tcc && !libtcc

package tcc

import "errors"

// CompileAndRun is a stub when TinyCC is not available.
func CompileAndRun(code string) (int, error) {
	return 0, ErrTCCUnavailable
}

// CompileToFile is unavailable when TinyCC headers are missing.
func CompileToFile(code, out string) error {
	return ErrTCCUnavailable
}

// ErrTCCUnavailable is returned when TinyCC headers or library are missing.
var ErrTCCUnavailable = errors.New("TinyCC not available; build with tags 'tcc libtcc'")
