//go:build !cosmo

package cosmo

import "errors"

// CompileAndRun is unavailable when Cosmopolitan is not enabled.
func CompileAndRun(code string) (string, error) {
	return "", ErrCosmoUnavailable
}

// CompileToFile is unavailable when cosmocc is missing.
func CompileToFile(code, output string) error {
	return ErrCosmoUnavailable
}

// ErrCosmoUnavailable indicates the Cosmo toolchain was not found.
var ErrCosmoUnavailable = errors.New("Cosmopolitan not available; build with tag 'cosmo'")
