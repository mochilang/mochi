//go:build cosmo && !libcosmo

package cosmo

import "errors"

// ErrCosmoUnavailable is returned when the cosmopolitan static library is missing.
var ErrCosmoUnavailable = errors.New("Cosmopolitan compiler not available; build with tags 'cosmo libcosmo'")

func CompileAndRun(code string) (string, error) { return "", ErrCosmoUnavailable }
func CompileToFile(code, out string) error      { return ErrCosmoUnavailable }
