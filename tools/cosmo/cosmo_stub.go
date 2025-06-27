//go:build !cosmo

package cosmo

import "errors"

// ErrCosmoUnavailable is returned when cosmocc isn't present.
var ErrCosmoUnavailable = errors.New("Cosmopolitan compiler not available; build with tags 'cosmo'")

func CompileAndRun(code string) (string, error) { return "", ErrCosmoUnavailable }
func CompileToFile(code, out string) error      { return ErrCosmoUnavailable }
