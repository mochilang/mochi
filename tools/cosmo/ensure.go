package cosmo

import (
	"fmt"
	"os"
	"path/filepath"
)

// EnsureCosmo verifies that the static Cosmopolitan library is installed.
// The library is expected at $COSMO_LIB or tools/cosmo/cosmo/libcosmo.a.

func EnsureCosmo() error {
	path := os.Getenv("COSMO_LIB")
	if path == "" {
		path = filepath.Join("tools", "cosmo", "cosmo", "libcosmo.a")
	}
	if _, err := os.Stat(path); err != nil {
		return fmt.Errorf("cosmopolitan library not found: %s", path)
	}
	return nil
}
