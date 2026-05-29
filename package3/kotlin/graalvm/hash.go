package graalvm

import (
	"crypto/sha256"
	"fmt"
	"io"
	"os"
)

// HashLibrary computes the SHA-256 of the compiled shared library.
func HashLibrary(soPath string) ([32]byte, error) {
	f, err := os.Open(soPath)
	if err != nil {
		return [32]byte{}, fmt.Errorf("graalvm: open library %q: %w", soPath, err)
	}
	defer f.Close()

	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		return [32]byte{}, fmt.Errorf("graalvm: hash library %q: %w", soPath, err)
	}

	var result [32]byte
	copy(result[:], h.Sum(nil))
	return result, nil
}
