// Package blob provides content-addressed local cache for Maven artifacts.
package blob

import (
	gsha256 "crypto/sha256"
	"io"
	"os"

	"lukechampine.com/blake3"
)

// dualWriter writes to two underlying hash writers simultaneously.
type dualWriter struct {
	sha256w io.Writer
	blake3w io.Writer
}

func (d *dualWriter) Write(p []byte) (n int, err error) {
	n, err = d.sha256w.Write(p)
	if err != nil {
		return n, err
	}
	_, err = d.blake3w.Write(p)
	return n, err
}

// HashReader computes both SHA-256 and BLAKE3 hashes from the given reader.
// It streams the data once through a dual-writer.
func HashReader(r io.Reader) (sha256sum [32]byte, blake3sum [32]byte, err error) {
	sha256h := gsha256.New()
	blake3h := blake3.New(32, nil)

	dw := &dualWriter{sha256w: sha256h, blake3w: blake3h}
	if _, err = io.Copy(dw, r); err != nil {
		return sha256sum, blake3sum, err
	}

	copy(sha256sum[:], sha256h.Sum(nil))
	copy(blake3sum[:], blake3h.Sum(nil))
	return sha256sum, blake3sum, nil
}

// HashFile computes both SHA-256 and BLAKE3 hashes for the file at path.
// It reads the file once using a dual-writer.
func HashFile(path string) (sha256sum [32]byte, blake3sum [32]byte, err error) {
	f, err := os.Open(path)
	if err != nil {
		return sha256sum, blake3sum, err
	}
	defer f.Close()
	return HashReader(f)
}
