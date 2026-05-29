package lockfile

import (
	"crypto/sha256"
	"encoding/hex"
)

// WrapperSHA256 returns the lowercase hex SHA-256 of the wrapper
// crate's rendered src/lib.rs. This is the hash the lockfile pins
// under `wrapper-sha256` per MEP-73 spec §3. The argument is the
// raw source string the bridge would write to disk; the function
// is the single canonical hashing point so callers and the drift
// checker observe the same digest.
//
// SHA-256 (rather than BLAKE3) is chosen here to match the cargo /
// crates.io ecosystem's preferred digest for source-level pins.
// The .crate tarball still uses dual BLAKE3 + SHA-256 hashing per
// MEP-57; the wrapper hash is single-algorithm because the wrapper
// crate is bridge-internal and never published.
func WrapperSHA256(libRS string) string {
	h := sha256.Sum256([]byte(libRS))
	return hex.EncodeToString(h[:])
}

// RustdocSHA256 returns the lowercase hex SHA-256 of a rustdoc JSON
// document. Surfaced as a sibling helper to WrapperSHA256 so all
// lockfile-bound hashing lives in one place.
func RustdocSHA256(rustdocJSON []byte) string {
	h := sha256.Sum256(rustdocJSON)
	return hex.EncodeToString(h[:])
}
