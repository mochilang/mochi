package lockfile

import (
	"crypto/sha256"
	"crypto/sha512"
	"encoding/hex"
	"sort"
)

// SHA256Hex returns the lowercase hex-encoded SHA-256 of data.
func SHA256Hex(data []byte) string {
	h := sha256.Sum256(data)
	return hex.EncodeToString(h[:])
}

// SHA512Hex returns the lowercase hex-encoded SHA-512 of data.
func SHA512Hex(data []byte) string {
	h := sha512.Sum512(data)
	return hex.EncodeToString(h[:])
}

// ComputeBeamIngestSHA256 hashes the sorted concatenation of raw ETF bytes
// extracted from Dbgi/Abst chunks across all .beam files in a package.
// modules maps module-name → raw ETF bytes (the bytes after the FOR1/IFF
// chunk header, i.e. the raw payload of the Dbgi or Abst chunk). The map
// is sorted by module name before concatenating so the hash is deterministic
// regardless of filesystem traversal order.
func ComputeBeamIngestSHA256(modules map[string][]byte) string {
	names := make([]string, 0, len(modules))
	for name := range modules {
		names = append(names, name)
	}
	sort.Strings(names)

	h := sha256.New()
	for _, name := range names {
		h.Write(modules[name])
	}
	return hex.EncodeToString(h.Sum(nil))
}

// ComputeShimSHA256 returns the SHA-256 of the shim.erl source content.
func ComputeShimSHA256(shimContent string) string {
	return SHA256Hex([]byte(shimContent))
}
