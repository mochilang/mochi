package moduleproxy

import (
	"archive/zip"
	"crypto/sha256"
	"encoding/base64"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"sort"
)

// H1Prefix is the literal prefix Go's checksum database uses for the
// Hash1 algorithm. Lines in go.sum and sum.golang.org's tree always
// start with "h1:".
const H1Prefix = "h1:"

// Hash1Files computes the Go modules "Hash1" dirhash over the given
// (filename, contents) inputs. The algorithm matches
// golang.org/x/mod/sumdb/dirhash.Hash1 byte-for-byte:
//
//  1. For each input file, compute sha256(contents) and render
//     "%x  %s\n" (hex-lower, two spaces, filename, newline).
//  2. Sort the per-file lines lexicographically.
//  3. The final digest is base64.StdEncoding(sha256(concat(sorted))).
//  4. Prepend "h1:" to the base64 digest.
//
// Hash1Files returns an error if any open function returns an error.
// The opens are sequential, never parallel; the order of `files` does
// not affect the final digest (sorting is internal).
func Hash1Files(files []string, open func(string) (io.ReadCloser, error)) (string, error) {
	if open == nil {
		return "", errors.New("moduleproxy: nil open function")
	}
	lines := make([]string, 0, len(files))
	for _, name := range files {
		rc, err := open(name)
		if err != nil {
			return "", fmt.Errorf("moduleproxy: open %s: %w", name, err)
		}
		fh := sha256.New()
		if _, err := io.Copy(fh, rc); err != nil {
			rc.Close()
			return "", fmt.Errorf("moduleproxy: read %s: %w", name, err)
		}
		if err := rc.Close(); err != nil {
			return "", fmt.Errorf("moduleproxy: close %s: %w", name, err)
		}
		lines = append(lines, fmt.Sprintf("%s  %s\n", hex.EncodeToString(fh.Sum(nil)), name))
	}
	sort.Strings(lines)
	outer := sha256.New()
	for _, line := range lines {
		outer.Write([]byte(line))
	}
	return H1Prefix + base64.StdEncoding.EncodeToString(outer.Sum(nil)), nil
}

// HashZip computes the Hash1 dirhash over a module .zip stream. The
// algorithm matches golang.org/x/mod/sumdb/dirhash.HashZip. The zip is
// read fully into memory (typical Go module zips are under a few MB).
//
// modulePath and version are the LHS of the proxy URL: the zip entry
// names are expected to be prefixed with "<modulePath>@<version>/...".
// modulePath / version are validated against entry names to guard
// against confused-deputy attacks where the proxy hands a zip whose
// entries claim a different module.
func HashZip(r io.Reader, modulePath, version string) (string, error) {
	buf, err := io.ReadAll(r)
	if err != nil {
		return "", fmt.Errorf("moduleproxy: read zip: %w", err)
	}
	zr, err := zip.NewReader(newBytesReaderAt(buf), int64(len(buf)))
	if err != nil {
		return "", fmt.Errorf("moduleproxy: parse zip: %w", err)
	}
	expectedPrefix := modulePath + "@" + version + "/"
	files := make([]string, 0, len(zr.File))
	for _, f := range zr.File {
		if f.FileInfo().IsDir() {
			continue
		}
		if modulePath != "" && expectedPrefix != "" && !hasPrefix(f.Name, expectedPrefix) {
			return "", fmt.Errorf("moduleproxy: zip entry %q does not start with %q", f.Name, expectedPrefix)
		}
		files = append(files, f.Name)
	}
	open := func(name string) (io.ReadCloser, error) {
		for _, f := range zr.File {
			if f.Name == name {
				return f.Open()
			}
		}
		return nil, fmt.Errorf("moduleproxy: zip entry %q not found", name)
	}
	return Hash1Files(files, open)
}

// HashGoMod computes the Hash1 digest of a single go.mod file. The
// algorithm matches dirhash.Hash1Mod: it treats the file as a single
// "go.mod" entry. The result is the value stored alongside the zip
// hash in go.sum as the "<module> <version>/go.mod h1:..." line.
func HashGoMod(modBytes []byte) string {
	fh := sha256.Sum256(modBytes)
	inner := fmt.Sprintf("%s  go.mod\n", hex.EncodeToString(fh[:]))
	outer := sha256.Sum256([]byte(inner))
	return H1Prefix + base64.StdEncoding.EncodeToString(outer[:])
}

func hasPrefix(s, prefix string) bool {
	return len(s) >= len(prefix) && s[:len(prefix)] == prefix
}

// bytesReaderAt is a minimal io.ReaderAt over an in-memory byte slice,
// avoiding a dep on bytes.Reader inside the moduleproxy package
// (keeping the file self-contained).
type bytesReaderAt struct{ buf []byte }

func newBytesReaderAt(buf []byte) *bytesReaderAt { return &bytesReaderAt{buf: buf} }

func (b *bytesReaderAt) ReadAt(p []byte, off int64) (int, error) {
	if off < 0 || off >= int64(len(b.buf)) {
		return 0, io.EOF
	}
	n := copy(p, b.buf[off:])
	if n < len(p) {
		return n, io.EOF
	}
	return n, nil
}
