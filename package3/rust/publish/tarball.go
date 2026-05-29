package publish

import (
	"archive/tar"
	"bytes"
	"compress/gzip"
	"fmt"
	"sort"
	"strings"
	"time"

	"mochi/package3/rust/library"
)

// Tarball renders a Cargo-compatible .crate tarball from a
// library.Files map. The .crate format is a gzipped tar where every
// entry lives under `<crate>-<version>/`, matching `cargo package`'s
// output bit-for-bit (modulo the file modes Cargo chooses).
//
// The implementation is byte-stable: the same input always produces
// byte-identical output. To achieve this:
//
//   - File entries are sorted alphabetically by path.
//   - All mtimes are pinned to the Unix epoch (matching cargo's
//     SOURCE_DATE_EPOCH=0 reproducible mode).
//   - File modes are pinned to 0644.
//   - The gzip header uses level 6 (cargo's default) with no
//     filename / mtime / OS bytes.
//
// Returns the gzipped tarball bytes ready for upload.
func Tarball(crateName, version string, files library.Files) ([]byte, error) {
	if strings.TrimSpace(crateName) == "" {
		return nil, PublishError{Reason: "Tarball: empty crateName"}
	}
	if strings.TrimSpace(version) == "" {
		return nil, PublishError{Reason: "Tarball: empty version"}
	}
	if len(files) == 0 {
		return nil, PublishError{Reason: "Tarball: empty Files"}
	}
	prefix := fmt.Sprintf("%s-%s/", crateName, version)

	paths := make([]string, 0, len(files))
	for p := range files {
		paths = append(paths, p)
	}
	sort.Strings(paths)

	var tarBuf bytes.Buffer
	tw := tar.NewWriter(&tarBuf)
	for _, path := range paths {
		content := files[path]
		hdr := &tar.Header{
			Name:     prefix + path,
			Size:     int64(len(content)),
			Mode:     0644,
			Typeflag: tar.TypeReg,
			ModTime:  time.Unix(0, 0).UTC(),
			Format:   tar.FormatUSTAR,
		}
		if err := tw.WriteHeader(hdr); err != nil {
			return nil, fmt.Errorf("publish: tar header for %q: %w", path, err)
		}
		if _, err := tw.Write([]byte(content)); err != nil {
			return nil, fmt.Errorf("publish: tar body for %q: %w", path, err)
		}
	}
	if err := tw.Close(); err != nil {
		return nil, fmt.Errorf("publish: tar close: %w", err)
	}

	var gzBuf bytes.Buffer
	gz, err := gzip.NewWriterLevel(&gzBuf, gzip.DefaultCompression)
	if err != nil {
		return nil, fmt.Errorf("publish: gzip writer: %w", err)
	}
	gz.Name = ""
	gz.ModTime = time.Time{}
	if _, err := gz.Write(tarBuf.Bytes()); err != nil {
		return nil, fmt.Errorf("publish: gzip write: %w", err)
	}
	if err := gz.Close(); err != nil {
		return nil, fmt.Errorf("publish: gzip close: %w", err)
	}
	return gzBuf.Bytes(), nil
}

// ExtractTarball is the inverse of Tarball. It returns the path-to-
// contents map a fresh consumer would see by unpacking the .crate.
// Used by callers verifying the round-trip property and by tests.
//
// The returned paths strip the `<crate>-<version>/` prefix, matching
// the library.Files shape Tarball consumed.
func ExtractTarball(crateBytes []byte) (library.Files, error) {
	gz, err := gzip.NewReader(bytes.NewReader(crateBytes))
	if err != nil {
		return nil, fmt.Errorf("publish: gunzip: %w", err)
	}
	defer gz.Close()
	tr := tar.NewReader(gz)
	out := library.Files{}
	for {
		hdr, err := tr.Next()
		if err != nil {
			break
		}
		buf := make([]byte, hdr.Size)
		if _, err := readFull(tr, buf); err != nil {
			return nil, fmt.Errorf("publish: read %q: %w", hdr.Name, err)
		}
		// strip leading <crate>-<version>/ component
		name := hdr.Name
		if i := strings.IndexByte(name, '/'); i >= 0 {
			name = name[i+1:]
		}
		if name == "" {
			continue
		}
		out[name] = string(buf)
	}
	return out, nil
}

// readFull reads exactly len(buf) bytes or returns an error.
func readFull(r interface{ Read(p []byte) (int, error) }, buf []byte) (int, error) {
	n := 0
	for n < len(buf) {
		k, err := r.Read(buf[n:])
		n += k
		if err != nil {
			if n == len(buf) {
				return n, nil
			}
			return n, err
		}
	}
	return n, nil
}
