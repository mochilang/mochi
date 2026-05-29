package metadata

import (
	"archive/zip"
	"errors"
	"fmt"
	"io"
	"strings"

	bridgeerrors "github.com/mochilang/mochi/package3/kotlin/errors"
)

// IngestJAR opens a JAR file (which is a ZIP archive) at jarPath, iterates all
// .class entries, extracts @kotlin.Metadata from each, and returns the top-level
// APIObjects (nested classes are embedded in their enclosing class, not returned
// as top-level entries). Entries without @kotlin.Metadata are silently skipped.
func IngestJAR(jarPath string) ([]*APIObject, error) {
	zr, err := zip.OpenReader(jarPath)
	if err != nil {
		return nil, fmt.Errorf("ingest: open JAR %q: %w", jarPath, err)
	}
	defer zr.Close()

	return ingestZipReader(&zr.Reader)
}

// IngestJARBytes reads a JAR from an in-memory byte slice. Useful for testing.
func IngestJARBytes(data []byte) ([]*APIObject, error) {
	zr, err := zip.NewReader(readerAt(data), int64(len(data)))
	if err != nil {
		return nil, fmt.Errorf("ingest: parse JAR bytes: %w", err)
	}
	return ingestZipReader(zr)
}

// ingestZipReader processes all .class entries in a zip.Reader.
func ingestZipReader(zr *zip.Reader) ([]*APIObject, error) {
	var results []*APIObject

	for _, f := range zr.File {
		if !strings.HasSuffix(f.Name, ".class") {
			continue
		}
		// Skip inner/nested classes (they appear as Outer$Inner.class)
		baseName := f.Name
		if idx := strings.LastIndex(baseName, "/"); idx >= 0 {
			baseName = baseName[idx+1:]
		}
		if strings.Contains(baseName, "$") {
			continue
		}

		obj, err := processClassEntry(f)
		if err != nil {
			if errors.Is(err, bridgeerrors.ErrNoKotlinMetadata) {
				continue
			}
			// On other errors (malformed class), skip with a warning; do not fail the whole JAR.
			continue
		}
		if obj != nil {
			results = append(results, obj)
		}
	}

	return results, nil
}

// processClassEntry reads one zip.File entry and returns its APIObject.
func processClassEntry(f *zip.File) (*APIObject, error) {
	rc, err := f.Open()
	if err != nil {
		return nil, fmt.Errorf("ingest: open entry %q: %w", f.Name, err)
	}
	defer rc.Close()

	classBytes, err := io.ReadAll(rc)
	if err != nil {
		return nil, fmt.Errorf("ingest: read entry %q: %w", f.Name, err)
	}

	raw, err := ExtractMetadata(classBytes)
	if err != nil {
		return nil, err
	}

	switch raw.Kind {
	case 1: // class
		return DecodeClass(raw)
	case 2: // file facade
		return DecodePackage(raw)
	case 3, 4: // synthetic class, multi-file-part — skip
		return nil, nil
	case 5: // multi-file-facade
		obj := &APIObject{
			Kind:                  ClassKindFileFacade,
			MetadataSchemaVersion: raw.Version,
		}
		if raw.XS != "" {
			obj.ClassName = strings.ReplaceAll(raw.XS, "/", ".")
			obj.JVMClassName = raw.XS
		}
		return obj, nil
	default:
		return nil, nil
	}
}

// readerAt wraps a []byte to implement io.ReaderAt.
type readerAt []byte

func (r readerAt) ReadAt(p []byte, off int64) (int, error) {
	if off >= int64(len(r)) {
		return 0, io.EOF
	}
	n := copy(p, r[off:])
	if n < len(p) {
		return n, io.EOF
	}
	return n, nil
}
