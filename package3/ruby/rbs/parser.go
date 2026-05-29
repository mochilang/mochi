package rbs

import (
	"archive/tar"
	"compress/gzip"
	"io"
	"io/fs"
	"path/filepath"
	"strings"
)

// ParseFromTarball extracts and parses all .rbs files from an unpacked gem
// tarball directory (or a tar.gz reader). It returns a GemSurface with
// SourceBundled if any .rbs files are found, or an empty surface with
// SourceNone if the gem ships no RBS.
//
// The tarball is the data.tar.gz sub-archive inside the .gem file.
func ParseFromTarball(gemName, gemVersion string, r io.Reader) (*GemSurface, error) {
	gz, err := gzip.NewReader(r)
	if err != nil {
		return nil, err
	}
	defer gz.Close()

	surface := &GemSurface{
		Gem:     gemName,
		Version: gemVersion,
		Source:  SourceNone,
	}

	tr := tar.NewReader(gz)
	var rbsFiles []string
	var rbsContents []string
	for {
		hdr, err := tr.Next()
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, err
		}
		if hdr.Typeflag != tar.TypeReg {
			continue
		}
		// RBS files live under sig/ or spec/sig/ by convention.
		clean := filepath.Clean(hdr.Name)
		if filepath.Ext(clean) != ".rbs" {
			continue
		}
		dir := filepath.ToSlash(filepath.Dir(clean))
		if !strings.HasPrefix(dir, "sig") && !strings.Contains(dir, "/sig") {
			continue
		}
		data, err := io.ReadAll(tr)
		if err != nil {
			return nil, err
		}
		rbsFiles = append(rbsFiles, clean)
		rbsContents = append(rbsContents, string(data))
	}

	if len(rbsFiles) == 0 {
		return surface, nil
	}

	surface.Source = SourceBundled
	classes, err := parseRBSTexts(rbsContents)
	if err != nil {
		return nil, err
	}
	surface.Classes = classes
	return surface, nil
}

// ParseFromDir parses .rbs files from a directory on disk (used for
// gem_rbs_collection sigs after they are downloaded and extracted).
func ParseFromDir(gemName, gemVersion, dir string) (*GemSurface, error) {
	surface := &GemSurface{
		Gem:     gemName,
		Version: gemVersion,
		Source:  SourceGemRBSCollection,
	}
	var contents []string
	err := filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || filepath.Ext(path) != ".rbs" {
			return nil
		}
		data, err := io.ReadAll(openFile(path))
		if err != nil {
			return err
		}
		contents = append(contents, string(data))
		return nil
	})
	if err != nil {
		return nil, err
	}
	if len(contents) == 0 {
		surface.Source = SourceNone
		return surface, nil
	}
	classes, err := parseRBSTexts(contents)
	if err != nil {
		return nil, err
	}
	surface.Classes = classes
	return surface, nil
}

// parseRBSTexts is the lightweight Go RBS parser (~400 LOC target). The
// full implementation is phase 2; this stub returns an empty slice so the
// package compiles and the type system is exercisable in unit tests.
func parseRBSTexts(_ []string) ([]ClassDecl, error) {
	// Phase 2 implementation: walk RBS grammar subset covering
	// class/module declarations, method declarations (def name: type),
	// attribute declarations (attr_reader, attr_accessor, attr_writer),
	// and type aliases. Items the parser does not recognise are silently
	// skipped with a SkipUnknown entry in the SkipReport.
	return nil, nil
}

// openFile is a thin wrapper so ParseFromDir is testable without os import.
var openFile = func(path string) io.ReadCloser {
	// Implemented in parse_os.go to avoid os import in this file.
	panic("openFile not wired")
}
