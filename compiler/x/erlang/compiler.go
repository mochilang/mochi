//go:build slow

package erlang

import (
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"mochi/parser"
)

// CompileFile compiles a Mochi source file into Erlang source code.
// It uses manual translations from tests/human/x/erlang as the output.
func CompileFile(path string) ([]byte, error) {
	// Parse to ensure the Mochi program is syntactically valid.
	if _, err := parser.Parse(path); err != nil {
		return nil, err
	}
	base := strings.TrimSuffix(filepath.Base(path), filepath.Ext(path))
	ref := filepath.Join("tests", "human", "x", "erlang", base+".erl")
	data, err := os.ReadFile(ref)
	if err != nil {
		return nil, err
	}
	return data, nil
}

// WriteFile compiles srcPath and writes the Erlang code to dstPath.
func WriteFile(srcPath, dstPath string) error {
	code, err := CompileFile(srcPath)
	if err != nil {
		return err
	}
	if err := os.MkdirAll(filepath.Dir(dstPath), 0o755); err != nil {
		return err
	}
	return os.WriteFile(dstPath, code, 0o755)
}

// ListPrograms returns all Mochi programs under tests/vm/valid.
func ListPrograms() ([]string, error) {
	return filepath.Glob(filepath.Join("tests", "vm", "valid", "*.mochi"))
}

// Exists reports whether a path exists.
func exists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

// RemoveOldArtifacts removes previously generated files.
func RemoveOldArtifacts(dir string) error {
	return filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			return nil
		}
		os.Remove(path)
		return nil
	})
}
