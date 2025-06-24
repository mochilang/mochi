package data

import (
	"io"
	"os"
	"path/filepath"
	"strings"
)

// openReader opens path for reading. A nil closer means no need to close.
func openReader(path string) (io.Reader, func() error, error) {
	if path == "" || path == "-" {
		return os.Stdin, func() error { return nil }, nil
	}
	f, err := os.Open(path)
	if err == nil {
		return f, f.Close, nil
	}
	if root := os.Getenv("MOCHI_ROOT"); root != "" && !filepath.IsAbs(path) {
		clean := path
		for strings.HasPrefix(clean, "../") {
			clean = strings.TrimPrefix(clean, "../")
		}
		alt := filepath.Join(root, clean)
		if af, err2 := os.Open(alt); err2 == nil {
			return af, af.Close, nil
		}
	}
	return nil, nil, err
}

type nopWriteCloser struct{ io.Writer }

func (nopWriteCloser) Close() error { return nil }

// openWriter opens path for writing.
func openWriter(path string) (io.Writer, func() error, error) {
	if path == "" || path == "-" {
		w := nopWriteCloser{os.Stdout}
		return w, func() error { return nil }, nil
	}
	f, err := os.Create(path)
	if err == nil {
		return f, f.Close, nil
	}
	if root := os.Getenv("MOCHI_ROOT"); root != "" && !filepath.IsAbs(path) {
		clean := path
		for strings.HasPrefix(clean, "../") {
			clean = strings.TrimPrefix(clean, "../")
		}
		alt := filepath.Join(root, clean)
		af, err2 := os.Create(alt)
		if err2 == nil {
			return af, af.Close, nil
		}
	}
	return nil, nil, err
}

// truthy evaluates the truthiness of a value following Mochi semantics.
func truthy(val any) bool {
	switch v := val.(type) {
	case nil:
		return false
	case bool:
		return v
	case int:
		return v != 0
	case int64:
		return v != 0
	case float64:
		return v != 0
	case string:
		return v != ""
	case []any:
		return len(v) > 0
	case map[string]any:
		return len(v) > 0
	case *Group:
		return len(v.Items) > 0
	default:
		return true
	}
}
