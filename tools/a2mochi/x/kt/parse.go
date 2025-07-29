//go:build slow

package kt

import (
	"os"
	"path/filepath"
	"strings"
)

// Program represents a Kotlin source file.
type Program struct {
	Name   string `json:"name"`
	Source string `json:"source"`
}

// ParseFile reads path and returns a Program describing it.
func ParseFile(path string) (*Program, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	name := strings.TrimSuffix(filepath.Base(path), filepath.Ext(path))
	return &Program{Name: name, Source: string(data)}, nil
}

// Parse wraps ParseFile for compatibility when only source is provided.
func Parse(src string) (*Program, error) {
	return &Program{Name: "", Source: src}, nil
}
