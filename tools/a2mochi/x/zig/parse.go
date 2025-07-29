//go:build slow

package zig

import (
	"fmt"
	"os"
	"strings"
)

// Program is a very small wrapper around the Zig source code to convert.
// The original implementation relied on the Zig compiler to produce a JSON AST
// but the toolchain in many environments does not provide the `zig` binary.
// For the simplified converter used in tests we only keep the source text.
type Program struct {
	Src string
}

// Parse returns a Program for the given Zig source. It does not perform any
// syntactic checks because the small subset of Zig supported by the converter
// can be processed using simple textual rules.
func Parse(src string) (*Program, error) {
	src = strings.TrimPrefix(src, "\ufeff")
	if strings.TrimSpace(src) == "" {
		return nil, fmt.Errorf("empty source")
	}
	return &Program{Src: src}, nil
}

// ParseFile reads the file at path and returns a Program.
func ParseFile(path string) (*Program, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Parse(string(data))
}
