//go:build slow

package zig

import (
	"fmt"
	"os"

	"mochi/ast"
)

// ConvertSource converts Zig source code to Mochi source. The current
// implementation is a stub and always returns an error.
func ConvertSource(src string) (string, error) {
	return "", fmt.Errorf("zig conversion not implemented")
}

// Convert converts Zig source code into a Mochi AST node. This is currently
// unimplemented and returns an error.
func Convert(src string) (*ast.Node, error) {
	return nil, fmt.Errorf("zig conversion not implemented")
}

// ConvertFile reads the given file and calls Convert on its contents.
func ConvertFile(path string) (*ast.Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

// ConvertFileSource reads the file and returns the Mochi source string.
func ConvertFileSource(path string) (string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	return ConvertSource(string(data))
}
