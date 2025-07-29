//go:build slow

package pas

import (
	pasparser "github.com/akrennmair/pascal/parser"
	"strings"
)

// Node represents a parsed Pascal program.
type Node struct {
	AST   *pasparser.AST
	Lines []string
}

// Parse parses Pascal source using the official pascal parser.
func Parse(src string) (*Node, error) {
	ast, err := pasparser.Parse("input.pas", src)
	if err != nil {
		return nil, err
	}
	lines := strings.Split(strings.ReplaceAll(src, "\r\n", "\n"), "\n")
	return &Node{AST: ast, Lines: lines}, nil
}
