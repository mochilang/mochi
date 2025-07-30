package pas

import (
	pasparser "github.com/akrennmair/pascal/parser"
	"strings"
)

// Program represents a parsed Pascal source file.
type Program struct {
	AST   *pasparser.AST `json:"ast"`
	Lines []string       `json:"lines"`
}

// Inspect parses Pascal source code using the official pascal parser.
func Inspect(src string) (*Program, error) {
	src = strings.TrimSpace(src)
	ast, err := pasparser.Parse("input.pas", src)
	if err != nil {
		return nil, err
	}
	lines := strings.Split(strings.ReplaceAll(src, "\r\n", "\n"), "\n")
	return &Program{AST: ast, Lines: lines}, nil
}
