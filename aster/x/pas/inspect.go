package pas

import (
	pasparser "github.com/akrennmair/pascal/parser"
	"strings"
)

// Program represents a parsed Pascal source file.
type Program struct {
	Root *ProgramNode `json:"root"`
}

// Inspect parses Pascal source code using the official Pascal parser.
// Positions are omitted from the resulting AST.
func Inspect(src string) (*Program, error) {
	return InspectWithOption(src, Option{})
}

// InspectWithOption behaves like Inspect but allows callers to request
// positional information in the resulting AST. The underlying parser
// does not provide positions so they are currently unavailable.
func InspectWithOption(src string, opt Option) (*Program, error) {
	src = strings.TrimSpace(src)
	ast, err := pasparser.Parse("input.pas", src)
	if err != nil {
		return nil, err
	}
	root := convertProgram(ast, opt)
	return &Program{Root: (*ProgramNode)(root)}, nil
}
