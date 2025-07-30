package mochi

import (
	"mochi/parser"
)

// Program holds the parsed structure of a Mochi source file.
type Program struct {
	AST *parser.Program `json:"ast,omitempty"`
}

// Inspect parses Mochi source code using the official parser and returns
// its AST wrapped in a Program structure.
func Inspect(src string) (*Program, error) {
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	return &Program{AST: prog}, nil
}
