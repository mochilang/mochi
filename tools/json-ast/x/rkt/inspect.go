//go:build slow

package rkt

import (
	rktparse "mochi/tools/a2mochi/x/rkt"
)

// Program represents a parsed Racket source file.
type Program struct {
	Forms []rktparse.Form `json:"forms"`
}

// Inspect parses the provided Racket source code and returns a Program
// describing its structure using the official Racket parser.
func Inspect(src string) (*Program, error) {
	prog, err := rktparse.Parse(src)
	if err != nil {
		return nil, err
	}
	return &Program{Forms: prog.Forms}, nil
}
