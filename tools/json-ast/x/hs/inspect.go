//go:build slow

package hs

import (
	hsparse "mochi/tools/a2mochi/x/hs"
)

// Program represents a parsed Haskell source file.
type Program struct {
	Items []hsparse.Item `json:"items"`
}

// Inspect parses the provided Haskell source code using the official parser
// and returns a Program describing its structure.
func Inspect(src string) (*Program, error) {
	prog, err := hsparse.Parse(src)
	if err != nil {
		return nil, err
	}
	return &Program{Items: prog.Items}, nil
}
