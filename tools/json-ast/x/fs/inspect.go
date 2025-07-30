//go:build slow

package fs

import (
	a2fs "mochi/tools/a2mochi/x/fs"
)

// Program represents a parsed F# source file.
type Program struct {
	Vars   []a2fs.Var  `json:"vars"`
	Prints []string    `json:"prints"`
	Stmts  []a2fs.Stmt `json:"stmts"`
}

// Inspect parses F# source code using the official F# parser and returns its Program representation.
func Inspect(src string) (*Program, error) {
	p, err := a2fs.Parse(src)
	if err != nil {
		return nil, err
	}
	return &Program{
		Vars:   p.Vars,
		Prints: p.Prints,
		Stmts:  p.Stmts,
	}, nil
}
