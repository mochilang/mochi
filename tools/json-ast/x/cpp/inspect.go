//go:build slow

package cpp

import (
	a2cpp "mochi/tools/a2mochi/x/cpp"
)

// Program describes a C++ source file as parsed by clang++.
type Program struct {
	Funcs   []a2cpp.Func   `json:"funcs"`
	Enums   []a2cpp.Enum   `json:"enums"`
	Structs []a2cpp.Struct `json:"structs"`
	Globals []a2cpp.Global `json:"globals"`
	Source  string         `json:"source"`
}

// Inspect parses the provided C++ source code using clang++ and
// returns a Program describing its structure.
func Inspect(src string) (*Program, error) {
	p, err := a2cpp.Parse(src)
	if err != nil {
		return nil, err
	}
	return &Program{
		Funcs:   p.Funcs,
		Enums:   p.Enums,
		Structs: p.Structs,
		Globals: p.Globals,
		Source:  p.Src,
	}, nil
}
