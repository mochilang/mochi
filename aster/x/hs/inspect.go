//go:build slow

package hs

import (
	"strings"

	hsparse "mochi/tools/a2mochi/x/hs"
)

// Program represents a parsed Haskell source file.
type Program struct {
	Pragmas []string       `json:"pragmas,omitempty"`
	Imports []string       `json:"imports,omitempty"`
	Items   []hsparse.Item `json:"items"`
	Source  string         `json:"-"`
}

// Inspect parses the provided Haskell source code using the official parser
// and returns a Program describing its structure.
func Inspect(src string) (*Program, error) {
	prog, err := hsparse.Parse(src)
	if err != nil {
		return nil, err
	}
	lines := strings.Split(src, "\n")
	var pragmas []string
	var imports []string
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "{-#") {
			pragmas = append(pragmas, trimmed)
			continue
		}
		if strings.HasPrefix(trimmed, "import") {
			imports = append(imports, trimmed)
			continue
		}
		if trimmed == "" || strings.HasPrefix(trimmed, "--") {
			continue
		}
		break
	}
	return &Program{Pragmas: pragmas, Imports: imports, Items: prog.Items, Source: src}, nil
}
