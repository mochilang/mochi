package rb

import (
	rubyparser "mochi/tools/a2mochi/x/ruby"
)

// Program represents a parsed Ruby source file.
//
// The structure mirrors the Node type from the a2mochi Ruby parser which
// provides a more typed representation of Ruby's AST compared to the generic
// S-expression returned by ripper.
type Program struct {
	AST *rubyparser.Node `json:"ast"`
}

// Inspect parses the given Ruby source code and returns a Program describing
// its AST. It relies on the a2mochi Ruby parser which internally uses the
// official "ripper" library.
func Inspect(src string) (*Program, error) {
	node, err := rubyparser.Parse(src)
	if err != nil {
		return nil, err
	}
	// The root node stores the full source in the Source field which is not
	// needed in the JSON output. Clear it to reduce noise.
	node.Source = ""
	return &Program{AST: node}, nil
}
