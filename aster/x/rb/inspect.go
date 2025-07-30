package rb

// Program represents a parsed Ruby source file.
//
// The structure mirrors the Node type produced by this package's Ruby parser
// which offers a typed representation of Ruby's AST compared to the generic
// S-expression returned by ripper.
// Program wraps the root Node returned by the parser.
type Program struct {
	Root *Node `json:"root"`
}

// Inspect parses the given Ruby source code and returns a Program describing
// its AST. It relies on the embedded Ruby interpreter via ripper.
func Inspect(src string) (*Program, error) {
	node, err := Parse(src)
	if err != nil {
		return nil, err
	}
	return &Program{Root: node}, nil
}
