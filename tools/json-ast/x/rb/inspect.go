package rb

// Program represents a parsed Ruby source file.
//
// The structure mirrors the Node type produced by this package's Ruby parser
// which offers a typed representation of Ruby's AST compared to the generic
// S-expression returned by ripper.
type Program struct {
	AST *Node `json:"ast"`
}

// Inspect parses the given Ruby source code and returns a Program describing
// its AST. It relies on the embedded Ruby interpreter via ripper.
func Inspect(src string) (*Program, error) {
	node, err := Parse(src)
	if err != nil {
		return nil, err
	}
	// The root node stores the full source in the Source field which is not
	// needed in the JSON output. Clear it to reduce noise.
	node.Source = ""
	return &Program{AST: node}, nil
}
