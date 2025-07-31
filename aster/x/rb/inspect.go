package rb

// Program represents a parsed Ruby source file. The AST is rooted at a
// ProgramNode which embeds Node. This mirrors the structure returned by the
// parser while providing a slightly richer type hierarchy for use by the
// printer and tests.
type Program struct {
	Root *ProgramNode `json:"root"`
}

// Inspect parses the given Ruby source code and returns a Program describing
// its AST. It relies on the embedded Ruby interpreter via ripper.
func Inspect(src string) (*Program, error) {
	node, err := Parse(src)
	if err != nil {
		return nil, err
	}
	if node == nil {
		return &Program{Root: nil}, nil
	}
	root := &ProgramNode{Node: *node}
	return &Program{Root: root}, nil
}
