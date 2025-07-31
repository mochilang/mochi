package ruby

// Program represents a parsed Ruby source file.
// Program is the JSON representation returned by Inspect.
type Program struct {
	Root *ProgramNode `json:"root"`
}

// Inspect parses the given Ruby source code and returns a Program describing
// its AST using the bundled tree-sitter parser.
// Inspect parses the given Ruby source code and returns a Program describing
// its AST using the bundled tree-sitter parser. Positional information is
// omitted unless opts specifies IncludePositions.
// Inspect parses the given Ruby source code using tree-sitter and returns its
// Program representation. Positional information is omitted by default; pass an
// Options value with IncludePositions=true to retain it.
func Inspect(src string, opts ...Options) (*Program, error) {
	var opt Options
	if len(opts) > 0 {
		opt = opts[0]
	}
	return InspectWithOptions(src, opt)
}

// InspectWithOptions behaves like Inspect but accepts an explicit Options value.
func InspectWithOptions(src string, opt Options) (*Program, error) {
	node, err := Parse(src, opt)
	if err != nil {
		return nil, err
	}
	if node == nil {
		return &Program{Root: nil}, nil
	}
	root := &ProgramNode{Node: *node}
	return &Program{Root: root}, nil
}
