package python

import py "mochi/aster/x/py"

// Inspect parses Python source code and returns its AST using the
// underlying tree-sitter implementation from the py package.
func Inspect(src string) (*Program, error) {
	return py.Inspect(src)
}

// InspectWithOption behaves like Inspect but allows callers to specify
// parser options such as including position information.
func InspectWithOption(src string, opt Option) (*Program, error) {
	return py.InspectWithOption(src, py.Option(opt))
}
