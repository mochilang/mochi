package python

import py "mochi/aster/x/py"

// Print reconstructs Python source code from the provided Program AST.
// The implementation delegates to the py package printer which walks
// the AST without reusing the original source text.
func Print(p *Program) (string, error) {
	return py.Print((*py.Program)(p))
}
