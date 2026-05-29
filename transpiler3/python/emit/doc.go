// Package emit renders a pysrc.Module to .py source bytes.
//
// Phase 1 emits source directly via pysrc.Module.PySource() and writes
// it to disk. Phase 16 (reproducibility) will introduce a libcst /
// ast.unparse subprocess pass to canonicalise whitespace per the
// CPython 3.12 grammar; Phase 1 keeps the dependency surface minimal
// so the test gate runs on any host with a working CPython binary.
package emit
