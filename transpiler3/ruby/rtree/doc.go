// Package rtree provides in-memory Ruby AST nodes for the Mochi Ruby transpiler.
// Each node has a RubyString(indent int) string method that renders canonical
// Ruby 3.4+ source. Top-level SourceFile.RubySource() emits the frozen string
// literal magic comment and joins requires + declarations.
package rtree
