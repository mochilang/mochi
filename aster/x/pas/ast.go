package pas

import (
	pasparser "github.com/akrennmair/pascal/parser"
)

// Node represents a simplified Pascal AST node converted from the pascal parser.
// Positional fields are optional and omitted from JSON when zero.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Option controls AST generation. When Positions is true the Start/End fields
// are populated. The pascal parser currently does not provide positional
// information so this flag has no effect, but it matches the other language
// implementations.
type Option struct {
	Positions bool
}

// Typed aliases mirror important Pascal constructs so Program can expose a
// structured view while still using Node internally. Only a very small subset
// of nodes required for the tests are represented.
type (
	ProgramNode Node
	Block       Node
	WriteStmt   Node
	String      Node
)

// convertProgram converts a pasparser.AST into our Node representation.
func convertProgram(ast *pasparser.AST, opt Option) *Node {
	if ast == nil {
		return &Node{Kind: "program"}
	}
	n := &Node{Kind: "program", Text: ast.Name}
	if b := convertBlock(ast.Block, opt); b != nil {
		n.Children = append(n.Children, b)
	}
	return n
}

func convertBlock(b *pasparser.Block, opt Option) *Node {
	if b == nil {
		return nil
	}
	node := &Node{Kind: "block"}
	for _, stmt := range b.Statements {
		if s := convertStatement(stmt, opt); s != nil {
			node.Children = append(node.Children, s)
		}
	}
	return node
}

func convertStatement(s pasparser.Statement, opt Option) *Node {
	switch stmt := s.(type) {
	case *pasparser.WriteStatement:
		kind := "write"
		if stmt.AppendNewLine {
			kind = "writeln"
		}
		node := &Node{Kind: kind}
		if stmt.FileVar != nil {
			if c := convertExpression(stmt.FileVar, opt); c != nil {
				node.Children = append(node.Children, c)
			}
		}
		for _, p := range stmt.ActualParams {
			if c := convertExpression(p, opt); c != nil {
				node.Children = append(node.Children, c)
			}
		}
		return node
	default:
		return nil
	}
}

func convertExpression(e pasparser.Expression, opt Option) *Node {
	switch expr := e.(type) {
	case *pasparser.StringExpr:
		return &Node{Kind: "string", Text: expr.Value}
	case *pasparser.FormatExpr:
		// collapse format expressions to just the contained expression when
		// width/decimal are not present.
		if expr.Width == nil && expr.DecimalPlaces == nil {
			return convertExpression(expr.Expr, opt)
		}
		n := &Node{Kind: "format"}
		if c := convertExpression(expr.Expr, opt); c != nil {
			n.Children = append(n.Children, c)
		}
		if w := convertExpression(expr.Width, opt); w != nil {
			w.Kind = "width"
			n.Children = append(n.Children, w)
		}
		if d := convertExpression(expr.DecimalPlaces, opt); d != nil {
			d.Kind = "decimal_places"
			n.Children = append(n.Children, d)
		}
		return n
	default:
		return nil
	}
}
