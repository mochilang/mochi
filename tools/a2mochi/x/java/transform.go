//go:build slow

package java

import (
	"fmt"
	"strconv"

	mochias "mochi/ast"
)

// Transform converts the parsed Java Node into a Mochi AST node.
func Transform(n *Node) (*mochias.Node, error) {
	if n == nil {
		return nil, fmt.Errorf("nil node")
	}
	mutated := map[string]bool{}
	for _, st := range n.Body {
		if st.Kind == "Assign" {
			mutated[st.Name] = true
		}
	}
	prog := &mochias.Node{Kind: "program"}
	for _, st := range n.Body {
		prog.Children = append(prog.Children, stmtNode(st, mutated))
	}
	return prog, nil
}

// TransformFile reads a Java file and converts it to a Mochi AST node.
func TransformFile(path string) (*mochias.Node, error) {
	n, err := ParseFile(path)
	if err != nil {
		return nil, err
	}
	return Transform(n)
}

func stmtNode(s Stmt, mutated map[string]bool) *mochias.Node {
	switch s.Kind {
	case "VarDecl":
		kind := "var"
		if s.Expr != nil && !mutated[s.Name] {
			kind = "let"
		}
		n := &mochias.Node{Kind: kind, Value: s.Name}
		if s.Expr != nil {
			n.Children = append(n.Children, exprNode(*s.Expr))
		} else {
			n.Children = append(n.Children, &mochias.Node{Kind: "int", Value: 0})
		}
		return n
	case "Assign":
		return &mochias.Node{Kind: "assign", Value: s.Name, Children: []*mochias.Node{exprNode(*s.Expr)}}
	case "Print":
		call := &mochias.Node{Kind: "call", Value: "print"}
		if s.Expr != nil {
			call.Children = append(call.Children, exprNode(*s.Expr))
		}
		return call
	case "ForRange":
		r := &mochias.Node{Kind: "range", Children: []*mochias.Node{exprNode(*s.Start), exprNode(*s.End)}}
		b := blockNode(s.Body, mutated)
		return &mochias.Node{Kind: "for", Value: s.Name, Children: []*mochias.Node{r, b}}
	case "ForEach":
		in := &mochias.Node{Kind: "in", Children: []*mochias.Node{exprNode(*s.Expr)}}
		b := blockNode(s.Body, mutated)
		return &mochias.Node{Kind: "for", Value: s.Name, Children: []*mochias.Node{in, b}}
	case "While":
		b := blockNode(s.Body, mutated)
		return &mochias.Node{Kind: "while", Children: []*mochias.Node{exprNode(*s.Expr), b}}
	case "If":
		thenN := blockNode(s.Then, mutated)
		n := &mochias.Node{Kind: "if", Children: []*mochias.Node{exprNode(*s.Expr), thenN}}
		if len(s.Else) > 0 {
			n.Children = append(n.Children, blockNode(s.Else, mutated))
		}
		return n
	default:
		return &mochias.Node{Kind: "unknown"}
	}
}

func blockNode(body []Stmt, mutated map[string]bool) *mochias.Node {
	blk := &mochias.Node{Kind: "block"}
	for _, st := range body {
		blk.Children = append(blk.Children, stmtNode(st, mutated))
	}
	return blk
}

func exprNode(e Expr) *mochias.Node {
	switch e.Kind {
	case "Literal":
		if i, err := strconv.Atoi(e.Value); err == nil {
			return &mochias.Node{Kind: "int", Value: i}
		}
		if f, err := strconv.ParseFloat(e.Value, 64); err == nil {
			return &mochias.Node{Kind: "float", Value: f}
		}
		if e.Value == "true" || e.Value == "false" {
			return &mochias.Node{Kind: "bool", Value: e.Value == "true"}
		}
		return &mochias.Node{Kind: "string", Value: e.Value}
	case "String":
		return &mochias.Node{Kind: "string", Value: e.Value}
	case "Ident":
		return &mochias.Node{Kind: "selector", Value: e.Name}
	case "Binary":
		return &mochias.Node{Kind: "binary", Value: op(e.Op), Children: []*mochias.Node{exprNode(*e.Left), exprNode(*e.Right)}}
	case "Call":
		n := &mochias.Node{Kind: "call"}
		n.Children = append(n.Children, exprNode(*e.Target))
		for _, a := range e.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	case "Member":
		return &mochias.Node{Kind: "selector", Value: e.Name, Children: []*mochias.Node{exprNode(*e.Expr)}}
	case "Array":
		arr := &mochias.Node{Kind: "list"}
		for _, el := range e.Elems {
			arr.Children = append(arr.Children, exprNode(el))
		}
		return arr
	case "Cond":
		if e.Then != nil && e.Else != nil &&
			e.Then.Kind == "Literal" && e.Then.Value == "1" &&
			e.Else.Kind == "Literal" && e.Else.Value == "0" {
			return exprNode(*e.Cond)
		}
		// general ternary not supported; return condition only
		return exprNode(*e.Cond)
	default:
		return &mochias.Node{Kind: "unknown"}
	}
}

func op(k string) string {
	switch k {
	case "PLUS":
		return "+"
	case "MINUS":
		return "-"
	case "MULTIPLY":
		return "*"
	case "DIVIDE":
		return "/"
	case "REMAINDER":
		return "%"
	case "LESS_THAN":
		return "<"
	case "GREATER_THAN":
		return ">"
	case "EQUAL_TO":
		return "=="
	case "NOT_EQUAL_TO":
		return "!="
	case "LESS_THAN_EQUAL":
		return "<="
	case "GREATER_THAN_EQUAL":
		return ">="
	}
	return k
}
