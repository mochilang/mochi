package fs

import (
	sitter "github.com/smacker/go-tree-sitter"
	fsharp "github.com/tree-sitter/tree-sitter-fsharp/bindings/go"
)

type Program struct {
	Vars   []Var    `json:"vars"`
	Prints []string `json:"prints"`
	Stmts  []Stmt   `json:"stmts"`
}

// Inspect parses F# code using tree-sitter.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(fsharp.LanguageFSharp()))
	tree := parser.Parse(nil, []byte(src))
	p := &Program{Vars: []Var{}, Prints: []string{}, Stmts: []Stmt{}}
	walk(tree.RootNode(), []byte(src), p)
	return p, nil
}

func walk(n *sitter.Node, src []byte, p *Program) {
	if n == nil {
		return
	}
	switch n.Type() {
	case "value_declaration":
		if v := parseVar(n, src); v != nil {
			p.Vars = append(p.Vars, *v)
		}
	case "for_expression":
		if s := parseFor(n, src); s != nil {
			p.Stmts = append(p.Stmts, s)
		}
	case "application_expression":
		if s := parsePrint(n, src); s != "" {
			p.Prints = append(p.Prints, s)
		}
	}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		walk(n.NamedChild(i), src, p)
	}
}

func parseVar(n *sitter.Node, src []byte) *Var {
	nameNode := findDescendant(n, "identifier")
	body := n.ChildByFieldName("body")
	if nameNode == nil || body == nil {
		return nil
	}
	name := nameNode.Content(src)
	expr := string(src[body.StartByte():body.EndByte()])
	return &Var{Name: name, Expr: expr, Mutable: false, Type: "", Line: int(n.StartPoint().Row) + 1, Raw: ""}
}

func parseFor(n *sitter.Node, src []byte) Stmt {
	pat := n.ChildByFieldName("pattern")
	iter := n.ChildByFieldName("iterator")
	body := n.ChildByFieldName("body")
	if pat == nil || iter == nil {
		return nil
	}
	nameNode := findDescendant(pat, "identifier")
	if nameNode == nil {
		return nil
	}
	varName := nameNode.Content(src)
	expr := string(src[iter.StartByte():iter.EndByte()])
	var stmts []Stmt
	if body != nil {
		tmp := &Program{}
		walk(body, src, tmp)
		stmts = tmp.Stmts
	}
	return ForIn{Var: varName, Expr: expr, Body: stmts, Line: int(n.StartPoint().Row) + 1, Raw: ""}
}

func parsePrint(n *sitter.Node, src []byte) string {
	fn := n.ChildByFieldName("function")
	if fn == nil {
		return ""
	}
	id := findDescendant(fn, "identifier")
	if id == nil || id.Content(src) != "printfn" {
		return ""
	}
	args := n.ChildByFieldName("arguments")
	if args == nil {
		return ""
	}
	return string(src[args.StartByte():args.EndByte()])
}

func findDescendant(n *sitter.Node, typ string) *sitter.Node {
	if n.Type() == typ {
		return n
	}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		if res := findDescendant(n.NamedChild(i), typ); res != nil {
			return res
		}
	}
	return nil
}
