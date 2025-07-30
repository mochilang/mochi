package prolog

import (
	"encoding/json"
	"fmt"
	sitter "github.com/tree-sitter/go-tree-sitter"
	"strconv"
)

// Option controls how the AST is generated when converting parsed terms into
// Node structures. Position information is omitted unless Positions is true so
// the resulting JSON stays compact by default.
type Option struct {
	// Positions controls whether line and column information is populated.
	// It is disabled by default so the resulting JSON remains compact.
	Positions bool
}

// Term represents a Prolog term.
type Term struct {
	Var     string   `json:"var,omitempty"`
	Functor string   `json:"functor,omitempty"`
	Args    []Term   `json:"args,omitempty"`
	List    []Term   `json:"-"`
	Atom    string   `json:"-"`
	Number  *float64 `json:"-"`
	Bool    *bool    `json:"-"`
	Text    string   `json:"-"`
}

// Node is a simplified representation used in the JSON output. Only meaningful
// tokens are preserved. Text contains the literal token string for leaves.
// Node represents a simplified AST node used in the JSON output.  Only nodes
// that carry meaningful values are kept.  Position fields are omitted when zero
// so callers can choose whether to populate them.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Exported aliases for common node kinds so Program can expose a slightly more
// structured API.
type (
	Program Node
	Clause  Node
	Var     Node
	Atom    Node
	Number  Node
	Bool    Node
	List    Node
	Dict    Node
)

// Clause describes a predicate clause.
// rawClause is used when decoding the JSON output produced by the helper
// Prolog script. It mirrors the structure generated there.
type rawClause struct {
	Name   string `json:"name"`
	Params []Term `json:"params"`
	Goal   Term   `json:"goal"`
	Start  int    `json:"start,omitempty"`
	End    int    `json:"end,omitempty"`
}

// rawProgram is the top-level structure emitted by the helper script.
type rawProgram struct {
	Clauses []rawClause `json:"clauses"`
}

// toTerm converts a tree-sitter node into a Term. The conversion is best effort
// and only relies on generic node information so it works even if the grammar
// changes. Leaf nodes become atoms or variables, inner nodes become functor
// terms with their named children as arguments.
func toTerm(n *sitter.Node, src []byte) Term {
	if n == nil {
		return Term{}
	}
	if n.ChildCount() == 0 {
		text := string(src[n.StartByte():n.EndByte()])
		kind := n.Kind()
		if kind == "variable" {
			return Term{Var: text, Text: text}
		}
		if kind == "true" || kind == "false" {
			b := kind == "true"
			return Term{Bool: &b, Text: text}
		}
		if num, err := strconv.ParseFloat(text, 64); err == nil {
			return Term{Number: &num, Text: text}
		}
		return Term{Atom: text, Text: text}
	}
	t := Term{Functor: n.Kind(), Text: n.Kind()}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(uint(i))
		t.Args = append(t.Args, toTerm(child, src))
	}
	return t
}

// termToNode converts a parsed Term into the simplified Node representation.
func termToNode(t Term) *Node {
	switch {
	case t.Var != "":
		return &Node{Kind: "var", Text: t.Var}
	case t.Number != nil:
		return &Node{Kind: "number", Text: t.Text}
	case t.Bool != nil:
		return &Node{Kind: "bool", Text: t.Text}
	case t.Atom != "":
		return &Node{Kind: "atom", Text: t.Atom}
	case t.List != nil:
		n := &Node{Kind: "list"}
		for _, el := range t.List {
			n.Children = append(n.Children, termToNode(el))
		}
		return n
	case t.Functor != "":
		n := &Node{Kind: t.Functor}
		for _, a := range t.Args {
			n.Children = append(n.Children, termToNode(a))
		}
		return n
	default:
		return &Node{Kind: "unknown"}
	}
}

// pos converts a byte offset into 1-indexed line and column numbers.
func pos(src []byte, off int) (int, int) {
	line, col := 1, 0
	for i, b := range src {
		if i >= off {
			break
		}
		if b == '\n' {
			line++
			col = 0
		} else {
			col++
		}
	}
	return line, col
}

// convert transforms a tree-sitter node into our Node structure.
func convert(n *sitter.Node, src []byte, opt Option) *Node {
	if n == nil {
		return nil
	}
	sp := n.StartPosition()
	ep := n.EndPosition()
	node := &Node{Kind: n.Kind()}
	if opt.Positions {
		node.Start = int(sp.Row) + 1
		node.StartCol = int(sp.Column)
		node.End = int(ep.Row) + 1
		node.EndCol = int(ep.Column)
	}

	if n.NamedChildCount() == 0 {
		switch n.Kind() {
		case "variable", "number", "atom", "true", "false", "string":
			node.Text = string(src[n.StartByte():n.EndByte()])
		default:
			// skip non-value leaf nodes
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(uint(i))
		c := convert(child, src, opt)
		if c == nil {
			continue
		}
		node.Children = append(node.Children, c)
	}
	if node.Text == "" && len(node.Children) == 0 {
		return nil
	}
	return node
}

// programToNode converts a Program parsed by SWI-Prolog into a Node tree.
func programToNode(p *rawProgram, src []byte, opt Option) *Program {
	root := &Node{Kind: "program"}
	for _, c := range p.Clauses {
		clause := &Node{Kind: "clause"}
		if opt.Positions {
			line, col := pos(src, c.Start)
			endLine, endCol := pos(src, c.End)
			clause.Start = line
			clause.StartCol = col
			clause.End = endLine
			clause.EndCol = endCol
		}

		head := &Node{Kind: c.Name}
		for _, p := range c.Params {
			head.Children = append(head.Children, termToNode(p))
		}
		clause.Children = append(clause.Children, head)
		clause.Children = append(clause.Children, termToNode(c.Goal))
		root.Children = append(root.Children, clause)
	}
	return (*Program)(root)
}

// UnmarshalJSON decodes a term from the JSON form produced by the inspector script.
func (t *Term) UnmarshalJSON(b []byte) error {
	if len(b) == 0 {
		return nil
	}
	t.Var = ""
	t.Functor = ""
	t.Args = nil
	t.List = nil
	t.Atom = ""
	t.Number = nil
	t.Bool = nil
	t.Text = ""
	switch b[0] {
	case '{':
		var obj map[string]json.RawMessage
		if err := json.Unmarshal(b, &obj); err != nil {
			return err
		}
		if v, ok := obj["var"]; ok {
			if err := json.Unmarshal(v, &t.Var); err != nil {
				return err
			}
			t.Text = t.Var
			return nil
		}
		if f, ok := obj["functor"]; ok {
			if err := json.Unmarshal(f, &t.Functor); err != nil {
				return err
			}
			if a, ok := obj["args"]; ok {
				if err := json.Unmarshal(a, &t.Args); err != nil {
					return err
				}
			}
			t.Text = t.Functor
			return nil
		}
		for k, v := range obj {
			var child Term
			if err := json.Unmarshal(v, &child); err != nil {
				return err
			}
			t.Functor = "{}"
			t.Args = append(t.Args, Term{Functor: ":", Args: []Term{{Atom: k, Text: k}, child}})
		}
		t.Text = "{}"
		return nil
	case '[':
		if err := json.Unmarshal(b, &t.List); err != nil {
			return err
		}
		t.Text = "[]"
		return nil
	case '"':
		if err := json.Unmarshal(b, &t.Atom); err != nil {
			return err
		}
		t.Text = t.Atom
		return nil
	case 't', 'f':
		var v bool
		if err := json.Unmarshal(b, &v); err != nil {
			return err
		}
		t.Bool = &v
		if v {
			t.Text = "true"
		} else {
			t.Text = "false"
		}
		return nil
	default:
		var num float64
		if err := json.Unmarshal(b, &num); err == nil {
			t.Number = &num
			t.Text = fmt.Sprintf("%v", num)
			return nil
		}
	}
	return fmt.Errorf("unknown term: %s", string(b))
}

// MarshalJSON encodes the term back to the JSON format understood by the golden files.
func (t Term) MarshalJSON() ([]byte, error) {
	switch {
	case t.Var != "":
		return json.Marshal(struct {
			Var string `json:"var"`
		}{t.Var})
	case t.Functor != "":
		return json.Marshal(struct {
			Args    []Term `json:"args,omitempty"`
			Functor string `json:"functor"`
		}{t.Args, t.Functor})
	case t.List != nil:
		return json.Marshal(t.List)
	case t.Number != nil:
		return json.Marshal(*t.Number)
	case t.Bool != nil:
		return json.Marshal(*t.Bool)
	default:
		return json.Marshal(t.Atom)
	}
}
