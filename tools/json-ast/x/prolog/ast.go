package prolog

import (
	"encoding/json"
	"fmt"
	sitter "github.com/smacker/go-tree-sitter"
	"strconv"
)

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
type Node struct {
	Kind     string `json:"kind"`
	Text     string `json:"text,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// Clause describes a predicate clause.
type Clause struct {
	Name   string `json:"name"`
	Params []Term `json:"params"`
	Goal   Term   `json:"goal"`
	Start  int    `json:"start"`
	End    int    `json:"end"`
}

// Program represents a parsed Prolog file.
type Program struct {
	Clauses []Clause `json:"clauses"`
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
		text := n.Content(src)
		if n.Type() == "variable" {
			return Term{Var: text, Text: text}
		}
		if n.Type() == "true" || n.Type() == "false" {
			b := n.Type() == "true"
			return Term{Bool: &b, Text: text}
		}
		if num, err := strconv.ParseFloat(text, 64); err == nil {
			return Term{Number: &num, Text: text}
		}
		return Term{Atom: text, Text: text}
	}
	t := Term{Functor: n.Type(), Text: n.Type()}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		t.Args = append(t.Args, toTerm(child, src))
	}
	return t
}

// termToNode converts a parsed Term into the simplified Node representation.
func termToNode(t Term) Node {
	switch {
	case t.Var != "":
		return Node{Kind: "var", Text: t.Var}
	case t.Number != nil:
		return Node{Kind: "number", Text: t.Text}
	case t.Bool != nil:
		return Node{Kind: "bool", Text: t.Text}
	case t.Atom != "":
		return Node{Kind: "atom", Text: t.Atom}
	case t.List != nil:
		n := Node{Kind: "list"}
		for _, el := range t.List {
			n.Children = append(n.Children, termToNode(el))
		}
		return n
	case t.Functor != "":
		n := Node{Kind: t.Functor}
		for _, a := range t.Args {
			n.Children = append(n.Children, termToNode(a))
		}
		return n
	default:
		return Node{Kind: "unknown"}
	}
}

// programToNode converts a Program parsed by SWI-Prolog into a Node tree.
func programToNode(p *Program) Node {
	root := Node{Kind: "program"}
	for _, c := range p.Clauses {
		clause := Node{Kind: "clause"}
		head := Node{Kind: c.Name}
		for _, p := range c.Params {
			head.Children = append(head.Children, termToNode(p))
		}
		clause.Children = append(clause.Children, head)
		clause.Children = append(clause.Children, termToNode(c.Goal))
		root.Children = append(root.Children, clause)
	}
	return root
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
