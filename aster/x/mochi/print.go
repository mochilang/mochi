package mochi

import (
	"encoding/json"
	"fmt"
	"regexp"
	"strings"

	mast "mochi/ast"
)

// Print returns Mochi source code for the given Program.
func Print(p *Program) (string, error) {
	if p == nil || p.File == nil {
		return "", fmt.Errorf("nil program")
	}
	n := toAST(&p.File.Node)
	src := n.Source()
	// Normalize query keywords that mast.Fprint doesn't handle exactly
	src = strings.ReplaceAll(src, "union_all", "union all")
	src = strings.ReplaceAll(src, "left_join", "left join")
	src = strings.ReplaceAll(src, "right_join", "right join")
	src = strings.ReplaceAll(src, "outer_join", "outer join")
	reSort := regexp.MustCompile(`(?m)^(\s*)sort (.+)$`)
	src = reSort.ReplaceAllString(src, "${1}sort by ${2}")
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src += "\n"
	}
	return src, nil
}

func toAST(n *Node) *mast.Node {
	if n == nil {
		return nil
	}
	a := &mast.Node{Kind: n.Kind}
	if n.Text != "" || n.Kind == "string" {
		a.Value = n.Text
	}
	for _, c := range n.Children {
		if ac := toAST(c); ac != nil {
			a.Children = append(a.Children, ac)
		}
	}
	return a
}

// Marshal encodes Program as JSON.
func Marshal(p *Program) ([]byte, error) {
	return json.MarshalIndent(p, "", "  ")
}
