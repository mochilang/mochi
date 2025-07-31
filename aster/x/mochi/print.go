package mochi

import (
	"bytes"
	"encoding/json"
	"fmt"

	mast "mochi/ast"
)

// Print returns Mochi source code for the given Program.
func Print(p *Program) (string, error) {
	if p == nil || p.File == nil {
		return "", fmt.Errorf("nil program")
	}
	n := toAST(&p.File.Node)
	var buf bytes.Buffer
	if err := mast.Fprint(&buf, n); err != nil {
		return "", err
	}
	src := buf.String()
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
