//go:build slow

package gox

import (
	"fmt"
	"go/ast"
	goparser "go/parser"
	"go/token"
	"os"
	"strconv"
	"strings"
	"time"

	mochias "mochi/ast"
)

// Node wraps a parsed Go file.
type Node struct {
	File  *ast.File
	Fset  *token.FileSet
	Lines []string
}

// Parse parses Go source code into a Node.
func Parse(src string) (*Node, error) {
	fset := token.NewFileSet()
	file, err := goparser.ParseFile(fset, "", src, goparser.ParseComments)
	if err != nil {
		return nil, err
	}
	return &Node{File: file, Fset: fset, Lines: strings.Split(src, "\n")}, nil
}

// Convert converts a parsed Go Node into a Mochi AST node.
// Only a tiny subset of Go is supported; for now this is unimplemented and
// returns an empty program.
func Convert(n *Node) (*mochias.Node, error) {
	if n == nil {
		return nil, fmt.Errorf("nil node")
	}

	root := &mochias.Node{Kind: "program"}

	for _, decl := range n.File.Decls {
		switch d := decl.(type) {
		case *ast.GenDecl:
			if d.Tok != token.VAR {
				continue
			}
			for _, sp := range d.Specs {
				vs, ok := sp.(*ast.ValueSpec)
				if !ok || len(vs.Names) != 1 || len(vs.Values) != 1 {
					continue
				}
				name := vs.Names[0].Name
				val := exprToNode(vs.Values[0])
				root.Children = append(root.Children, &mochias.Node{
					Kind:     "var",
					Value:    name,
					Children: []*mochias.Node{val},
				})
			}
		case *ast.FuncDecl:
			if d.Body == nil {
				continue
			}
			if d.Name.Name == "main" {
				for _, st := range d.Body.List {
					root.Children = append(root.Children, stmtToNode(st)...)
				}
				continue
			}
			fn := &mochias.Node{Kind: "fun", Value: d.Name.Name}
			for _, p := range d.Type.Params.List {
				for _, n := range p.Names {
					fn.Children = append(fn.Children, &mochias.Node{Kind: "param", Value: n.Name})
				}
			}
			fn.Children = append(fn.Children, blockToNode(d.Body))
			root.Children = append(root.Children, fn)
		}
	}

	return root, nil
}

func stmtToNode(st ast.Stmt) []*mochias.Node {
	switch s := st.(type) {
	case *ast.ExprStmt:
		if call, ok := s.X.(*ast.CallExpr); ok {
			if n := callToNode(call); n != nil {
				return []*mochias.Node{n}
			}
		}
	case *ast.AssignStmt:
		if len(s.Lhs) == 1 && len(s.Rhs) == 1 {
			lhs := exprToNode(s.Lhs[0])
			rhs := exprToNode(s.Rhs[0])
			return []*mochias.Node{{
				Kind:     "assign",
				Children: []*mochias.Node{lhs, rhs},
			}}
		}
	case *ast.ForStmt:
		body := blockToNode(s.Body)
		// while style loop: for cond { .. }
		if s.Init == nil && s.Post == nil {
			return []*mochias.Node{{
				Kind:     "while",
				Children: []*mochias.Node{exprToNode(s.Cond), body},
			}}
		}
		// basic for loop: for i := start; i < end; i++ { ... }
		if as, ok := s.Init.(*ast.AssignStmt); ok &&
			len(as.Lhs) == 1 && len(as.Rhs) == 1 &&
			as.Tok == token.DEFINE {
			if inc, ok := s.Post.(*ast.IncDecStmt); ok && inc.Tok == token.INC {
				if id, ok := as.Lhs[0].(*ast.Ident); ok {
					varName := id.Name
					if xid, ok := inc.X.(*ast.Ident); ok && xid.Name == varName {
						if be, ok := s.Cond.(*ast.BinaryExpr); ok {
							if lhs, ok := be.X.(*ast.Ident); ok && lhs.Name == varName {
								start := exprToNode(as.Rhs[0])
								end := exprToNode(be.Y)
								rng := &mochias.Node{Kind: "range", Children: []*mochias.Node{start, end}}
								return []*mochias.Node{{
									Kind:     "for",
									Value:    varName,
									Children: []*mochias.Node{rng, body},
								}}
							}
						}
					}
				}
			}
		}
		// fallback to while loop if pattern does not match
		return []*mochias.Node{{
			Kind:     "while",
			Children: []*mochias.Node{exprToNode(s.Cond), body},
		}}
	case *ast.IfStmt:
		cond := exprToNode(s.Cond)
		thenBlk := blockToNode(s.Body)
		var elseBlk *mochias.Node
		if s.Else != nil {
			if eb, ok := s.Else.(*ast.BlockStmt); ok {
				elseBlk = blockToNode(eb)
			}
		}
		n := &mochias.Node{Kind: "if", Children: []*mochias.Node{cond, thenBlk}}
		if elseBlk != nil {
			n.Children = append(n.Children, elseBlk)
		}
		return []*mochias.Node{n}
	case *ast.ReturnStmt:
		if len(s.Results) == 1 {
			return []*mochias.Node{{
				Kind:     "return",
				Children: []*mochias.Node{exprToNode(s.Results[0])},
			}}
		}
		return []*mochias.Node{{Kind: "return"}}
	}
	return nil
}

func blockToNode(b *ast.BlockStmt) *mochias.Node {
	if b == nil {
		return &mochias.Node{Kind: "block"}
	}
	blk := &mochias.Node{Kind: "block"}
	for _, st := range b.List {
		blk.Children = append(blk.Children, stmtToNode(st)...)
	}
	return blk
}

func callToNode(c *ast.CallExpr) *mochias.Node {
	switch fn := c.Fun.(type) {
	case *ast.SelectorExpr:
		if id, ok := fn.X.(*ast.Ident); ok && id.Name == "fmt" {
			switch fn.Sel.Name {
			case "Println":
				n := &mochias.Node{Kind: "call", Value: "print"}
				for _, arg := range c.Args {
					n.Children = append(n.Children, exprToNode(arg))
				}
				return n
			case "Sprint":
				n := &mochias.Node{Kind: "call", Value: "str"}
				for _, arg := range c.Args {
					n.Children = append(n.Children, exprToNode(arg))
				}
				return n
			}
		}
	case *ast.Ident:
		if fn.Name == "len" {
			n := &mochias.Node{Kind: "call", Value: "len"}
			for _, arg := range c.Args {
				n.Children = append(n.Children, exprToNode(arg))
			}
			return n
		}
		n := &mochias.Node{Kind: "call", Value: fn.Name}
		for _, arg := range c.Args {
			n.Children = append(n.Children, exprToNode(arg))
		}
		return n
	}
	return nil
}

func exprToNode(e ast.Expr) *mochias.Node {
	switch v := e.(type) {
	case *ast.BasicLit:
		switch v.Kind {
		case token.INT:
			iv, _ := strconv.Atoi(v.Value)
			return &mochias.Node{Kind: "int", Value: iv}
		case token.STRING:
			s, _ := strconv.Unquote(v.Value)
			return &mochias.Node{Kind: "string", Value: s}
		}
	case *ast.Ident:
		return &mochias.Node{Kind: "selector", Value: v.Name}
	case *ast.BinaryExpr:
		return &mochias.Node{Kind: "binary", Value: v.Op.String(), Children: []*mochias.Node{
			exprToNode(v.X), exprToNode(v.Y),
		}}
	case *ast.UnaryExpr:
		if v.Op == token.SUB {
			return &mochias.Node{Kind: "unary", Value: "-", Children: []*mochias.Node{exprToNode(v.X)}}
		}
	case *ast.CallExpr:
		if n := callToNode(v); n != nil {
			return n
		}
		if fn, ok := v.Fun.(*ast.FuncLit); ok {
			// detect pattern: func() int { if cond { return 1 } return 0 }()
			if len(fn.Body.List) == 1 {
				if ifs, ok := fn.Body.List[0].(*ast.IfStmt); ok {
					if len(ifs.Body.List) == 1 {
						if ret, ok := ifs.Body.List[0].(*ast.ReturnStmt); ok && len(ret.Results) == 1 {
							if bl, ok := ret.Results[0].(*ast.BasicLit); ok && bl.Value == "1" {
								if ifs.Else != nil {
									if eb, ok := ifs.Else.(*ast.BlockStmt); ok && len(eb.List) == 1 {
										if ret0, ok := eb.List[0].(*ast.ReturnStmt); ok && len(ret0.Results) == 1 {
											if bl0, ok := ret0.Results[0].(*ast.BasicLit); ok && bl0.Value == "0" {
												return exprToNode(ifs.Cond)
											}
										}
									}
								}
							}
						}
					}
				}
			} else if len(fn.Body.List) == 2 {
				if ifs, ok := fn.Body.List[0].(*ast.IfStmt); ok {
					if len(ifs.Body.List) == 1 {
						if ret, ok := ifs.Body.List[0].(*ast.ReturnStmt); ok && len(ret.Results) == 1 {
							if bl, ok := ret.Results[0].(*ast.BasicLit); ok && bl.Value == "1" {
								if ret0, ok := fn.Body.List[1].(*ast.ReturnStmt); ok && len(ret0.Results) == 1 {
									if bl0, ok := ret0.Results[0].(*ast.BasicLit); ok && bl0.Value == "0" {
										return exprToNode(ifs.Cond)
									}
								}
							}
						}
					}
				}
			}
		}
		n := &mochias.Node{Kind: "call"}
		n.Children = append(n.Children, exprToNode(v.Fun))
		for _, arg := range v.Args {
			n.Children = append(n.Children, exprToNode(arg))
		}
		return n
	case *ast.ParenExpr:
		return exprToNode(v.X)
	}
	return &mochias.Node{Kind: "unknown"}
}

// ConvertSource emits Mochi source code for the parsed Go node. Only a header
// comment and the original source code block are emitted for now.
func ConvertSource(n *Node) (string, error) {
	if n == nil {
		return "", fmt.Errorf("nil node")
	}
	astNode, err := Convert(n)
	if err != nil {
		return "", err
	}

	var code strings.Builder
	if err := mochias.Fprint(&code, astNode); err != nil {
		return "", err
	}

	version := strings.TrimSpace(readVersion())
	t := time.Now().In(time.FixedZone("GMT+7", 7*3600)).Format("2006-01-02 15:04:05")

	var b strings.Builder
	fmt.Fprintf(&b, "// a2mochi go v%s %s GMT+7\n", version, t)
	b.WriteString("/*\n")
	b.WriteString(strings.Join(n.Lines, "\n"))
	if len(n.Lines) == 0 || n.Lines[len(n.Lines)-1] != "" {
		b.WriteByte('\n')
	}
	b.WriteString("*/\n")
	b.WriteString(code.String())
	b.WriteByte('\n')
	return b.String(), nil
}

// readVersion reads the VERSION file at repo root.
func readVersion() string {
	data, err := os.ReadFile("VERSION")
	if err != nil {
		return "dev"
	}
	return string(data)
}
