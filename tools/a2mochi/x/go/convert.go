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
			if d.Name.Name != "main" || d.Body == nil {
				continue
			}
			for _, st := range d.Body.List {
				root.Children = append(root.Children, stmtToNode(st)...)
			}
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
       case *ast.ForStmt:
               // support simple for i := a; i < b; i++ { ... }
               if init, ok := s.Init.(*ast.AssignStmt); ok && init.Tok == token.DEFINE && len(init.Lhs) == 1 && len(init.Rhs) == 1 {
                       if id, ok := init.Lhs[0].(*ast.Ident); ok {
                               if cond, ok := s.Cond.(*ast.BinaryExpr); ok && cond.Op == token.LSS {
                                       if inc, ok := s.Post.(*ast.IncDecStmt); ok && inc.Tok == token.INC {
                                               start := exprToNode(init.Rhs[0])
                                               end := exprToNode(cond.Y)
                                               body := blockToNode(s.Body)
                                               fr := &mochias.Node{Kind: "range", Children: []*mochias.Node{start, end}}
                                               n := &mochias.Node{Kind: "for", Value: id.Name, Children: []*mochias.Node{fr, body}}
                                               return []*mochias.Node{n}
                                       }
                               }
                       }
               }
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
		if id, ok := fn.X.(*ast.Ident); ok && id.Name == "fmt" && fn.Sel.Name == "Println" {
			n := &mochias.Node{Kind: "call", Value: "print"}
			for _, arg := range c.Args {
				n.Children = append(n.Children, exprToNode(arg))
			}
			return n
		}
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
