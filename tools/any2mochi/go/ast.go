package goconv

import (
	"encoding/json"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"strings"
)

// GoAST represents a simplified Go AST suitable for JSON serialization.
type GoAST struct {
	Functions []GoASTFunc `json:"functions,omitempty"`
	Types     []GoASTType `json:"types,omitempty"`
	Vars      []GoASTVar  `json:"vars,omitempty"`
}

type GoASTFunc struct {
	Name   string       `json:"name"`
	Recv   string       `json:"recv,omitempty"`
	Params []GoASTParam `json:"params,omitempty"`
	Result string       `json:"result,omitempty"`
}

type GoASTParam struct {
	Name string `json:"name,omitempty"`
	Type string `json:"type,omitempty"`
}

type GoASTType struct {
	Name   string       `json:"name"`
	Fields []GoASTField `json:"fields,omitempty"`
}

type GoASTField struct {
	Name string `json:"name,omitempty"`
	Type string `json:"type,omitempty"`
}

type GoASTVar struct {
	Name string `json:"name"`
	Type string `json:"type,omitempty"`
}

// ParseGoAST parses Go source and returns the simplified AST.
func ParseGoAST(src string) (*GoAST, error) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "src.go", src, parser.SkipObjectResolution)
	if err != nil {
		return nil, err
	}
	out := &GoAST{}
	for _, decl := range file.Decls {
		switch d := decl.(type) {
		case *ast.FuncDecl:
			if d.Name == nil {
				continue
			}
			fn := GoASTFunc{Name: d.Name.Name}
			if d.Recv != nil && len(d.Recv.List) > 0 {
				fn.Recv = exprString(fset, d.Recv.List[0].Type)
			}
			if d.Type.Params != nil {
				for _, p := range d.Type.Params.List {
					typ := exprString(fset, p.Type)
					if len(p.Names) == 0 {
						fn.Params = append(fn.Params, GoASTParam{Type: typ})
						continue
					}
					for _, n := range p.Names {
						fn.Params = append(fn.Params, GoASTParam{Name: n.Name, Type: typ})
					}
				}
			}
			if d.Type.Results != nil && len(d.Type.Results.List) == 1 && len(d.Type.Results.List[0].Names) == 0 {
				ret := exprString(fset, d.Type.Results.List[0].Type)
				if ret != "" && ret != "void" {
					fn.Result = ret
				}
			}
			out.Functions = append(out.Functions, fn)
		case *ast.GenDecl:
			switch d.Tok {
			case token.TYPE:
				for _, spec := range d.Specs {
					ts, ok := spec.(*ast.TypeSpec)
					if !ok {
						continue
					}
					if st, ok := ts.Type.(*ast.StructType); ok {
						gt := GoASTType{Name: ts.Name.Name}
						for _, f := range st.Fields.List {
							typ := exprString(fset, f.Type)
							if len(f.Names) == 0 {
								gt.Fields = append(gt.Fields, GoASTField{Type: typ})
								continue
							}
							for _, n := range f.Names {
								gt.Fields = append(gt.Fields, GoASTField{Name: n.Name, Type: typ})
							}
						}
						out.Types = append(out.Types, gt)
					}
				}
			case token.VAR, token.CONST:
				for _, spec := range d.Specs {
					vs, ok := spec.(*ast.ValueSpec)
					if !ok {
						continue
					}
					typ := ""
					if vs.Type != nil {
						typ = exprString(fset, vs.Type)
					}
					for _, n := range vs.Names {
						out.Vars = append(out.Vars, GoASTVar{Name: n.Name, Type: typ})
					}
				}
			}
		}
	}
	return out, nil
}

// ParseGoASTFile reads a Go file and returns its simplified AST.
func ParseGoASTFile(path string) (*GoAST, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ParseGoAST(string(data))
}

// ConvertGoAST converts a simplified Go AST to Mochi code.
func ConvertGoAST(g *GoAST) []byte {
	var b strings.Builder
	for _, t := range g.Types {
		b.WriteString("type ")
		b.WriteString(t.Name)
		b.WriteString(" {\n")
		for _, f := range t.Fields {
			b.WriteString("  ")
			if f.Name != "" {
				b.WriteString(f.Name)
			} else {
				b.WriteString(f.Type)
				b.WriteByte('\n')
				continue
			}
			if f.Type != "" {
				b.WriteString(": ")
				b.WriteString(f.Type)
			}
			b.WriteByte('\n')
		}
		b.WriteString("}\n")
	}
	for _, v := range g.Vars {
		b.WriteString("let ")
		b.WriteString(v.Name)
		if v.Type != "" {
			b.WriteString(": ")
			b.WriteString(v.Type)
		}
		b.WriteByte('\n')
	}
	for _, fn := range g.Functions {
		b.WriteString("fun ")
		if fn.Recv != "" {
			b.WriteString(fn.Recv)
			b.WriteByte('.')
		}
		b.WriteString(fn.Name)
		b.WriteByte('(')
		for i, p := range fn.Params {
			if i > 0 {
				b.WriteString(", ")
			}
			if p.Name != "" {
				b.WriteString(p.Name)
			} else {
				b.WriteByte('_')
			}
			if p.Type != "" {
				b.WriteString(": ")
				b.WriteString(p.Type)
			}
		}
		b.WriteByte(')')
		if fn.Result != "" && fn.Result != "void" {
			b.WriteString(": ")
			b.WriteString(fn.Result)
		}
		b.WriteString(" {}\n")
	}
	return []byte(b.String())
}

// ConvertGoJSON converts the JSON encoding of a simplified Go AST to Mochi code.
func ConvertGoJSON(data []byte) ([]byte, error) {
	var g GoAST
	if err := json.Unmarshal(data, &g); err != nil {
		return nil, err
	}
	return ConvertGoAST(&g), nil
}

// ConvertGoViaJSON parses Go source, encodes it to JSON, decodes back, and returns Mochi code.
func ConvertGoViaJSON(src string) ([]byte, error) {
	ast, err := ParseGoAST(src)
	if err != nil {
		return nil, err
	}
	data, err := json.Marshal(ast)
	if err != nil {
		return nil, err
	}
	return ConvertGoJSON(data)
}

// ConvertGoViaJSONFile reads a file and converts it via JSON AST.
func ConvertGoViaJSONFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertGoViaJSON(string(data))
}
