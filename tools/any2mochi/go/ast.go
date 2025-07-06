package golang

import (
	"encoding/json"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"strings"
)

// AST represents a simplified Go AST suitable for JSON serialization.
type AST struct {
	Functions []Func `json:"functions,omitempty"`
	Types     []Type `json:"types,omitempty"`
	Vars      []Var  `json:"vars,omitempty"`
}

type Func struct {
	Name   string  `json:"name"`
	Recv   string  `json:"recv,omitempty"`
	Params []Param `json:"params,omitempty"`
	Result string  `json:"result,omitempty"`
}

type Param struct {
	Name string `json:"name,omitempty"`
	Type string `json:"type,omitempty"`
}

type Type struct {
	Name      string  `json:"name"`
	Interface bool    `json:"interface,omitempty"`
	Fields    []Field `json:"fields,omitempty"`
	Methods   []Func  `json:"methods,omitempty"`
}

type Field struct {
	Name string `json:"name,omitempty"`
	Type string `json:"type,omitempty"`
}

type Var struct {
	Name string `json:"name"`
	Type string `json:"type,omitempty"`
}

// ParseAST parses Go source and returns the simplified AST.
func ParseAST(src string) (*AST, error) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "src.go", src, parser.SkipObjectResolution)
	if err != nil {
		return nil, err
	}
	out := &AST{}
	for _, decl := range file.Decls {
		switch d := decl.(type) {
		case *ast.FuncDecl:
			if d.Name == nil {
				continue
			}
			fn := Func{Name: d.Name.Name}
			if d.Recv != nil && len(d.Recv.List) > 0 {
				fn.Recv = exprString(fset, d.Recv.List[0].Type)
			}
			if d.Type.Params != nil {
				for _, p := range d.Type.Params.List {
					typ := exprString(fset, p.Type)
					if len(p.Names) == 0 {
						fn.Params = append(fn.Params, Param{Type: typ})
						continue
					}
					for _, n := range p.Names {
						fn.Params = append(fn.Params, Param{Name: n.Name, Type: typ})
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
					switch tt := ts.Type.(type) {
					case *ast.StructType:
						gt := Type{Name: ts.Name.Name}
						for _, f := range tt.Fields.List {
							typ := exprString(fset, f.Type)
							if len(f.Names) == 0 {
								gt.Fields = append(gt.Fields, Field{Type: typ})
								continue
							}
							for _, n := range f.Names {
								gt.Fields = append(gt.Fields, Field{Name: n.Name, Type: typ})
							}
						}
						out.Types = append(out.Types, gt)
					case *ast.InterfaceType:
						gt := Type{Name: ts.Name.Name, Interface: true}
						for _, m := range tt.Methods.List {
							if len(m.Names) == 0 {
								continue
							}
							ft, ok := m.Type.(*ast.FuncType)
							if !ok {
								continue
							}
							fn := Func{Name: m.Names[0].Name}
							if ft.Params != nil {
								for _, p := range ft.Params.List {
									typ := exprString(fset, p.Type)
									if len(p.Names) == 0 {
										fn.Params = append(fn.Params, Param{Type: typ})
										continue
									}
									for _, n := range p.Names {
										fn.Params = append(fn.Params, Param{Name: n.Name, Type: typ})
									}
								}
							}
							if ft.Results != nil && len(ft.Results.List) == 1 && len(ft.Results.List[0].Names) == 0 {
								ret := exprString(fset, ft.Results.List[0].Type)
								if ret != "" && ret != "void" {
									fn.Result = ret
								}
							}
							gt.Methods = append(gt.Methods, fn)
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
						out.Vars = append(out.Vars, Var{Name: n.Name, Type: typ})
					}
				}
			}
		}
	}
	return out, nil
}

// ParseFile reads a Go file and returns its simplified AST.
func ParseFile(path string) (*AST, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ParseAST(string(data))
}

// ConvertAST converts a simplified Go AST to Mochi code.
func ConvertAST(g *AST) []byte {
	var b strings.Builder
	for _, t := range g.Types {
		b.WriteString("type ")
		b.WriteString(t.Name)
		if t.Interface {
			b.WriteString(" interface {\n")
			for _, m := range t.Methods {
				b.WriteString("  ")
				b.WriteString(m.Name)
				b.WriteByte('(')
				for i, p := range m.Params {
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
				if m.Result != "" && m.Result != "void" {
					b.WriteString(": ")
					b.WriteString(m.Result)
				}
				b.WriteByte('\n')
			}
			b.WriteString("}\n")
			continue
		}
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

// ConvertJSON converts the JSON encoding of a simplified Go AST to Mochi code.
func ConvertJSON(data []byte) ([]byte, error) {
	var g AST
	if err := json.Unmarshal(data, &g); err != nil {
		return nil, err
	}
	return ConvertAST(&g), nil
}

// ConvertViaJSON parses Go source, encodes it to JSON, decodes back, and returns Mochi code.
func ConvertViaJSON(src string) ([]byte, error) {
	ast, err := ParseAST(src)
	if err != nil {
		return nil, err
	}
	data, err := json.Marshal(ast)
	if err != nil {
		return nil, err
	}
	return ConvertJSON(data)
}

// ConvertViaJSONFile reads a file and converts it via JSON AST.
func ConvertViaJSONFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertViaJSON(string(data))
}
