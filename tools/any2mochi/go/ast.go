package golang

import (
	"encoding/json"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"reflect"
	"strconv"
	"strings"
)

// AST represents a simplified Go AST suitable for JSON serialization.
type AST struct {
	Package   string   `json:"package,omitempty"`
	Imports   []string `json:"imports,omitempty"`
	Functions []Func   `json:"functions,omitempty"`
	Types     []Type   `json:"types,omitempty"`
	Vars      []Var    `json:"vars,omitempty"`
}

type Func struct {
	Name    string   `json:"name"`
	Recv    string   `json:"recv,omitempty"`
	Params  []Param  `json:"params,omitempty"`
	Results []string `json:"results,omitempty"`
	Doc     string   `json:"doc,omitempty"`
	Line    int      `json:"line,omitempty"`
	EndLine int      `json:"endLine,omitempty"`
}

type Param struct {
	Name string `json:"name,omitempty"`
	Type string `json:"type,omitempty"`
}

type Type struct {
	Name      string  `json:"name"`
	Fields    []Field `json:"fields,omitempty"`
	Methods   []Func  `json:"methods,omitempty"`
	Interface bool    `json:"interface,omitempty"`
	Doc       string  `json:"doc,omitempty"`
	Line      int     `json:"line,omitempty"`
	EndLine   int     `json:"endLine,omitempty"`
}

type Field struct {
	Name    string `json:"name,omitempty"`
	Type    string `json:"type,omitempty"`
	Tag     string `json:"tag,omitempty"`
	Doc     string `json:"doc,omitempty"`
	EndLine int    `json:"endLine,omitempty"`
	Line    int    `json:"line,omitempty"`
}

type Var struct {
	Name    string `json:"name"`
	Type    string `json:"type,omitempty"`
	Value   string `json:"value,omitempty"`
	Const   bool   `json:"const,omitempty"`
	Doc     string `json:"doc,omitempty"`
	Line    int    `json:"line,omitempty"`
	EndLine int    `json:"endLine,omitempty"`
}

// ParseAST parses Go source and returns the simplified AST.
func ParseAST(src string) (*AST, error) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "src.go", src, parser.ParseComments|parser.SkipObjectResolution)
	if err != nil {
		return nil, err
	}
	out := &AST{Package: file.Name.Name}
	for _, imp := range file.Imports {
		if path, err := strconv.Unquote(imp.Path.Value); err == nil {
			out.Imports = append(out.Imports, path)
		}
	}
	for _, decl := range file.Decls {
		switch d := decl.(type) {
		case *ast.FuncDecl:
			if d.Name == nil {
				continue
			}
			fn := Func{Name: d.Name.Name}
			fn.Line = fset.Position(d.Pos()).Line
			fn.EndLine = fset.Position(d.End()).Line
			if d.Doc != nil {
				fn.Doc = strings.TrimSpace(d.Doc.Text())
			}
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
			if d.Type.Results != nil {
				for _, r := range d.Type.Results.List {
					typ := exprString(fset, r.Type)
					if typ == "" || typ == "void" {
						continue
					}
					n := len(r.Names)
					if n == 0 {
						n = 1
					}
					for i := 0; i < n; i++ {
						fn.Results = append(fn.Results, typ)
					}
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
						gt := Type{Name: ts.Name.Name, Line: fset.Position(ts.Pos()).Line, EndLine: fset.Position(ts.End()).Line}
						if d.Doc != nil {
							gt.Doc = strings.TrimSpace(d.Doc.Text())
						}
						for _, f := range st.Fields.List {
							typ := exprString(fset, f.Type)
							tag := ""
							if f.Tag != nil {
								tag = strings.Trim(f.Tag.Value, "`")
							}
							doc := ""
							if f.Doc != nil {
								doc = strings.TrimSpace(f.Doc.Text())
							} else if f.Comment != nil {
								doc = strings.TrimSpace(f.Comment.Text())
							}
							if len(f.Names) == 0 {
								gt.Fields = append(gt.Fields, Field{Type: typ, Tag: tag, Doc: doc, Line: fset.Position(f.Pos()).Line, EndLine: fset.Position(f.End()).Line})
								continue
							}
							for _, n := range f.Names {
								gt.Fields = append(gt.Fields, Field{Name: n.Name, Type: typ, Tag: tag, Doc: doc, Line: fset.Position(n.Pos()).Line, EndLine: fset.Position(f.End()).Line})
							}
						}
						out.Types = append(out.Types, gt)
					}
					if iface, ok := ts.Type.(*ast.InterfaceType); ok {
						gt := Type{Name: ts.Name.Name, Interface: true, Line: fset.Position(ts.Pos()).Line, EndLine: fset.Position(ts.End()).Line}
						if d.Doc != nil {
							gt.Doc = strings.TrimSpace(d.Doc.Text())
						}
						for _, m := range iface.Methods.List {
							if len(m.Names) == 0 {
								continue
							}
							name := m.Names[0].Name
							ft, ok := m.Type.(*ast.FuncType)
							if !ok {
								continue
							}
							fn := Func{Name: name, Recv: ts.Name.Name}
							fn.Line = fset.Position(m.Pos()).Line
							fn.EndLine = fset.Position(m.End()).Line
							if m.Doc != nil {
								fn.Doc = strings.TrimSpace(m.Doc.Text())
							}
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
							if ft.Results != nil {
								for _, r := range ft.Results.List {
									typ := exprString(fset, r.Type)
									if typ == "" || typ == "void" {
										continue
									}
									n := len(r.Names)
									if n == 0 {
										n = 1
									}
									for i := 0; i < n; i++ {
										fn.Results = append(fn.Results, typ)
									}
								}
							}
							gt.Methods = append(gt.Methods, fn)
							out.Functions = append(out.Functions, fn)
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
					for i, n := range vs.Names {
						v := Var{Name: n.Name, Type: typ, Const: d.Tok == token.CONST, Line: fset.Position(n.Pos()).Line, EndLine: fset.Position(n.End()).Line}
						if vs.Doc != nil {
							v.Doc = strings.TrimSpace(vs.Doc.Text())
						}
						if i < len(vs.Values) {
							v.Value = exprString(fset, vs.Values[i])
						}
						out.Vars = append(out.Vars, v)
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
		if t.Doc != "" {
			for _, ln := range strings.Split(t.Doc, "\n") {
				b.WriteString("// ")
				b.WriteString(ln)
				b.WriteByte('\n')
			}
		}
		b.WriteString("type ")
		b.WriteString(t.Name)
		if t.Interface {
			b.WriteString(" interface {\n")
			for _, m := range t.Methods {
				b.WriteString("  ")
				b.WriteString("fun ")
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
				if len(m.Results) == 1 {
					if m.Results[0] != "" && m.Results[0] != "void" {
						b.WriteString(": ")
						b.WriteString(m.Results[0])
					}
				} else if len(m.Results) > 1 {
					b.WriteString(": (")
					for i, r := range m.Results {
						if i > 0 {
							b.WriteString(", ")
						}
						b.WriteString(r)
					}
					b.WriteString(")")
				}
				b.WriteByte('\n')
			}
			b.WriteString("}\n")
			continue
		}
		b.WriteString(" {\n")
		for _, f := range t.Fields {
			if f.Doc != "" {
				for _, ln := range strings.Split(f.Doc, "\n") {
					b.WriteString("  // ")
					b.WriteString(ln)
					b.WriteByte('\n')
				}
			}
			b.WriteString("  ")
			name := f.Name
			if j := jsonFieldName(f.Tag); j != "" {
				name = j
			}
			if name != "" {
				b.WriteString(name)
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
		if v.Doc != "" {
			for _, ln := range strings.Split(v.Doc, "\n") {
				b.WriteString("// ")
				b.WriteString(ln)
				b.WriteByte('\n')
			}
		}
		b.WriteString("let ")
		b.WriteString(v.Name)
		if v.Type != "" {
			b.WriteString(": ")
			b.WriteString(v.Type)
		}
		if v.Value != "" {
			b.WriteString(" = ")
			b.WriteString(v.Value)
		}
		b.WriteByte('\n')
	}
	for _, fn := range g.Functions {
		if fn.Doc != "" {
			for _, ln := range strings.Split(fn.Doc, "\n") {
				b.WriteString("// ")
				b.WriteString(ln)
				b.WriteByte('\n')
			}
		}
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
		if len(fn.Results) == 1 {
			if fn.Results[0] != "" && fn.Results[0] != "void" {
				b.WriteString(": ")
				b.WriteString(fn.Results[0])
			}
		} else if len(fn.Results) > 1 {
			b.WriteString(": (")
			for i, r := range fn.Results {
				if i > 0 {
					b.WriteString(", ")
				}
				b.WriteString(r)
			}
			b.WriteString(")")
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

func jsonFieldName(tag string) string {
	tag = strings.TrimSpace(tag)
	if tag == "" {
		return ""
	}
	st := reflect.StructTag(tag)
	if j := st.Get("json"); j != "" {
		j = strings.Split(j, ",")[0]
		if j != "" && j != "-" {
			return j
		}
	}
	return ""
}
