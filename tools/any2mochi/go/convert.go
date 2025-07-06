package goconv

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"os"
	"strings"
)

// ConvertGo converts Go source code to Mochi without relying on a
// language server. It parses the source using the standard library
// and emits minimal Mochi stubs.
func ConvertGo(src string) ([]byte, error) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "src.go", src, parser.SkipObjectResolution)
	if err != nil {
		return nil, err
	}
	var out strings.Builder
	for _, decl := range file.Decls {
		switch d := decl.(type) {
		case *ast.FuncDecl:
			writeGoFunc(&out, fset, d)
		case *ast.GenDecl:
			writeGoGenDecl(&out, fset, d)
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func numberedSnippet(src string) string {
	lines := strings.Split(src, "\n")
	if len(lines) > 10 {
		lines = lines[:10]
	}
	for i, l := range lines {
		lines[i] = fmt.Sprintf("%3d: %s", i+1, l)
	}
	return strings.Join(lines, "\n")
}

// ConvertGoFile reads the Go file at path and converts it to Mochi.
func ConvertGoFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertGo(string(data))
}

type goParam struct{ name, typ string }

func writeGoFunc(out *strings.Builder, fset *token.FileSet, fn *ast.FuncDecl) {
	if fn.Name == nil {
		return
	}
	name := fn.Name.Name
	if fn.Recv != nil && len(fn.Recv.List) > 0 {
		recv := exprString(fset, fn.Recv.List[0].Type)
		name = recv + "." + name
	}
	out.WriteString("fun ")
	out.WriteString(name)
	out.WriteByte('(')
	first := true
	if fn.Type.Params != nil {
		for _, p := range fn.Type.Params.List {
			typ := exprString(fset, p.Type)
			if len(p.Names) == 0 {
				if !first {
					out.WriteString(", ")
				}
				out.WriteString("_")
				if typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				first = false
				continue
			}
			for _, n := range p.Names {
				if !first {
					out.WriteString(", ")
				}
				out.WriteString(n.Name)
				if typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				first = false
			}
		}
	}
	out.WriteByte(')')
	if fn.Type.Results != nil && len(fn.Type.Results.List) == 1 && len(fn.Type.Results.List[0].Names) == 0 {
		ret := exprString(fset, fn.Type.Results.List[0].Type)
		if ret != "" && ret != "void" {
			out.WriteString(": ")
			out.WriteString(ret)
		}
	}
	out.WriteString(" {}\n")
}

func writeGoGenDecl(out *strings.Builder, fset *token.FileSet, d *ast.GenDecl) {
	switch d.Tok {
	case token.TYPE:
		for _, spec := range d.Specs {
			ts, ok := spec.(*ast.TypeSpec)
			if !ok {
				continue
			}
			if st, ok := ts.Type.(*ast.StructType); ok {
				out.WriteString("type ")
				out.WriteString(ts.Name.Name)
				out.WriteString(" {\n")
				for _, f := range st.Fields.List {
					typ := exprString(fset, f.Type)
					if len(f.Names) == 0 {
						out.WriteString("  ")
						out.WriteString(typ)
						out.WriteByte('\n')
						continue
					}
					for _, n := range f.Names {
						out.WriteString("  ")
						out.WriteString(n.Name)
						if typ != "" {
							out.WriteString(": ")
							out.WriteString(typ)
						}
						out.WriteByte('\n')
					}
				}
				out.WriteString("}\n")
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
				out.WriteString("let ")
				out.WriteString(n.Name)
				if typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
		}
	}
}

func exprString(fset *token.FileSet, e ast.Expr) string {
	var b bytes.Buffer
	printer.Fprint(&b, fset, e)
	return b.String()
}
