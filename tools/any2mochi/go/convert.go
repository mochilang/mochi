package golang

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

// Convert converts Go source code to Mochi without relying on a
// language server. It parses the source using the standard library
// and emits minimal Mochi stubs.
func Convert(src string) ([]byte, error) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "src.go", src, parser.SkipObjectResolution)
	if err != nil {
		return nil, err
	}
	var out strings.Builder
	for _, decl := range file.Decls {
		switch d := decl.(type) {
		case *ast.FuncDecl:
			writeFunc(&out, fset, d)
		case *ast.GenDecl:
			writeGenDecl(&out, fset, d)
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\n%s", detailedSnippet(src, 0))
	}
	return []byte(out.String()), nil
}

// ConvertFile reads the Go file at path and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

func writeFunc(out *strings.Builder, fset *token.FileSet, fn *ast.FuncDecl) {
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
	if fn.Type.Results != nil {
		var rets []string
		for _, r := range fn.Type.Results.List {
			typ := exprString(fset, r.Type)
			if typ == "" || typ == "void" {
				continue
			}
			n := len(r.Names)
			if n == 0 {
				n = 1
			}
			for i := 0; i < n; i++ {
				rets = append(rets, typ)
			}
		}
		if len(rets) == 1 {
			out.WriteString(": ")
			out.WriteString(rets[0])
		} else if len(rets) > 1 {
			out.WriteString(": (")
			out.WriteString(strings.Join(rets, ", "))
			out.WriteString(")")
		}
	}
	out.WriteString(" {}\n")
}

func writeGenDecl(out *strings.Builder, fset *token.FileSet, d *ast.GenDecl) {
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
			for i, n := range vs.Names {
				out.WriteString("let ")
				out.WriteString(n.Name)
				if typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				if i < len(vs.Values) {
					if val, ok := exprToMochi(fset, vs.Values[i]); ok {
						out.WriteString(" = ")
						out.WriteString(val)
					}
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

func exprToMochi(fset *token.FileSet, e ast.Expr) (string, bool) {
	switch ex := e.(type) {
	case *ast.BasicLit:
		return ex.Value, true
	case *ast.CompositeLit:
		switch t := ex.Type.(type) {
		case *ast.ArrayType:
			parts := make([]string, len(ex.Elts))
			for i, el := range ex.Elts {
				v, ok := exprToMochi(fset, el)
				if !ok {
					return "", false
				}
				parts[i] = v
			}
			return "[" + strings.Join(parts, ", ") + "]", true
		case *ast.Ident:
			fields := make([]string, len(ex.Elts))
			for i, el := range ex.Elts {
				kv, ok := el.(*ast.KeyValueExpr)
				if !ok {
					return "", false
				}
				key := exprString(fset, kv.Key)
				val, ok := exprToMochi(fset, kv.Value)
				if !ok {
					return "", false
				}
				fields[i] = fmt.Sprintf("%s: %s", key, val)
			}
			return fmt.Sprintf("%s { %s }", t.Name, strings.Join(fields, ", ")), true
		}
	}
	return exprString(fset, e), true
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

func detailedSnippet(src string, line int) string {
	const ctx = 2
	lines := strings.Split(src, "\n")
	start := line - ctx
	if start < 0 {
		start = 0
	}
	end := line + ctx
	if end >= len(lines) {
		end = len(lines) - 1
	}
	var b strings.Builder
	for i := start; i <= end; i++ {
		prefix := "    "
		if i == line {
			prefix = ">>> "
		}
		fmt.Fprintf(&b, "%d:%s%s\n", i+1, prefix, strings.TrimSpace(lines[i]))
	}
	return b.String()
}
