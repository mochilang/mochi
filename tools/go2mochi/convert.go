package go2mochi

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

type converter struct {
	fset       *token.FileSet
	src        []byte
	lines      []string
	typeParams map[string]bool
}

func (c *converter) pushTypeParams(fl *ast.FieldList) {
	if fl == nil {
		return
	}
	if c.typeParams == nil {
		c.typeParams = map[string]bool{}
	}
	for _, f := range fl.List {
		for _, n := range f.Names {
			c.typeParams[n.Name] = true
		}
	}
}

func (c *converter) popTypeParams(fl *ast.FieldList) {
	if fl == nil || c.typeParams == nil {
		return
	}
	for _, f := range fl.List {
		for _, n := range f.Names {
			delete(c.typeParams, n.Name)
		}
	}
}

func (c *converter) isTypeParam(name string) bool {
	return c.typeParams != nil && c.typeParams[name]
}

func (c *converter) snippet(pos token.Pos) string {
	p := c.fset.Position(pos)
	if p.Line <= 0 || p.Line > len(c.lines) {
		return ""
	}
	start := p.Line - 2
	if start < 0 {
		start = 0
	}
	end := p.Line + 2
	if end > len(c.lines) {
		end = len(c.lines)
	}
	var b strings.Builder
	for i := start; i < end; i++ {
		prefix := "    "
		if i == p.Line-1 {
			prefix = ">>> "
		}
		fmt.Fprintf(&b, "%d:%s%s\n", i+1, prefix, strings.TrimSpace(c.lines[i]))
	}
	return strings.TrimSuffix(b.String(), "\n")
}

func (c *converter) errorf(n ast.Node, format string, args ...interface{}) error {
	msg := fmt.Sprintf(format, args...)
	if n == nil {
		return fmt.Errorf("%s", msg)
	}
	pos := c.fset.Position(n.Pos())
	return fmt.Errorf("%s:%d:%d: %s\n%s", pos.Filename, pos.Line, pos.Column, msg, c.snippet(n.Pos()))
}

// Convert reads Go source from path and returns the corresponding Mochi code.
// Only a tiny subset of Go is supported. If the program cannot be translated a
// descriptive error is returned.
func Convert(path string) ([]byte, error) {
	src, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, path, src, parser.ParseComments)
	if err != nil {
		return nil, err
	}

	c := &converter{fset: fset, src: src, lines: strings.Split(string(src), "\n")}
	code, err := c.translateFile(file)
	if err != nil {
		return nil, err
	}
	return []byte(code), nil
}

// translateFile converts a parsed Go file into Mochi source. Only a very small
// subset of Go is currently supported. Programs outside of this subset will
// return an error so the caller can fall back to a pre-written Mochi file.
func (c *converter) translateFile(f *ast.File) (string, error) {
	var mainFn *ast.FuncDecl
	var b strings.Builder
	for _, d := range f.Decls {
		switch decl := d.(type) {
		case *ast.FuncDecl:
			if decl.Name.Name == "main" {
				mainFn = decl
				continue
			}
			code, err := c.translateFunc(decl)
			if err != nil {
				return "", err
			}
			if code != "" {
				b.WriteString(code)
				b.WriteByte('\n')
			}
		case *ast.GenDecl:
			// ignore import declarations
			if decl.Tok == token.IMPORT {
				continue
			}
			if decl.Tok == token.TYPE && len(decl.Specs) == 1 {
				spec, ok := decl.Specs[0].(*ast.TypeSpec)
				if !ok {
					return "", c.errorf(decl, "unsupported declaration")
				}
				code, err := c.translateTypeDecl(spec)
				if err != nil {
					return "", err
				}
				if code != "" {
					b.WriteString(code)
					b.WriteByte('\n')
				}
				continue
			}
			return "", c.errorf(decl, "unsupported declaration")
		default:
			return "", c.errorf(decl, "unsupported declaration")
		}
	}
	if mainFn == nil {
		return "", c.errorf(f, "no main function")
	}
	for _, st := range mainFn.Body.List {
		line, err := c.translateStmt(st)
		if err != nil {
			return "", err
		}
		if line != "" {
			b.WriteString(line)
			b.WriteByte('\n')
		}
	}
	return strings.TrimSuffix(b.String(), "\n"), nil
}

func (c *converter) translateFunc(fn *ast.FuncDecl) (string, error) {
	if fn.Recv != nil {
		return "", c.errorf(fn, "unsupported method declaration")
	}
	// Track generic type parameters and map them to `any` types
	c.pushTypeParams(fn.Type.TypeParams)
	defer c.popTypeParams(fn.Type.TypeParams)
	var b strings.Builder
	b.WriteString("fun ")
	b.WriteString(fn.Name.Name)
	b.WriteByte('(')
	if fn.Type.Params != nil {
		for i, p := range fn.Type.Params.List {
			if len(p.Names) != 1 {
				return "", c.errorf(p, "unsupported parameter list")
			}
			if i > 0 {
				b.WriteString(", ")
			}
			b.WriteString(p.Names[0].Name)
			if p.Type != nil {
				b.WriteString(": ")
				b.WriteString(c.typeString(p.Type))
			}
		}
	}
	b.WriteByte(')')
	if fn.Type.Results != nil && len(fn.Type.Results.List) > 0 {
		if len(fn.Type.Results.List) != 1 || len(fn.Type.Results.List[0].Names) != 0 {
			return "", c.errorf(fn.Type.Results, "unsupported multiple return values")
		}
		b.WriteString(": ")
		b.WriteString(c.typeString(fn.Type.Results.List[0].Type))
	}
	b.WriteString(" {\n")
	for _, st := range fn.Body.List {
		line, err := c.translateStmt(st)
		if err != nil {
			return "", err
		}
		if line != "" {
			b.WriteString("  ")
			b.WriteString(line)
			b.WriteByte('\n')
		}
	}
	b.WriteString("}")
	return b.String(), nil
}

func (c *converter) translateTypeDecl(ts *ast.TypeSpec) (string, error) {
	st, ok := ts.Type.(*ast.StructType)
	if !ok {
		return "", c.errorf(ts, "unsupported type spec")
	}
	var b strings.Builder
	b.WriteString("type ")
	b.WriteString(ts.Name.Name)
	b.WriteString(" {\n")
	for _, f := range st.Fields.List {
		if len(f.Names) != 1 {
			return "", c.errorf(f, "unsupported field list")
		}
		name := f.Names[0].Name
		typ := c.typeString(f.Type)
		fmt.Fprintf(&b, "  %s: %s\n", name, typ)
	}
	b.WriteString("}")
	return b.String(), nil
}

func nodeString(fset *token.FileSet, n ast.Node) string {
	var buf bytes.Buffer
	printer.Fprint(&buf, fset, n)
	return buf.String()
}

func (c *converter) typeString(n ast.Expr) string {
	switch t := n.(type) {
	case *ast.ArrayType:
		return fmt.Sprintf("list<%s>", c.typeString(t.Elt))
	case *ast.Ident:
		if c.isTypeParam(t.Name) {
			return "any"
		}
		return t.Name
	case *ast.MapType:
		return fmt.Sprintf("map<%s, %s>", c.typeString(t.Key), c.typeString(t.Value))
	default:
		return nodeString(c.fset, n)
	}
}

func (c *converter) translateStmt(s ast.Stmt) (string, error) {
	switch st := s.(type) {
	case *ast.ExprStmt:
		return c.translateExprStmt(st)
	case *ast.AssignStmt:
		if len(st.Lhs) != 1 || len(st.Rhs) != 1 {
			return "", c.errorf(st, "unsupported assignment")
		}
		rhs, err := c.translateExpr(st.Rhs[0])
		if err != nil {
			return "", err
		}
		switch lhs := st.Lhs[0].(type) {
		case *ast.Ident:
			switch st.Tok {
			case token.DEFINE:
				return fmt.Sprintf("let %s = %s", lhs.Name, rhs), nil
			case token.ASSIGN:
				return fmt.Sprintf("%s = %s", lhs.Name, rhs), nil
			case token.ADD_ASSIGN, token.SUB_ASSIGN, token.MUL_ASSIGN, token.QUO_ASSIGN, token.REM_ASSIGN:
				op := st.Tok.String()[:1]
				return fmt.Sprintf("%s = %s %s %s", lhs.Name, lhs.Name, op, rhs), nil
			default:
				return "", c.errorf(st, "unsupported assign op %s", st.Tok)
			}
		case *ast.IndexExpr:
			target, err := c.translateExpr(lhs.X)
			if err != nil {
				return "", err
			}
			idx, err := c.translateExpr(lhs.Index)
			if err != nil {
				return "", err
			}
			if st.Tok != token.ASSIGN {
				return "", c.errorf(st, "unsupported assign op %s", st.Tok)
			}
			return fmt.Sprintf("%s[%s] = %s", target, idx, rhs), nil
		default:
			return "", c.errorf(lhs, "unsupported assignment lhs")
		}
	case *ast.DeclStmt:
		gen, ok := st.Decl.(*ast.GenDecl)
		if !ok || gen.Tok != token.VAR || len(gen.Specs) != 1 {
			return "", c.errorf(st, "unsupported declaration")
		}
		vs, ok := gen.Specs[0].(*ast.ValueSpec)
		if !ok || len(vs.Names) != 1 {
			return "", c.errorf(st, "unsupported var spec")
		}
		name := vs.Names[0].Name
		if len(vs.Values) == 1 {
			rhs, err := c.translateExpr(vs.Values[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("var %s = %s", name, rhs), nil
		}
		if len(vs.Values) == 0 && vs.Type != nil {
			return fmt.Sprintf("var %s: %s", name, c.typeString(vs.Type)), nil
		}
		return "", c.errorf(st, "unsupported var spec")
	case *ast.ReturnStmt:
		if len(st.Results) == 0 {
			return "return", nil
		}
		if len(st.Results) != 1 {
			return "", c.errorf(st, "unsupported multiple return values")
		}
		r, err := c.translateExpr(st.Results[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("return %s", r), nil
	case *ast.IfStmt:
		cond, err := c.translateExpr(st.Cond)
		if err != nil {
			return "", err
		}
		var b strings.Builder
		b.WriteString("if ")
		b.WriteString(cond)
		b.WriteString(" {\n")
		for _, s2 := range st.Body.List {
			line, err := c.translateStmt(s2)
			if err != nil {
				return "", err
			}
			if line != "" {
				b.WriteString("  ")
				b.WriteString(line)
				b.WriteByte('\n')
			}
		}
		b.WriteString("}")
		if st.Else != nil {
			switch e := st.Else.(type) {
			case *ast.BlockStmt:
				b.WriteString(" else {\n")
				for _, s2 := range e.List {
					line, err := c.translateStmt(s2)
					if err != nil {
						return "", err
					}
					if line != "" {
						b.WriteString("  ")
						b.WriteString(line)
						b.WriteByte('\n')
					}
				}
				b.WriteString("}")
			default:
				return "", c.errorf(st.Else, "unsupported else")
			}
		}
		return b.String(), nil
	case *ast.BranchStmt:
		switch st.Tok {
		case token.BREAK:
			return "break", nil
		case token.CONTINUE:
			return "continue", nil
		default:
			return "", c.errorf(st, "unsupported branch %s", st.Tok)
		}
	case *ast.ForStmt:
		if st.Init == nil && st.Post == nil {
			cond := "true"
			var err error
			if st.Cond != nil {
				cond, err = c.translateExpr(st.Cond)
				if err != nil {
					return "", err
				}
			}
			var b strings.Builder
			b.WriteString("while ")
			b.WriteString(cond)
			b.WriteString(" {\n")
			for _, s2 := range st.Body.List {
				line, err := c.translateStmt(s2)
				if err != nil {
					return "", err
				}
				if line != "" {
					b.WriteString("  ")
					b.WriteString(line)
					b.WriteByte('\n')
				}
			}
			b.WriteString("}")
			return b.String(), nil
		}
		// try for i := a; i < b; i++ pattern
		init, ok := st.Init.(*ast.AssignStmt)
		if !ok || len(init.Lhs) != 1 || len(init.Rhs) != 1 || init.Tok != token.DEFINE {
			return "", c.errorf(st, "unsupported for clause")
		}
		ivar, ok := init.Lhs[0].(*ast.Ident)
		if !ok {
			return "", c.errorf(init.Lhs[0], "unsupported for init")
		}
		be, ok := st.Cond.(*ast.BinaryExpr)
		if !ok || be.Op != token.LSS {
			return "", c.errorf(st.Cond, "unsupported for condition")
		}
		if xid, ok := be.X.(*ast.Ident); !ok || xid.Name != ivar.Name {
			return "", c.errorf(be.X, "unsupported for condition")
		}
		inc, ok := st.Post.(*ast.IncDecStmt)
		if !ok || inc.Tok != token.INC {
			return "", c.errorf(st.Post, "unsupported for post")
		}
		if xid, ok := inc.X.(*ast.Ident); !ok || xid.Name != ivar.Name {
			return "", c.errorf(inc.X, "unsupported for post")
		}
		start, err := c.translateExpr(init.Rhs[0])
		if err != nil {
			return "", err
		}
		end, err := c.translateExpr(be.Y)
		if err != nil {
			return "", err
		}
		var b strings.Builder
		b.WriteString("for ")
		b.WriteString(ivar.Name)
		b.WriteString(" in ")
		b.WriteString(fmt.Sprintf("%s..%s", start, end))
		b.WriteString(" {\n")
		for _, s2 := range st.Body.List {
			line, err := c.translateStmt(s2)
			if err != nil {
				return "", err
			}
			if line != "" {
				b.WriteString("  ")
				b.WriteString(line)
				b.WriteByte('\n')
			}
		}
		b.WriteString("}")
		return b.String(), nil
	case *ast.RangeStmt:
		var b strings.Builder
		b.WriteString("for ")
		names := []string{}
		if ident, ok := st.Key.(*ast.Ident); ok && ident.Name != "_" {
			names = append(names, ident.Name)
		}
		if ident, ok := st.Value.(*ast.Ident); ok && ident.Name != "_" {
			names = append(names, ident.Name)
		}
		if len(names) > 0 {
			b.WriteString(strings.Join(names, ", "))
			b.WriteString(" in ")
		} else {
			b.WriteString("_ in ")
		}
		expr, err := c.translateExpr(st.X)
		if err != nil {
			return "", err
		}
		b.WriteString(expr)
		b.WriteString(" {\n")
		for _, s2 := range st.Body.List {
			line, err := c.translateStmt(s2)
			if err != nil {
				return "", err
			}
			if line != "" {
				b.WriteString("  ")
				b.WriteString(line)
				b.WriteByte('\n')
			}
		}
		b.WriteString("}")
		return b.String(), nil
	case *ast.SwitchStmt:
		if st.Init != nil {
			return "", c.errorf(st.Init, "unsupported switch init")
		}
		var tag string
		var err error
		if st.Tag != nil {
			tag, err = c.translateExpr(st.Tag)
			if err != nil {
				return "", err
			}
		}
		var b strings.Builder
		for i, cl := range st.Body.List {
			cc, ok := cl.(*ast.CaseClause)
			if !ok {
				return "", c.errorf(cl, "unsupported switch clause")
			}
			if i == 0 {
				b.WriteString("if ")
			} else {
				if len(cc.List) == 0 {
					b.WriteString("else {\n")
				} else {
					b.WriteString("else if ")
				}
			}
			if len(cc.List) > 0 {
				conds := make([]string, len(cc.List))
				for j, e := range cc.List {
					cnd, err := c.translateExpr(e)
					if err != nil {
						return "", err
					}
					if tag != "" {
						conds[j] = fmt.Sprintf("%s == %s", tag, cnd)
					} else {
						conds[j] = cnd
					}
				}
				b.WriteString(strings.Join(conds, " || "))
				b.WriteString(" {\n")
			} else if i == 0 {
				b.WriteString("true {\n")
			}
			for _, st2 := range cc.Body {
				if br, ok := st2.(*ast.BranchStmt); ok && br.Tok == token.FALLTHROUGH {
					return "", c.errorf(br, "unsupported fallthrough")
				}
				line, err := c.translateStmt(st2)
				if err != nil {
					return "", err
				}
				if line != "" {
					b.WriteString("  ")
					b.WriteString(line)
					b.WriteByte('\n')
				}
			}
			b.WriteString("}")
			if i < len(st.Body.List)-1 && len(cc.List) > 0 {
				b.WriteString(" ")
			}
		}
		return b.String(), nil
	default:
		return "", c.errorf(st, "unsupported statement %T", s)
	}
}

func (c *converter) translateExprStmt(es *ast.ExprStmt) (string, error) {
	if call, ok := es.X.(*ast.CallExpr); ok {
		if sel, ok := call.Fun.(*ast.SelectorExpr); ok {
			if pkg, ok := sel.X.(*ast.Ident); ok && pkg.Name == "fmt" && sel.Sel.Name == "Scanln" {
				if len(call.Args) == 1 {
					if u, ok := call.Args[0].(*ast.UnaryExpr); ok && u.Op == token.AND {
						if id, ok := u.X.(*ast.Ident); ok {
							return fmt.Sprintf("%s = input()", id.Name), nil
						}
					}
				}
				return "", c.errorf(call, "unsupported Scanln args")
			}
		}
	}
	expr, err := c.translateExpr(es.X)
	if err != nil {
		return "", err
	}
	if expr == "" {
		return "", c.errorf(es, "unsupported expression statement")
	}
	return expr, nil
}

func (c *converter) translateExpr(e ast.Expr) (string, error) {
	if e == nil {
		return "", nil
	}
	switch ex := e.(type) {
	case *ast.FuncLit:
		// Ignore generic type parameters on function literals
		var b strings.Builder
		b.WriteString("fun (")
		if ex.Type.Params != nil {
			for i, p := range ex.Type.Params.List {
				if len(p.Names) != 1 {
					return "", c.errorf(p, "unsupported parameter list")
				}
				if i > 0 {
					b.WriteString(", ")
				}
				b.WriteString(p.Names[0].Name)
				if p.Type != nil {
					b.WriteString(": ")
					b.WriteString(c.typeString(p.Type))
				}
			}
		}
		b.WriteByte(')')
		if ex.Type.Results != nil && len(ex.Type.Results.List) > 0 {
			if len(ex.Type.Results.List) != 1 || len(ex.Type.Results.List[0].Names) != 0 {
				return "", c.errorf(ex.Type.Results, "unsupported multiple return values")
			}
			b.WriteString(": ")
			b.WriteString(c.typeString(ex.Type.Results.List[0].Type))
		}
		b.WriteString(" {\n")
		for _, st := range ex.Body.List {
			line, err := c.translateStmt(st)
			if err != nil {
				return "", err
			}
			if line != "" {
				b.WriteString("  ")
				b.WriteString(line)
				b.WriteByte('\n')
			}
		}
		b.WriteString("}")
		return b.String(), nil
	case *ast.BasicLit:
		return ex.Value, nil
	case *ast.Ident:
		return ex.Name, nil
	case *ast.SelectorExpr:
		x, err := c.translateExpr(ex.X)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%s.%s", x, ex.Sel.Name), nil
	case *ast.BinaryExpr:
		left, err := c.translateExpr(ex.X)
		if err != nil {
			return "", err
		}
		right, err := c.translateExpr(ex.Y)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%s %s %s", left, ex.Op.String(), right), nil
	case *ast.UnaryExpr:
		x, err := c.translateExpr(ex.X)
		if err != nil {
			return "", err
		}
		switch ex.Op {
		case token.NOT, token.SUB, token.ADD:
			return fmt.Sprintf("%s%s", ex.Op.String(), x), nil
		default:
			return "", c.errorf(ex, "unsupported unary op %s", ex.Op)
		}
	case *ast.ParenExpr:
		inner, err := c.translateExpr(ex.X)
		if err != nil {
			return "", err
		}
		switch ex.X.(type) {
		case *ast.BasicLit, *ast.Ident:
			return inner, nil
		default:
			return fmt.Sprintf("(%s)", inner), nil
		}
	case *ast.CallExpr:
		if idx, ok := ex.Fun.(*ast.IndexExpr); ok {
			ex = &ast.CallExpr{Fun: idx.X, Args: ex.Args}
		}
		if il, ok := ex.Fun.(*ast.IndexListExpr); ok {
			ex = &ast.CallExpr{Fun: il.X, Args: ex.Args}
		}
		if sel, ok := ex.Fun.(*ast.SelectorExpr); ok {
			if pkg, ok := sel.X.(*ast.Ident); ok && pkg.Name == "fmt" && sel.Sel.Name == "Println" {
				parts := make([]string, len(ex.Args))
				for i, a := range ex.Args {
					v, err := c.translateExpr(a)
					if err != nil {
						return "", err
					}
					parts[i] = fmt.Sprintf("str(%s)", v)
				}
				join := strings.Join(parts, " + \" \" + ")
				return fmt.Sprintf("print(%s)", join), nil
			}
			if pkg, ok := sel.X.(*ast.Ident); ok && pkg.Name == "strings" {
				switch sel.Sel.Name {
				case "ToLower":
					if len(ex.Args) != 1 {
						return "", c.errorf(ex, "unsupported ToLower args")
					}
					arg, err := c.translateExpr(ex.Args[0])
					if err != nil {
						return "", err
					}
					return fmt.Sprintf("lower(%s)", arg), nil
				case "ToUpper":
					if len(ex.Args) != 1 {
						return "", c.errorf(ex, "unsupported ToUpper args")
					}
					arg, err := c.translateExpr(ex.Args[0])
					if err != nil {
						return "", err
					}
					return fmt.Sprintf("upper(%s)", arg), nil
				}
			}
		}
		if arr, ok := ex.Fun.(*ast.ArrayType); ok && len(ex.Args) == 1 {
			// handle []rune("foo") -> "foo"
			if id, ok := arr.Elt.(*ast.Ident); ok && id.Name == "rune" {
				return c.translateExpr(ex.Args[0])
			}
		}
		if id, ok := ex.Fun.(*ast.Ident); ok {
			switch id.Name {
			case "append":
				if len(ex.Args) < 2 {
					return "", c.errorf(ex, "unsupported append args")
				}
				base, err := c.translateExpr(ex.Args[0])
				if err != nil {
					return "", err
				}
				items := make([]string, len(ex.Args)-1)
				for i, a := range ex.Args[1:] {
					v, err := c.translateExpr(a)
					if err != nil {
						return "", err
					}
					items[i] = v
				}
				if ex.Ellipsis != token.NoPos {
					if len(items) != 1 {
						return "", c.errorf(ex, "unsupported append ellipsis")
					}
					return fmt.Sprintf("concat(%s, %s)", base, items[0]), nil
				}
				out := base
				for _, it := range items {
					out = fmt.Sprintf("append(%s, %s)", out, it)
				}
				return out, nil
			case "string":
				if len(ex.Args) != 1 {
					return "", c.errorf(ex, "unsupported string cast")
				}
				arg, err := c.translateExpr(ex.Args[0])
				if err != nil {
					return "", err
				}
				return fmt.Sprintf("str(%s)", arg), nil
			}
		}
		fun, err := c.translateExpr(ex.Fun)
		if err != nil {
			return "", err
		}
		args := make([]string, len(ex.Args))
		for i, a := range ex.Args {
			v, err := c.translateExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = v
		}
		return fmt.Sprintf("%s(%s)", fun, strings.Join(args, ", ")), nil
	case *ast.IndexExpr:
		x, err := c.translateExpr(ex.X)
		if err != nil {
			return "", err
		}
		idx, err := c.translateExpr(ex.Index)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%s[%s]", x, idx), nil
	case *ast.SliceExpr:
		x, err := c.translateExpr(ex.X)
		if err != nil {
			return "", err
		}
		var low, high string
		if ex.Low != nil {
			low, err = c.translateExpr(ex.Low)
			if err != nil {
				return "", err
			}
		}
		if ex.High != nil {
			high, err = c.translateExpr(ex.High)
			if err != nil {
				return "", err
			}
		}
		return fmt.Sprintf("%s[%s:%s]", x, low, high), nil
	case *ast.CompositeLit:
		if _, ok := ex.Type.(*ast.ArrayType); ok {
			items := make([]string, len(ex.Elts))
			for i, el := range ex.Elts {
				v, err := c.translateExpr(el)
				if err != nil {
					return "", err
				}
				items[i] = v
			}
			return "[" + strings.Join(items, ", ") + "]", nil
		}
		if mp, ok := ex.Type.(*ast.MapType); ok {
			pairs := make([]string, len(ex.Elts))
			for i, el := range ex.Elts {
				kv, ok := el.(*ast.KeyValueExpr)
				if !ok {
					return "", c.errorf(el, "unsupported map element")
				}
				k, err := c.translateExpr(kv.Key)
				if err != nil {
					return "", err
				}
				v, err := c.translateExpr(kv.Value)
				if err != nil {
					return "", err
				}
				pairs[i] = fmt.Sprintf("%s: %s", k, v)
			}
			_ = mp // ignore types
			return "{" + strings.Join(pairs, ", ") + "}", nil
		}
		if ident, ok := ex.Type.(*ast.Ident); ok {
			fields := make([]string, len(ex.Elts))
			for i, el := range ex.Elts {
				kv, ok := el.(*ast.KeyValueExpr)
				if !ok {
					return "", c.errorf(el, "unsupported struct element")
				}
				k := kv.Key.(*ast.Ident).Name
				v, err := c.translateExpr(kv.Value)
				if err != nil {
					return "", err
				}
				fields[i] = fmt.Sprintf("%s: %s", k, v)
			}
			return fmt.Sprintf("%s { %s }", ident.Name, strings.Join(fields, ", ")), nil
		}
		return "", c.errorf(ex, "unsupported composite literal")
	default:
		return "", c.errorf(ex, "unsupported expr %T", e)
	}
}
