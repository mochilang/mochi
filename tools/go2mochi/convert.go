package go2mochi

import (
	"errors"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"strings"
)

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

	code, err := translateFile(file)
	if err != nil {
		return nil, err
	}
	return []byte(code), nil
}

// translateFile converts a parsed Go file into Mochi source. Only a very small
// subset of Go is currently supported. Programs outside of this subset will
// return an error so the caller can fall back to a pre-written Mochi file.
func translateFile(f *ast.File) (string, error) {
	var b strings.Builder
	for _, d := range f.Decls {
		fn, ok := d.(*ast.FuncDecl)
		if !ok {
			return "", errors.New("unsupported declaration")
		}
		if fn.Body == nil {
			return "", errors.New("unsupported function declaration")
		}
		if fn.Name.Name == "main" {
			body, err := translateBlock(fn.Body, "")
			if err != nil {
				return "", err
			}
			if body != "" {
				b.WriteString(body)
				b.WriteByte('\n')
			}
			continue
		}
		decl, err := translateFuncDecl(fn)
		if err != nil {
			return "", err
		}
		if decl != "" {
			b.WriteString(decl)
			b.WriteByte('\n')
		}
	}
	if b.Len() == 0 {
		return "", errors.New("no main function")
	}
	return strings.TrimSuffix(b.String(), "\n"), nil
}

func translateBlock(b *ast.BlockStmt, indent string) (string, error) {
	var out strings.Builder
	for _, st := range b.List {
		line, err := translateStmt(st, indent)
		if err != nil {
			return "", err
		}
		if line != "" {
			out.WriteString(line)
			out.WriteByte('\n')
		}
	}
	return strings.TrimSuffix(out.String(), "\n"), nil
}

func translateFuncDecl(fn *ast.FuncDecl) (string, error) {
	if fn.Recv != nil {
		return "", errors.New("unsupported method")
	}
	params := make([]string, 0, len(fn.Type.Params.List))
	for _, p := range fn.Type.Params.List {
		if len(p.Names) != 1 {
			return "", errors.New("unsupported parameter")
		}
		name := p.Names[0].Name
		typ, ok := p.Type.(*ast.Ident)
		if !ok {
			return "", errors.New("unsupported parameter type")
		}
		params = append(params, fmt.Sprintf("%s: %s", name, typ.Name))
	}
	ret := ""
	if fn.Type.Results != nil {
		if len(fn.Type.Results.List) != 1 {
			return "", errors.New("unsupported result type")
		}
		typ, ok := fn.Type.Results.List[0].Type.(*ast.Ident)
		if !ok {
			return "", errors.New("unsupported result type")
		}
		ret = typ.Name
	}
	body, err := translateBlock(fn.Body, "  ")
	if err != nil {
		return "", err
	}
	if ret != "" {
		return fmt.Sprintf("fun %s(%s): %s {\n%s\n}", fn.Name.Name, strings.Join(params, ", "), ret, body), nil
	}
	return fmt.Sprintf("fun %s(%s) {\n%s\n}", fn.Name.Name, strings.Join(params, ", "), body), nil
}

func translateStmt(s ast.Stmt, indent string) (string, error) {
	switch st := s.(type) {
	case *ast.ExprStmt:
		line, err := translateExprStmt(st)
		if err != nil {
			return "", err
		}
		if line == "" {
			return "", nil
		}
		return indent + line, nil
	case *ast.AssignStmt:
		if len(st.Lhs) != 1 || len(st.Rhs) != 1 {
			return "", errors.New("unsupported assignment")
		}
		ident, ok := st.Lhs[0].(*ast.Ident)
		if !ok {
			return "", errors.New("unsupported assignment lhs")
		}
		rhs, err := translateExpr(st.Rhs[0])
		if err != nil {
			return "", err
		}
		var line string
		switch st.Tok {
		case token.DEFINE:
			line = fmt.Sprintf("let %s = %s", ident.Name, rhs)
		case token.ASSIGN:
			line = fmt.Sprintf("%s = %s", ident.Name, rhs)
		default:
			return "", fmt.Errorf("unsupported assign op %s", st.Tok)
		}
		return indent + line, nil
	case *ast.DeclStmt:
		gen, ok := st.Decl.(*ast.GenDecl)
		if !ok || gen.Tok != token.VAR || len(gen.Specs) != 1 {
			return "", errors.New("unsupported declaration")
		}
		vs, ok := gen.Specs[0].(*ast.ValueSpec)
		if !ok || len(vs.Names) != 1 || len(vs.Values) != 1 {
			return "", errors.New("unsupported var spec")
		}
		rhs, err := translateExpr(vs.Values[0])
		if err != nil {
			return "", err
		}
		return indent + fmt.Sprintf("var %s = %s", vs.Names[0].Name, rhs), nil
	case *ast.ReturnStmt:
		if len(st.Results) > 1 {
			return "", errors.New("unsupported return values")
		}
		if len(st.Results) == 0 {
			return indent + "return", nil
		}
		expr, err := translateExpr(st.Results[0])
		if err != nil {
			return "", err
		}
		return indent + fmt.Sprintf("return %s", expr), nil
	case *ast.IfStmt:
		if st.Init != nil {
			return "", errors.New("unsupported if init")
		}
		cond, err := translateExpr(st.Cond)
		if err != nil {
			return "", err
		}
		body, err := translateBlock(st.Body, indent+"  ")
		if err != nil {
			return "", err
		}
		line := fmt.Sprintf("%sif %s {\n%s\n%s}", indent, cond, body, indent)
		if st.Else != nil {
			elseBody, err := translateStmt(st.Else, indent+"  ")
			if err != nil {
				return "", err
			}
			if strings.HasPrefix(elseBody, indent+"  if") {
				elseBody = strings.TrimPrefix(elseBody, indent+"  ")
				line += " else " + strings.TrimPrefix(elseBody, indent)
			} else {
				line += " else {\n" + strings.TrimPrefix(elseBody, indent+"  ") + "\n" + indent + "}"
			}
		}
		return line, nil
	case *ast.ForStmt:
		return translateForStmt(st, indent)
	case *ast.RangeStmt:
		return translateRangeStmt(st, indent)
	default:
		return "", fmt.Errorf("unsupported statement %T", s)
	}
}

func translateExprStmt(es *ast.ExprStmt) (string, error) {
	if call, ok := es.X.(*ast.CallExpr); ok {
		if sel, ok := call.Fun.(*ast.SelectorExpr); ok {
			if pkg, ok := sel.X.(*ast.Ident); ok && pkg.Name == "fmt" && sel.Sel.Name == "Println" {
				if len(call.Args) != 1 {
					return "", errors.New("unsupported Println args")
				}
				arg, err := translateExpr(call.Args[0])
				if err != nil {
					return "", err
				}
				return fmt.Sprintf("print(%s)", arg), nil
			}
		}
	}
	return "", errors.New("unsupported expression statement")
}

func translateExpr(e ast.Expr) (string, error) {
	switch ex := e.(type) {
	case *ast.BasicLit:
		return ex.Value, nil
	case *ast.Ident:
		return ex.Name, nil
	case *ast.CallExpr:
		fun, ok := ex.Fun.(*ast.Ident)
		if !ok {
			return "", fmt.Errorf("unsupported call expr %T", ex.Fun)
		}
		args := make([]string, 0, len(ex.Args))
		for _, a := range ex.Args {
			s, err := translateExpr(a)
			if err != nil {
				return "", err
			}
			args = append(args, s)
		}
		return fmt.Sprintf("%s(%s)", fun.Name, strings.Join(args, ", ")), nil
	case *ast.BinaryExpr:
		left, err := translateExpr(ex.X)
		if err != nil {
			return "", err
		}
		right, err := translateExpr(ex.Y)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%s %s %s", left, ex.Op.String(), right), nil
	case *ast.UnaryExpr:
		x, err := translateExpr(ex.X)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%s%s", ex.Op.String(), x), nil
	case *ast.ParenExpr:
		return translateExpr(ex.X)
	case *ast.CompositeLit:
		switch ex.Type.(type) {
		case *ast.ArrayType:
			elems := make([]string, 0, len(ex.Elts))
			for _, e := range ex.Elts {
				v, err := translateExpr(e)
				if err != nil {
					return "", err
				}
				elems = append(elems, v)
			}
			return fmt.Sprintf("[%s]", strings.Join(elems, ", ")), nil
		case *ast.MapType:
			pairs := make([]string, 0, len(ex.Elts))
			for _, e := range ex.Elts {
				kv, ok := e.(*ast.KeyValueExpr)
				if !ok {
					return "", errors.New("unsupported map element")
				}
				k, err := translateExpr(kv.Key)
				if err != nil {
					return "", err
				}
				v, err := translateExpr(kv.Value)
				if err != nil {
					return "", err
				}
				pairs = append(pairs, fmt.Sprintf("%s: %s", k, v))
			}
			return fmt.Sprintf("{%s}", strings.Join(pairs, ", ")), nil
		default:
			return "", errors.New("unsupported composite literal")
		}
	default:
		return "", fmt.Errorf("unsupported expr %T", e)
	}
}

func translateForStmt(st *ast.ForStmt, indent string) (string, error) {
	if st.Init != nil && st.Post != nil && st.Cond != nil {
		init, ok := st.Init.(*ast.AssignStmt)
		if !ok || len(init.Lhs) != 1 || len(init.Rhs) != 1 {
			return "", errors.New("unsupported for init")
		}
		v, ok := init.Lhs[0].(*ast.Ident)
		if !ok {
			return "", errors.New("unsupported for init")
		}
		start, err := translateExpr(init.Rhs[0])
		if err != nil {
			return "", err
		}
		cond, ok := st.Cond.(*ast.BinaryExpr)
		if !ok || cond.Op != token.LSS {
			return "", errors.New("unsupported for cond")
		}
		if id, ok := cond.X.(*ast.Ident); !ok || id.Name != v.Name {
			return "", errors.New("unsupported for cond")
		}
		end, err := translateExpr(cond.Y)
		if err != nil {
			return "", err
		}
		if post, ok := st.Post.(*ast.IncDecStmt); !ok || post.Tok != token.INC {
			return "", errors.New("unsupported for post")
		} else if id, ok := post.X.(*ast.Ident); !ok || id.Name != v.Name {
			return "", errors.New("unsupported for post")
		}
		body, err := translateBlock(st.Body, indent+"  ")
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%sfor %s in %s..%s {\n%s\n%s}", indent, v.Name, start, end, body, indent), nil
	}
	return "", errors.New("unsupported for statement")
}

func translateRangeStmt(st *ast.RangeStmt, indent string) (string, error) {
	var name string
	if st.Value != nil {
		if id, ok := st.Value.(*ast.Ident); ok {
			name = id.Name
		} else {
			return "", errors.New("unsupported range value")
		}
	} else {
		name = "_"
	}
	expr, err := translateExpr(st.X)
	if err != nil {
		return "", err
	}
	body, err := translateBlock(st.Body, indent+"  ")
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%sfor %s in %s {\n%s\n%s}", indent, name, expr, body, indent), nil
}
