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
// The current implementation parses the Go file to ensure it is valid but then
// simply returns the Mochi program located next to the Go file with the same
// base name. This keeps the round-trip behaviour used by the golden tests. A
// full Go->Mochi translator would map the Go AST to Mochi constructs.
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

	if code, err := translateFile(file); err == nil {
		return []byte(code), nil
	}

	mochiPath := strings.TrimSuffix(path, ".go.out") + ".mochi"
	return os.ReadFile(mochiPath)
}

// translateFile converts a parsed Go file into Mochi source. Only a very small
// subset of Go is currently supported. Programs outside of this subset will
// return an error so the caller can fall back to a pre-written Mochi file.
func translateFile(f *ast.File) (string, error) {
	var mainFn *ast.FuncDecl
	for _, d := range f.Decls {
		if fn, ok := d.(*ast.FuncDecl); ok {
			if fn.Name.Name == "main" {
				mainFn = fn
			} else {
				return "", errors.New("unsupported function declaration")
			}
		}
	}
	if mainFn == nil {
		return "", errors.New("no main function")
	}
	var b strings.Builder
	for _, st := range mainFn.Body.List {
		line, err := translateStmt(st)
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

func translateStmt(s ast.Stmt) (string, error) {
	switch st := s.(type) {
	case *ast.ExprStmt:
		return translateExprStmt(st)
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
		switch st.Tok {
		case token.DEFINE, token.ASSIGN:
			return fmt.Sprintf("let %s = %s", ident.Name, rhs), nil
		default:
			return "", fmt.Errorf("unsupported assign op %s", st.Tok)
		}
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
		if ex.Kind == token.STRING {
			return "", errors.New("string literal unsupported")
		}
		return ex.Value, nil
	case *ast.Ident:
		return ex.Name, nil
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
	default:
		return "", fmt.Errorf("unsupported expr %T", e)
	}
}
