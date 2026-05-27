package lower

import (
	"fmt"
	"strings"
	"unicode"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// Lower translates an aotir.Program to a javasrc.CompilationUnit.
func Lower(prog *aotir.Program, className string) (*javasrc.CompilationUnit, error) {
	mainFn := prog.Functions[prog.Main]

	var stmts []javasrc.Stmt
	for _, s := range mainFn.Body.Statements {
		js, err := lowerStmt(s)
		if err != nil {
			return nil, err
		}
		if js != nil {
			stmts = append(stmts, js)
		}
	}

	mainMethod := &javasrc.MethodDecl{
		Modifiers:  []string{"public", "static"},
		ReturnType: javasrc.TypeVoid,
		Name:       "main",
		Params:     []javasrc.Param{{Type: &javasrc.TypeRef{Name: "String", Array: true}, Name: "args"}},
		Body:       &javasrc.Block{Stmts: stmts},
	}

	classDecl := &javasrc.ClassDecl{
		Modifiers: []string{"public", "final"},
		Name:      className,
		Members:   []javasrc.Member{mainMethod},
	}

	cu := &javasrc.CompilationUnit{
		Package: "dev.mochi.user",
		Types:   []javasrc.TypeDecl{classDecl},
	}
	return cu, nil
}

func lowerStmt(s aotir.Stmt) (javasrc.Stmt, error) {
	switch s := s.(type) {
	case *aotir.CallStmt:
		return lowerCallStmt(s)
	case *aotir.ReturnStmt:
		if s.Value == nil {
			return &javasrc.ReturnStmt{}, nil
		}
		v, err := lowerExpr(s.Value)
		if err != nil {
			return nil, err
		}
		return &javasrc.ReturnStmt{Value: v}, nil
	default:
		return nil, fmt.Errorf("jvm/lower: unsupported stmt %T", s)
	}
}

func lowerCallStmt(s *aotir.CallStmt) (javasrc.Stmt, error) {
	switch s.Func {
	case "mochi_print_str", "mochi_print_i64", "mochi_print_f64", "mochi_print_bool":
		if len(s.Args) != 1 {
			return nil, fmt.Errorf("jvm/lower: %s wants 1 arg, got %d", s.Func, len(s.Args))
		}
		arg, err := lowerExpr(s.Args[0])
		if err != nil {
			return nil, err
		}
		call := &javasrc.StaticCallExpr{
			Class:  "dev.mochi.runtime.io.IO",
			Method: "println",
			Args:   []javasrc.Expr{arg},
		}
		return &javasrc.ExprStmt{X: call}, nil
	default:
		return nil, fmt.Errorf("jvm/lower: unsupported builtin %q", s.Func)
	}
}

func lowerExpr(e aotir.Expr) (javasrc.Expr, error) {
	switch e := e.(type) {
	case *aotir.StringLit:
		return javasrc.StringLit(e.Value), nil
	case *aotir.IntLit:
		return javasrc.LongLit(e.Value), nil
	case *aotir.BoolLit:
		return javasrc.BoolLit(e.Value), nil
	case *aotir.FloatLit:
		return javasrc.DoubleLit(e.Value), nil
	default:
		return nil, fmt.Errorf("jvm/lower: unsupported expr %T", e)
	}
}

// ClassName converts a Mochi source filename to a Java class name.
// "hello.mochi" -> "HelloMochi"
// "my_program.mochi" -> "MyProgramMochi"
func ClassName(src string) string {
	// Strip directory prefix
	for i := len(src) - 1; i >= 0; i-- {
		if src[i] == '/' || src[i] == '\\' {
			src = src[i+1:]
			break
		}
	}
	// Strip .mochi extension
	src = strings.TrimSuffix(src, ".mochi")
	// Convert snake_case to PascalCase
	parts := strings.Split(src, "_")
	var sb strings.Builder
	for _, p := range parts {
		if len(p) == 0 {
			continue
		}
		runes := []rune(p)
		runes[0] = unicode.ToUpper(runes[0])
		sb.WriteString(string(runes))
	}
	sb.WriteString("Mochi")
	return sb.String()
}
