// Package lower translates an aotir.Program into a ptree.PhpFile.
// Entry point: Lower(prog, colours) → *ptree.PhpFile.
//
// Phase 0 ships an empty `mochi_main()` no-op plus a trailing call.
// Phase 1 wires CallStmt for the four print runtime entries by emitting
// matching `mochi_print_*` PHP helpers inline (Phase 15 will switch
// these to a Composer-autoloaded \Mochi\Runtime\IO). Phase 2 adds
// scalar literals + arithmetic, and so on.
package lower

import (
	"fmt"
	"path/filepath"
	"strings"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/php/colour"
	"mochi/transpiler3/php/ptree"
)

// runtimeFlags tracks which inline runtime helpers the lowered program
// needs so the emit pass only includes the ones that are actually used.
type runtimeFlags struct {
	printStr  bool
	printInt  bool
	printBool bool
	printF64  bool
}

type lowerer struct {
	runtime runtimeFlags
}

// Lower translates an aotir.Program into a ptree.PhpFile. The returned
// file represents a single .php source file (named "main.php" by the
// emit pass) containing the inline runtime helpers the lowered body
// needs, one `mochi_main()` function, and one trailing call site.
func Lower(prog *aotir.Program, _ colour.ColourMap) (*ptree.PhpFile, error) {
	if prog == nil {
		return nil, fmt.Errorf("php lower: nil program")
	}
	if prog.Main < 0 || prog.Main >= len(prog.Functions) {
		return nil, fmt.Errorf("php lower: invalid Main index %d", prog.Main)
	}
	l := &lowerer{}
	mainFn := prog.Functions[prog.Main]
	body, err := l.lowerBlock(mainFn.Body)
	if err != nil {
		return nil, err
	}
	mainDecl := &ptree.FuncDecl{
		PhpDoc:     []string{"Generated Mochi entry point. Do not edit by hand."},
		Name:       "mochi_main",
		ReturnType: "void",
		Body:       body,
	}

	decls := l.runtimeDecls()
	decls = append(decls, mainDecl)

	file := &ptree.PhpFile{
		// Phase 0/1 keep a global-namespace file. Phase 15 will add a
		// real PSR-4 namespace when the Composer package lands.
		Namespace: "",
		Uses:      nil,
		Decls:     decls,
		TrailingExec: []ptree.Stmt{
			&ptree.ExprStmt{
				Expr: &ptree.CallExpr{
					Callee: &ptree.IdentExpr{Name: "mochi_main"},
				},
			},
		},
	}
	return file, nil
}

// runtimeDecls emits FuncDecl entries for each inline runtime helper
// the lowered body requested. The order matches the C aotir naming
// (mochi_print_str / i64 / f64 / bool) for review-friendly diffs.
func (l *lowerer) runtimeDecls() []ptree.Decl {
	var out []ptree.Decl
	if l.runtime.printStr {
		out = append(out, &ptree.FuncDecl{
			PhpDoc:     []string{"Print a string followed by a newline (vm3 println contract)."},
			Name:       "mochi_print_str",
			Params:     []ptree.FuncParam{{TypeName: "string", Name: "value"}},
			ReturnType: "void",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `echo $value, "\n";`},
			},
		})
	}
	if l.runtime.printInt {
		out = append(out, &ptree.FuncDecl{
			PhpDoc:     []string{"Print a 64-bit signed integer followed by a newline."},
			Name:       "mochi_print_i64",
			Params:     []ptree.FuncParam{{TypeName: "int", Name: "value"}},
			ReturnType: "void",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `echo $value, "\n";`},
			},
		})
	}
	if l.runtime.printF64 {
		out = append(out, &ptree.FuncDecl{
			PhpDoc:     []string{"Print a 64-bit float followed by a newline (Go 'g' -1 64 format)."},
			Name:       "mochi_print_f64",
			Params:     []ptree.FuncParam{{TypeName: "float", Name: "value"}},
			ReturnType: "void",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `if (is_nan($value)) { echo "NaN\n"; return; }`},
				&ptree.RawStmt{Text: `if (is_infinite($value)) { echo $value < 0 ? "-Inf\n" : "+Inf\n"; return; }`},
				&ptree.RawStmt{Text: `if ((float) (int) $value === $value && abs($value) < 1.0e15) { echo (int) $value, "\n"; return; }`},
				&ptree.RawStmt{Text: `echo $value, "\n";`},
			},
		})
	}
	if l.runtime.printBool {
		out = append(out, &ptree.FuncDecl{
			PhpDoc:     []string{"Print a bool as the lowercase literal (vm3 contract), followed by a newline."},
			Name:       "mochi_print_bool",
			Params:     []ptree.FuncParam{{TypeName: "bool", Name: "value"}},
			ReturnType: "void",
			Body: []ptree.Stmt{
				&ptree.RawStmt{Text: `echo $value ? "true\n" : "false\n";`},
			},
		})
	}
	return out
}

// lowerBlock translates an aotir.Block to a list of PHP statements.
func (l *lowerer) lowerBlock(b *aotir.Block) ([]ptree.Stmt, error) {
	if b == nil {
		return nil, nil
	}
	var out []ptree.Stmt
	for _, st := range b.Statements {
		stmts, err := l.lowerStmt(st)
		if err != nil {
			return nil, err
		}
		out = append(out, stmts...)
	}
	return out, nil
}

// lowerStmt translates one aotir statement. Phase 1 only handles the
// four print CallStmt forms; later phases extend the switch with
// LetStmt, AssignStmt, control-flow, etc.
func (l *lowerer) lowerStmt(s aotir.Stmt) ([]ptree.Stmt, error) {
	switch v := s.(type) {
	case *aotir.CallStmt:
		return l.lowerCallStmt(v)
	default:
		return nil, fmt.Errorf("php lower: phase 1 cannot lower %T", s)
	}
}

// lowerCallStmt maps a runtime print call to its inline PHP helper.
// User-defined function calls are deferred to Phase 6+ (Mochi Phase 2.2
// in the C transpiler) and rejected here so misroutes are visible.
func (l *lowerer) lowerCallStmt(s *aotir.CallStmt) ([]ptree.Stmt, error) {
	switch s.Func {
	case "mochi_print_str":
		if len(s.Args) != 1 {
			return nil, fmt.Errorf("php lower: %s wants 1 arg, got %d", s.Func, len(s.Args))
		}
		arg, err := l.lowerExpr(s.Args[0])
		if err != nil {
			return nil, err
		}
		l.runtime.printStr = true
		return []ptree.Stmt{
			&ptree.ExprStmt{Expr: &ptree.CallExpr{Callee: &ptree.IdentExpr{Name: "mochi_print_str"}, Args: []ptree.Expr{arg}}},
		}, nil
	case "mochi_print_i64":
		if len(s.Args) != 1 {
			return nil, fmt.Errorf("php lower: %s wants 1 arg, got %d", s.Func, len(s.Args))
		}
		arg, err := l.lowerExpr(s.Args[0])
		if err != nil {
			return nil, err
		}
		l.runtime.printInt = true
		return []ptree.Stmt{
			&ptree.ExprStmt{Expr: &ptree.CallExpr{Callee: &ptree.IdentExpr{Name: "mochi_print_i64"}, Args: []ptree.Expr{arg}}},
		}, nil
	case "mochi_print_f64":
		if len(s.Args) != 1 {
			return nil, fmt.Errorf("php lower: %s wants 1 arg, got %d", s.Func, len(s.Args))
		}
		arg, err := l.lowerExpr(s.Args[0])
		if err != nil {
			return nil, err
		}
		l.runtime.printF64 = true
		return []ptree.Stmt{
			&ptree.ExprStmt{Expr: &ptree.CallExpr{Callee: &ptree.IdentExpr{Name: "mochi_print_f64"}, Args: []ptree.Expr{arg}}},
		}, nil
	case "mochi_print_bool":
		if len(s.Args) != 1 {
			return nil, fmt.Errorf("php lower: %s wants 1 arg, got %d", s.Func, len(s.Args))
		}
		arg, err := l.lowerExpr(s.Args[0])
		if err != nil {
			return nil, err
		}
		l.runtime.printBool = true
		return []ptree.Stmt{
			&ptree.ExprStmt{Expr: &ptree.CallExpr{Callee: &ptree.IdentExpr{Name: "mochi_print_bool"}, Args: []ptree.Expr{arg}}},
		}, nil
	default:
		return nil, fmt.Errorf("php lower: phase 1 cannot lower CallStmt to %q", s.Func)
	}
}

// lowerExpr translates one aotir expression. Phase 1 only supports the
// four scalar literal forms (StringLit, IntLit, FloatLit, BoolLit).
func (l *lowerer) lowerExpr(e aotir.Expr) (ptree.Expr, error) {
	switch v := e.(type) {
	case *aotir.StringLit:
		return &ptree.StringLit{Value: v.Value}, nil
	case *aotir.IntLit:
		return &ptree.IntLit{Value: v.Value}, nil
	case *aotir.FloatLit:
		return &ptree.FloatLit{Value: v.Value}, nil
	case *aotir.BoolLit:
		return &ptree.BoolLit{Value: v.Value}, nil
	default:
		return nil, fmt.Errorf("php lower: phase 1 cannot lower %T", e)
	}
}

// ModuleName converts a Mochi source filename to a PSR-4 module name.
// Phase 0/1 return the base name without the .mochi suffix; Phase 15
// will mangle this further when the Composer namespace is wired.
//
//	"hello.mochi"       -> "Hello"
//	"hello_world.mochi" -> "HelloWorld"
func ModuleName(src string) string {
	src = filepath.Base(src)
	src = strings.TrimSuffix(src, ".mochi")
	parts := strings.FieldsFunc(src, func(r rune) bool {
		return r == '_' || r == '-'
	})
	var sb strings.Builder
	for _, p := range parts {
		if len(p) == 0 {
			continue
		}
		sb.WriteString(strings.ToUpper(p[:1]))
		if len(p) > 1 {
			sb.WriteString(p[1:])
		}
	}
	if sb.Len() == 0 {
		return "Main"
	}
	return sb.String()
}
