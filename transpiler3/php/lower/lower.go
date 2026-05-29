// Package lower translates an aotir.Program into a ptree.PhpFile.
// Entry point: Lower(prog, colours) → *ptree.PhpFile.
//
// Phase 0 only knows how to lower programs whose Main function has
// an empty body; it emits the runtime preamble plus a `mochi_main()`
// no-op and a trailing call. Phase 1 adds CallStmt to print, Phase 2
// adds scalar literals + arithmetic, and so on.
package lower

import (
	"fmt"
	"path/filepath"
	"strings"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/php/colour"
	"mochi/transpiler3/php/ptree"
)

// Lower translates an aotir.Program into a ptree.PhpFile. The returned
// file represents a single .php source file (named "main.php" by the
// emit pass) containing one `mochi_main()` function and one trailing
// call site.
func Lower(prog *aotir.Program, _ colour.ColourMap) (*ptree.PhpFile, error) {
	if prog == nil {
		return nil, fmt.Errorf("php lower: nil program")
	}
	if prog.Main < 0 || prog.Main >= len(prog.Functions) {
		return nil, fmt.Errorf("php lower: invalid Main index %d", prog.Main)
	}
	mainFn := prog.Functions[prog.Main]
	body, err := lowerBlock(mainFn.Body)
	if err != nil {
		return nil, err
	}
	mainDecl := &ptree.FuncDecl{
		PhpDoc:     []string{"Generated Mochi entry point. Do not edit by hand."},
		Name:       "mochi_main",
		ReturnType: "void",
		Body:       body,
	}

	file := &ptree.PhpFile{
		// Phase 0 keeps a global-namespace file. Phase 15 adds a real
		// PSR-4 namespace when the Composer package gates land.
		Namespace: "",
		Uses:      nil,
		Decls:     []ptree.Decl{mainDecl},
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

// lowerBlock translates an aotir.Block to a list of PHP statements.
// Phase 0 handles only an empty body; later phases extend this switch.
func lowerBlock(b *aotir.Block) ([]ptree.Stmt, error) {
	if b == nil {
		return nil, nil
	}
	var out []ptree.Stmt
	for _, st := range b.Statements {
		stmts, err := lowerStmt(st)
		if err != nil {
			return nil, err
		}
		out = append(out, stmts...)
	}
	return out, nil
}

// lowerStmt translates one aotir statement. Phase 0 only knows the
// no-op shape (an empty body never enters this function). Unknown
// statement types return an error so future phases produce a clear
// failure instead of a silent miss.
func lowerStmt(s aotir.Stmt) ([]ptree.Stmt, error) {
	return nil, fmt.Errorf("php lower: phase 0 cannot lower %T", s)
}

// ModuleName converts a Mochi source filename to a PSR-4 module name.
// Phase 0 returns the base name without the .mochi suffix; Phase 15
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
