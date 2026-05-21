package migrate

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"strings"
)

// CallSite is one occurrence of the legacy `goffi.Call("pkg.Func", args...)`
// pattern in a Go source file emitted by the legacy transpiler. The
// codemod tool walks a directory tree and reports call sites so the
// Phase 9 migration can rewrite them to typed FFI bindings as the
// frontend lands.
type CallSite struct {
	File     string
	Line     int
	Pkg      string
	Func     string
	NumArgs  int
	Variadic bool
}

// ScanCallSites walks root looking for *.go files (excluding _test.go)
// and returns every call to goffi.Call whose first argument is a string
// literal of the form "pkg.Func". Sites whose first argument is not a
// literal are still reported with Pkg/Func empty so the migrator can
// flag them as needing manual review.
func ScanCallSites(root string) ([]CallSite, error) {
	var sites []CallSite
	err := filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			if info.Name() == "vendor" || strings.HasPrefix(info.Name(), ".") {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		fset := token.NewFileSet()
		f, perr := parser.ParseFile(fset, path, nil, parser.SkipObjectResolution)
		if perr != nil {
			return nil
		}
		ast.Inspect(f, func(n ast.Node) bool {
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}
			if !isGoffiCall(call.Fun) {
				return true
			}
			site := CallSite{
				File:     path,
				Line:     fset.Position(call.Pos()).Line,
				NumArgs:  len(call.Args),
				Variadic: call.Ellipsis.IsValid(),
			}
			if len(call.Args) > 0 {
				if lit, ok := call.Args[0].(*ast.BasicLit); ok && lit.Kind == token.STRING {
					name := strings.Trim(lit.Value, "\"`")
					if dot := strings.LastIndex(name, "."); dot > 0 {
						site.Pkg = name[:dot]
						site.Func = name[dot+1:]
					}
				}
				site.NumArgs = len(call.Args) - 1
			}
			sites = append(sites, site)
			return true
		})
		return nil
	})
	return sites, err
}

// isGoffiCall reports whether expr names goffi.Call or runtime/ffi/go.Call.
func isGoffiCall(expr ast.Expr) bool {
	sel, ok := expr.(*ast.SelectorExpr)
	if !ok || sel.Sel.Name != "Call" {
		return false
	}
	ident, ok := sel.X.(*ast.Ident)
	if !ok {
		return false
	}
	return ident.Name == "goffi"
}

// SiteReport renders a human-readable summary of scanned call sites.
// The first line is a count; one line per site follows with file:line,
// package.function, and arg count.
func SiteReport(sites []CallSite) string {
	var b strings.Builder
	fmt.Fprintf(&b, "%d legacy goffi.Call site(s):\n", len(sites))
	for _, s := range sites {
		name := s.Pkg + "." + s.Func
		if s.Pkg == "" {
			name = "(non-literal callee, manual review required)"
		}
		fmt.Fprintf(&b, "  %s:%d  %s  args=%d\n", s.File, s.Line, name, s.NumArgs)
	}
	return b.String()
}
