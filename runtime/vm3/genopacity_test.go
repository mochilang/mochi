package vm3

import (
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestGenerationOpacity is the MEP-41 Phase 2 structural backstop for
// the "generation opacity" property (§6.2 rule class C). The property
// is: no code in `runtime/vm3` may surface the 12-bit generation tag
// of a handle Cell into a value that user bytecode can observe. The
// generation tag is a secret that backs the use-after-free invariant;
// if user code could read it, it could (a) forge a handle that
// re-aliases a freed slot, defeating per-deref gen checks, or (b)
// build a TCE-style oracle against the runtime (Apple MIE §6,
// Sept 2025).
//
// Enforcement strategy: the only Go-level API that exposes the gen
// field is Cell.DecodeHandle, which returns (tag, gen, idx). Every
// caller in this package destructures that triple. The rule is:
//
//  1. If the caller binds the gen position to `_`, the gen value is
//     dropped at the language level. Safe by construction.
//  2. If the caller binds the gen position to a named identifier
//     (e.g. `tag, gen, idx := c.DecodeHandle()`), then the identifier
//     may flow into MakeHandle (rebuild a handle with the same gen,
//     e.g. memory.go::handleCellReturn) and nowhere else. Any other
//     use is a gen-opacity leak and the test fails.
//
// The test parses every non-test .go file in this package, walks the
// AST, and applies the rule above. This is a structural property: it
// holds today and the test wedges the codebase so it keeps holding.
//
// Why an AST test and not a runtime test: gen opacity is a property
// of the *source*, not of any particular runtime trace. A regex-grep
// can be defeated by formatting; the AST test cannot. A future PR
// that wants to add a new legitimate gen consumer (e.g. a debug
// dump) must explicitly extend the allowlist below, which forces
// reviewer attention on the boundary.
func TestGenerationOpacity(t *testing.T) {
	entries, err := os.ReadDir(".")
	if err != nil {
		t.Fatalf("read dir: %v", err)
	}
	fset := token.NewFileSet()
	for _, e := range entries {
		if e.IsDir() {
			continue
		}
		name := e.Name()
		if !strings.HasSuffix(name, ".go") || strings.HasSuffix(name, "_test.go") {
			continue
		}
		path := filepath.Join(".", name)
		file, err := parser.ParseFile(fset, path, nil, parser.AllErrors)
		if err != nil {
			t.Fatalf("parse %s: %v", path, err)
		}
		walkGenOpacity(t, fset, file)
	}
}

// walkGenOpacity inspects every short-variable declaration of the
// form `a, b, c := <expr>.DecodeHandle()`. For each such site:
//
//   - If the gen position (b) is `_`, it is safe.
//   - Otherwise, every use of the bound identifier within the
//     enclosing function must be an argument to MakeHandle. Any
//     other usage is a gen-leak and reported by the test.
//
// The check is intentionally lexical and conservative: a future
// non-trivial flow (e.g. gen routed through a helper) must be
// approved by extending allowedGenSinks below.
func walkGenOpacity(t *testing.T, fset *token.FileSet, file *ast.File) {
	ast.Inspect(file, func(n ast.Node) bool {
		fd, ok := n.(*ast.FuncDecl)
		if !ok || fd.Body == nil {
			return true
		}
		ast.Inspect(fd.Body, func(n ast.Node) bool {
			as, ok := n.(*ast.AssignStmt)
			if !ok || as.Tok != token.DEFINE {
				return true
			}
			if len(as.Lhs) != 3 || len(as.Rhs) != 1 {
				return true
			}
			call, ok := as.Rhs[0].(*ast.CallExpr)
			if !ok {
				return true
			}
			sel, ok := call.Fun.(*ast.SelectorExpr)
			if !ok || sel.Sel.Name != "DecodeHandle" {
				return true
			}
			genIdent, ok := as.Lhs[1].(*ast.Ident)
			if !ok {
				return true
			}
			if genIdent.Name == "_" {
				return true
			}
			assertGenOnlyFlowsToMakeHandle(t, fset, fd, genIdent)
			return true
		})
		return true
	})
}

// allowedGenSinks is the closed set of function-call targets that
// may receive the gen field of a handle. Today the only entry is
// MakeHandle (handle round-trip in memory.go::handleCellReturn).
// Add to this list only with reviewer sign-off; the entries are the
// gen-opacity trust boundary expressed in code.
var allowedGenSinks = map[string]struct{}{
	"MakeHandle": {},
}

// assertGenOnlyFlowsToMakeHandle walks fn.Body looking for every
// reference to ident. Each reference must be a positional argument
// to a call whose target is in allowedGenSinks. Any other use --
// assignment to another variable, return statement, comparison,
// arithmetic -- is a leak.
func assertGenOnlyFlowsToMakeHandle(t *testing.T, fset *token.FileSet, fn *ast.FuncDecl, decl *ast.Ident) {
	ident := decl.Name
	ast.Inspect(fn.Body, func(n ast.Node) bool {
		id, ok := n.(*ast.Ident)
		if !ok || id.Name != ident {
			return true
		}
		if id == decl {
			return true
		}
		parent := findParentCallArg(fn.Body, id)
		if parent == nil {
			t.Errorf("gen leak: %s gen-named identifier %q used outside MakeHandle (at %s)",
				fset.Position(id.Pos()).String(), ident, fset.Position(id.Pos()))
			return true
		}
		sinkName := callTargetName(parent)
		if _, allowed := allowedGenSinks[sinkName]; !allowed {
			t.Errorf("gen leak: %s gen identifier %q flows into disallowed sink %q (at %s)",
				fset.Position(id.Pos()).String(), ident, sinkName, fset.Position(id.Pos()))
		}
		return true
	})
}

// findParentCallArg searches body for a *ast.CallExpr whose argument
// list contains id as a direct positional argument. Returns the call
// or nil. The search is depth-first and stops at the first hit.
func findParentCallArg(body *ast.BlockStmt, id *ast.Ident) *ast.CallExpr {
	var hit *ast.CallExpr
	ast.Inspect(body, func(n ast.Node) bool {
		if hit != nil {
			return false
		}
		call, ok := n.(*ast.CallExpr)
		if !ok {
			return true
		}
		for _, arg := range call.Args {
			if argIdent, ok := arg.(*ast.Ident); ok && argIdent == id {
				hit = call
				return false
			}
		}
		return true
	})
	return hit
}

// callTargetName returns the bare name of the target of call. For
// `f(x)` it is "f"; for `pkg.F(x)` or `recv.F(x)` it is "F". Returns
// the empty string for non-Ident / non-SelectorExpr targets (e.g.
// function literals), which the test treats as a leak.
func callTargetName(call *ast.CallExpr) string {
	switch t := call.Fun.(type) {
	case *ast.Ident:
		return t.Name
	case *ast.SelectorExpr:
		return t.Sel.Name
	}
	return ""
}

// TestGenIsNotReadableFromAnyOpcode is the IR-side counterpart to
// TestGenerationOpacity. It asserts that no compiler3 IR opcode
// (today's set, captured in the verify package's kindOf) bears a
// name suggesting it materializes the generation field of a handle.
// The intent is to catch a future PR that adds an OpHandleGen
// without separately updating MEP-41 §6.2 rule class C.
//
// The lexical check is necessarily heuristic: a future opcode named
// OpVersionTag would slip through. The verifier's init-time coverage
// assertion is the structural backstop (every opcode must be
// classified, and a gen-exposing opcode would necessitate a new
// ProducerKind); this test is the documentation-as-test of the
// naming convention.
func TestGenIsNotReadableFromAnyOpcode(t *testing.T) {
	// The check is implemented as a grep over compiler3/ir/types.go
	// to keep this package free of a compiler3 import (avoiding an
	// import cycle once compiler3/verify imports runtime/vm3 in a
	// future phase). The path is hard-coded relative to module root
	// because tests run with their package's CWD.
	data, err := os.ReadFile(filepath.Join("..", "..", "compiler3", "ir", "types.go"))
	if err != nil {
		t.Fatalf("read compiler3/ir/types.go: %v", err)
	}
	src := string(data)
	for _, forbidden := range []string{
		"OpHandleGen",
		"OpHandleGeneration",
		"OpGenOf",
		"OpReadGen",
		"OpDecodeHandle",
	} {
		if strings.Contains(src, forbidden) {
			t.Errorf("compiler3/ir/types.go declares forbidden gen-exposing opcode %q; "+
				"see MEP-41 §6.2 rule class C", forbidden)
		}
	}
}
