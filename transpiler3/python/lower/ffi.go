package lower

import (
	"fmt"
	"sort"
	"strings"

	"mochi/transpiler3/c/aotir"
)

// rejectNonPythonExterns walks the program-level extern decls and returns
// an error for the FFI surfaces that the Python target does not support
// in Phase 12.0. Python FFI is the only natively-supported FFI surface;
// Go FFI, JS FFI, Java FFI, and C extern declarations all reject at lower
// time with an explicit, fixable error.
func rejectNonPythonExterns(prog *aotir.Program) error {
	if len(prog.GoFuncs) > 0 {
		return fmt.Errorf("python/lower: Go FFI (`extern go fun %s`) not supported on Python target", prog.GoFuncs[0].Name)
	}
	if len(prog.JSFuncs) > 0 {
		return fmt.Errorf("python/lower: JavaScript FFI (`extern js fun %s`) not supported on Python target", prog.JSFuncs[0].Name)
	}
	if len(prog.JavaFuncs) > 0 {
		return fmt.Errorf("python/lower: Java FFI (`extern java fun %s`) not supported on Python target", prog.JavaFuncs[0].MochiName)
	}
	if len(prog.ExternFuncs) > 0 {
		return fmt.Errorf("python/lower: C extern (`extern fun %s`) not supported on Python target", prog.ExternFuncs[0].Name)
	}
	return nil
}

// registerPythonExterns walks prog.PythonFuncs and records each external
// name. The Python target emits a single
// `from mochi_user_<modname>_externs import <name1>, <name2>, ...`
// import at the top of the generated module; the call sites use the
// bare names verbatim. The C target's `mochi_py_<name>` prefix (which
// targets a JSON-stdin subprocess wrapper in the C runtime) is stripped
// at every CallStmt / CallExpr site in lower.go.
func (l *lowerer) registerPythonExterns(prog *aotir.Program) {
	for _, decl := range prog.PythonFuncs {
		l.pythonExterns[decl.Name] = true
	}
}

// pythonExternNames returns the registered Python FFI function names in
// sorted order so the emitted import is deterministic across runs.
func (l *lowerer) pythonExternNames() []string {
	names := make([]string, 0, len(l.pythonExterns))
	for n := range l.pythonExterns {
		names = append(names, n)
	}
	sort.Strings(names)
	return names
}

// stripPythonExternPrefix returns the bare extern name if `name` has the
// `mochi_py_` prefix (the canonical mangling produced by the C lower for
// Python FFI calls). Returns ("", false) for any other name.
func stripPythonExternPrefix(name string) (string, bool) {
	return strings.CutPrefix(name, "mochi_py_")
}
