package lower

import (
	"fmt"
	"sort"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/go/gotree"
)

// Lower translates an aotir.Program into a gotree.File ready
// for emit. Phase 1 supports a single `main` function whose
// body is straight-line CallStmts targeting the print runtime
// shims (mochi_print_str, mochi_print_i64, mochi_print_f64,
// mochi_print_bool). Each lowers to fmt.Println(arg).
func Lower(prog *aotir.Program) (*gotree.File, error) {
	if prog == nil {
		return nil, fmt.Errorf("transpiler3/go/lower: nil program")
	}
	l := &lowerer{prog: prog, imports: map[string]struct{}{}}
	return l.lowerProgram()
}

type lowerer struct {
	prog    *aotir.Program
	imports map[string]struct{}
}

func (l *lowerer) lowerProgram() (*gotree.File, error) {
	f := &gotree.File{PackageName: "main"}

	mainFn, err := l.findMain()
	if err != nil {
		return nil, err
	}

	body, err := l.lowerBlock(mainFn.Body)
	if err != nil {
		return nil, err
	}

	f.Decls = append(f.Decls, &gotree.FuncDecl{
		Name: "main",
		Type: &gotree.FuncType{},
		Body: body,
	})

	f.Imports = l.emittedImports()
	return f, nil
}

func (l *lowerer) findMain() (*aotir.Function, error) {
	if len(l.prog.Functions) == 0 {
		return nil, fmt.Errorf("transpiler3/go/lower: program has no functions")
	}
	idx := l.prog.Main
	if idx < 0 || idx >= len(l.prog.Functions) {
		return nil, fmt.Errorf("transpiler3/go/lower: Main index %d out of range", idx)
	}
	fn := l.prog.Functions[idx]
	if fn == nil {
		return nil, fmt.Errorf("transpiler3/go/lower: Main function is nil")
	}
	if fn.Body == nil {
		return nil, fmt.Errorf("transpiler3/go/lower: Main function has nil body")
	}
	return fn, nil
}

func (l *lowerer) addImport(path string) { l.imports[path] = struct{}{} }

func (l *lowerer) emittedImports() []gotree.ImportSpec {
	out := make([]gotree.ImportSpec, 0, len(l.imports))
	for p := range l.imports {
		out = append(out, gotree.ImportSpec{Path: p})
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Path < out[j].Path })
	return out
}
