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

	for _, rd := range l.prog.Records {
		decl, err := l.lowerRecordDecl(rd)
		if err != nil {
			return nil, err
		}
		f.Decls = append(f.Decls, decl)
	}

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

// lowerRecordDecl emits `type Name struct { Field T; ... }` per
// MEP-54 §6 "Record lowering". Field order matches the source-order
// captured in RecordDecl.Fields.
func (l *lowerer) lowerRecordDecl(rd *aotir.RecordDecl) (gotree.Decl, error) {
	fields := make([]gotree.Field, 0, len(rd.Fields))
	for _, fd := range rd.Fields {
		ft, err := l.lowerFieldType(fd)
		if err != nil {
			return nil, fmt.Errorf("record %s field %s: %w", rd.Name, fd.Name, err)
		}
		fields = append(fields, gotree.Field{
			Names: []string{exportIdent(fd.Name)},
			Type:  &gotree.Ident{Name: ft},
		})
	}
	return &gotree.GenDecl{
		Tok: "type",
		Specs: []gotree.Spec{&gotree.TypeSpec{
			Name: rd.Name,
			Type: &gotree.StructType{Fields: fields},
		}},
	}, nil
}

// lowerFieldType resolves a RecordField's Go type-expression text.
// For nested record types it uses the field's RecordName.
func (l *lowerer) lowerFieldType(fd aotir.RecordField) (string, error) {
	if fd.Type == aotir.TypeRecord {
		if fd.RecordName == "" {
			return "", fmt.Errorf("nested record field missing RecordName")
		}
		return fd.RecordName, nil
	}
	return l.lowerType(fd.Type)
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
