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
	tmpSeq  int
}

// freshName returns a unique identifier with the given prefix. Used
// for transient lowerer-introduced bindings (e.g. match scrutinees,
// IIFE result holders).
func (l *lowerer) freshName(prefix string) string {
	l.tmpSeq++
	return fmt.Sprintf("%s%d", prefix, l.tmpSeq)
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

	for _, ud := range l.prog.Unions {
		decls, err := l.lowerUnionDecl(ud)
		if err != nil {
			return nil, err
		}
		f.Decls = append(f.Decls, decls...)
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

// lowerUnionDecl emits a tagged-union struct plus a constructor
// per variant. The Go layout flattens every variant's fields into
// uniquely-prefixed fields on the outer struct:
//
//	type Shape struct {
//	    Tag uint8
//	    Circle_R float64
//	    Rectangle_W float64
//	    Rectangle_H float64
//	}
//	func Shape_Circle(r float64) Shape { return Shape{Tag: 0, Circle_R: r} }
//	func Shape_Rectangle(w, h float64) Shape { return Shape{Tag: 1, Rectangle_W: w, Rectangle_H: h} }
func (l *lowerer) lowerUnionDecl(ud *aotir.UnionDecl) ([]gotree.Decl, error) {
	fields := []gotree.Field{{
		Names: []string{"Tag"},
		Type:  &gotree.Ident{Name: "uint8"},
	}}
	for _, v := range ud.Variants {
		for _, vf := range v.Fields {
			ft, err := l.lowerVariantFieldType(vf)
			if err != nil {
				return nil, fmt.Errorf("union %s variant %s field %s: %w", ud.Name, v.Name, vf.Name, err)
			}
			fields = append(fields, gotree.Field{
				Names: []string{variantFieldName(v.Name, vf.Name)},
				Type:  &gotree.Ident{Name: ft},
			})
		}
	}
	out := []gotree.Decl{&gotree.GenDecl{
		Tok: "type",
		Specs: []gotree.Spec{&gotree.TypeSpec{
			Name: ud.Name,
			Type: &gotree.StructType{Fields: fields},
		}},
	}}
	for _, v := range ud.Variants {
		ctor, err := l.lowerVariantCtor(ud, v)
		if err != nil {
			return nil, fmt.Errorf("union %s ctor %s: %w", ud.Name, v.Name, err)
		}
		out = append(out, ctor)
	}
	return out, nil
}

// lowerVariantFieldType picks the Go type expression for one variant
// field. Record-typed fields use the RecordName.
func (l *lowerer) lowerVariantFieldType(vf aotir.VariantField) (string, error) {
	if vf.FieldType == aotir.TypeRecord {
		if vf.RecordName == "" {
			return "", fmt.Errorf("variant field of record type missing RecordName")
		}
		return vf.RecordName, nil
	}
	return l.lowerType(vf.FieldType)
}

// variantFieldName joins variant + field with `_` so all variants'
// fields can live side-by-side on the outer struct without colliding.
func variantFieldName(variant, field string) string {
	return exportIdent(variant) + "_" + exportIdent(field)
}

// lowerVariantCtor emits a constructor for one variant:
//
//	func Union_Variant(f1 T1, f2 T2) Union {
//	    return Union{Tag: <n>, Variant_F1: f1, Variant_F2: f2}
//	}
func (l *lowerer) lowerVariantCtor(ud *aotir.UnionDecl, v aotir.VariantDecl) (gotree.Decl, error) {
	params := make([]gotree.Field, 0, len(v.Fields))
	elts := []gotree.Expr{&gotree.KeyValueExpr{
		Key:   &gotree.Ident{Name: "Tag"},
		Value: &gotree.BasicLit{Kind: gotree.IntLit, Value: fmt.Sprintf("%d", v.Tag)},
	}}
	for _, vf := range v.Fields {
		ft, err := l.lowerVariantFieldType(vf)
		if err != nil {
			return nil, err
		}
		params = append(params, gotree.Field{
			Names: []string{mangleIdent(vf.Name)},
			Type:  &gotree.Ident{Name: ft},
		})
		elts = append(elts, &gotree.KeyValueExpr{
			Key:   &gotree.Ident{Name: variantFieldName(v.Name, vf.Name)},
			Value: &gotree.Ident{Name: mangleIdent(vf.Name)},
		})
	}
	body := &gotree.BlockStmt{
		List: []gotree.Stmt{&gotree.ReturnStmt{Results: []gotree.Expr{
			&gotree.CompositeLit{Type: &gotree.Ident{Name: ud.Name}, Elts: elts},
		}}},
	}
	return &gotree.FuncDecl{
		Name: variantCtorName(ud.Name, v.Name),
		Type: &gotree.FuncType{
			Params:  params,
			Results: []gotree.Field{{Type: &gotree.Ident{Name: ud.Name}}},
		},
		Body: body,
	}, nil
}

// variantCtorName is the public name of the variant constructor.
func variantCtorName(union, variant string) string {
	return exportIdent(union) + "_" + exportIdent(variant)
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
