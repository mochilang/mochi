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
	l := &lowerer{
		prog:    prog,
		imports: map[string]struct{}{},
		helpers: map[string]struct{}{},
	}
	return l.lowerProgram()
}

type lowerer struct {
	prog    *aotir.Program
	imports map[string]struct{}
	helpers map[string]struct{}
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

	// Lower every non-Main top-level function before main. Each is a
	// user-defined Mochi `fun` with a mangled name (mochi__<name>) or
	// a lifted closure body (IsLifted=true) named __anon_N / __shim_X.
	// Capturing lifted functions emit an env struct typedef immediately
	// before the function so the env pointer parameter has a typed shape.
	for i, fn := range l.prog.Functions {
		if i == l.prog.Main {
			continue
		}
		if fn == nil {
			return nil, fmt.Errorf("transpiler3/go/lower: function %d is nil", i)
		}
		decls, err := l.lowerFunction(fn)
		if err != nil {
			return nil, fmt.Errorf("function %s: %w", fn.Name, err)
		}
		f.Decls = append(f.Decls, decls...)
	}

	body, err := l.lowerBlock(mainFn.Body)
	if err != nil {
		return nil, err
	}

	f.Decls = append(f.Decls, l.emittedHelpers()...)

	f.Decls = append(f.Decls, &gotree.FuncDecl{
		Name: "main",
		Type: &gotree.FuncType{},
		Body: body,
	})

	f.Imports = l.emittedImports()
	return f, nil
}

// lowerFunction lowers one aotir.Function to one or more Go decls. The
// function name is taken verbatim from Function.Name (already mangled
// to mochi__<source> by the shared aotir lowerer, or __anon_N / __shim_X
// for lifted closures). Each Param is rendered through paramTypeText.
// Unit returns omit the Results clause.
//
// For a capturing lifted function (IsLifted && EnvTypeName != ""), the
// result is two decls: the env struct typedef followed by the function
// itself, which takes `__mochi_env *<EnvTypeName>` as its first param.
func (l *lowerer) lowerFunction(fn *aotir.Function) ([]gotree.Decl, error) {
	var out []gotree.Decl
	if fn.IsLifted && fn.EnvTypeName != "" {
		envDecl, err := l.lowerEnvTypedef(fn)
		if err != nil {
			return nil, fmt.Errorf("env typedef: %w", err)
		}
		out = append(out, envDecl)
	}

	params := make([]gotree.Field, 0, len(fn.Params)+1)
	if fn.IsLifted && fn.EnvTypeName != "" {
		params = append(params, gotree.Field{
			Names: []string{envParamName},
			Type:  &gotree.Ident{Name: "*" + fn.EnvTypeName},
		})
	}
	for _, p := range fn.Params {
		pt, err := l.paramTypeText(p)
		if err != nil {
			return nil, fmt.Errorf("param %s: %w", p.Name, err)
		}
		params = append(params, gotree.Field{
			Names: []string{mangleIdent(p.Name)},
			Type:  &gotree.Ident{Name: pt},
		})
	}
	ft := &gotree.FuncType{Params: params}
	if fn.ReturnType != aotir.TypeUnit {
		rt, err := l.returnTypeText(fn)
		if err != nil {
			return nil, fmt.Errorf("return type: %w", err)
		}
		ft.Results = []gotree.Field{{Type: &gotree.Ident{Name: rt}}}
	}
	body, err := l.lowerBlock(fn.Body)
	if err != nil {
		return nil, fmt.Errorf("body: %w", err)
	}
	out = append(out, &gotree.FuncDecl{Name: fn.Name, Type: ft, Body: body})
	return out, nil
}

// envParamName is the formal parameter name carried by every capturing
// lifted function. Mirrors the C model's `__mochi_env` so capture
// references can be rewritten uniformly across emitters.
const envParamName = "__mochi_env"

// lowerEnvTypedef emits `type <EnvTypeName> struct { Field1 T1; ... }`
// for a capturing lifted function. Field names are taken from the
// Capture's FieldName and capitalised so they remain accessible from
// the same package.
func (l *lowerer) lowerEnvTypedef(fn *aotir.Function) (gotree.Decl, error) {
	fields := make([]gotree.Field, 0, len(fn.Captures))
	for _, c := range fn.Captures {
		ft, err := l.lowerType(c.VarType)
		if err != nil {
			return nil, fmt.Errorf("capture %s: %w", c.FieldName, err)
		}
		fields = append(fields, gotree.Field{
			Names: []string{exportIdent(c.FieldName)},
			Type:  &gotree.Ident{Name: ft},
		})
	}
	return &gotree.GenDecl{
		Tok: "type",
		Specs: []gotree.Spec{&gotree.TypeSpec{
			Name: fn.EnvTypeName,
			Type: &gotree.StructType{Fields: fields},
		}},
	}, nil
}

// paramTypeText picks the Go type-expression text for one Param.
// Mirrors letTypeText's dispatch on compound types.
func (l *lowerer) paramTypeText(p aotir.Param) (string, error) {
	switch p.Type {
	case aotir.TypeList:
		if p.ElemType == aotir.TypeRecord {
			if p.ElemRecordName == "" {
				return "", fmt.Errorf("list<record> param missing ElemRecordName")
			}
			return "[]" + p.ElemRecordName, nil
		}
		return l.lowerListType(p.ElemType)
	case aotir.TypeMap:
		return l.lowerMapType(p.KeyType, p.ValueType)
	case aotir.TypeSet:
		return l.lowerSetType(p.ElemType)
	case aotir.TypeRecord:
		if p.RecordName == "" {
			return "", fmt.Errorf("record param missing RecordName")
		}
		return p.RecordName, nil
	case aotir.TypeUnion:
		if p.UnionName == "" {
			return "", fmt.Errorf("union param missing UnionName")
		}
		return p.UnionName, nil
	case aotir.TypeFun:
		return l.lowerFunType(p.FunSig)
	}
	return l.lowerType(p.Type)
}

// returnTypeText picks the Go type-expression text for a Function's
// return type. Compound returns dispatch on ReturnType the same way
// letTypeText does for bindings.
func (l *lowerer) returnTypeText(fn *aotir.Function) (string, error) {
	switch fn.ReturnType {
	case aotir.TypeList:
		if fn.ReturnElemType == aotir.TypeRecord {
			if fn.ReturnElemRecordName == "" {
				return "", fmt.Errorf("list<record> return missing ReturnElemRecordName")
			}
			return "[]" + fn.ReturnElemRecordName, nil
		}
		return l.lowerListType(fn.ReturnElemType)
	case aotir.TypeMap:
		return l.lowerMapType(fn.ReturnKeyType, fn.ReturnValueType)
	case aotir.TypeSet:
		return l.lowerSetType(fn.ReturnElemType)
	case aotir.TypeRecord:
		if fn.ReturnRecordName == "" {
			return "", fmt.Errorf("record return missing ReturnRecordName")
		}
		return fn.ReturnRecordName, nil
	case aotir.TypeUnion:
		if fn.ReturnUnionName == "" {
			return "", fmt.Errorf("union return missing ReturnUnionName")
		}
		return fn.ReturnUnionName, nil
	case aotir.TypeFun:
		return l.lowerFunType(fn.ReturnFunSig)
	}
	return l.lowerType(fn.ReturnType)
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

func (l *lowerer) addHelper(name string) { l.helpers[name] = struct{}{} }

func (l *lowerer) emittedImports() []gotree.ImportSpec {
	out := make([]gotree.ImportSpec, 0, len(l.imports))
	for p := range l.imports {
		out = append(out, gotree.ImportSpec{Path: p})
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Path < out[j].Path })
	return out
}

// emittedHelpers returns helper decls in deterministic order. Helpers
// are inlined directly into the generated file so the emitter avoids
// a separate runtime module dependency for tiny utilities.
func (l *lowerer) emittedHelpers() []gotree.Decl {
	if len(l.helpers) == 0 {
		return nil
	}
	names := make([]string, 0, len(l.helpers))
	for n := range l.helpers {
		names = append(names, n)
	}
	sort.Strings(names)
	out := make([]gotree.Decl, 0, len(names))
	for _, n := range names {
		if d := helperDecl(n); d != nil {
			out = append(out, d)
		}
	}
	return out
}

// helperDecl returns the gotree.Decl for a named inline helper.
// Unknown names return nil and the caller silently skips them.
func helperDecl(name string) gotree.Decl {
	switch name {
	case "mochiListSlice":
		// func mochiListSlice[T any](xs []T, start, end int64) []T {
		//   n := int64(len(xs))
		//   if start < 0 { start = 0 } else if start > n { start = n }
		//   if end < start { end = start } else if end > n { end = n }
		//   return xs[start:end]
		// }
		return &gotree.RawDecl{Code: `func mochiListSlice[T any](xs []T, start, end int64) []T {
	n := int64(len(xs))
	if start < 0 {
		start = 0
	} else if start > n {
		start = n
	}
	if end < start {
		end = start
	} else if end > n {
		end = n
	}
	return xs[start:end]
}`}
	case "mochiListSumI64":
		return &gotree.RawDecl{Code: `func mochiListSumI64(xs []int64) int64 {
	var s int64
	for _, v := range xs {
		s += v
	}
	return s
}`}
	case "mochiListSumF64":
		return &gotree.RawDecl{Code: `func mochiListSumF64(xs []float64) float64 {
	var s float64
	for _, v := range xs {
		s += v
	}
	return s
}`}
	case "mochiListMinI64":
		return &gotree.RawDecl{Code: `func mochiListMinI64(xs []int64) int64 {
	if len(xs) == 0 {
		panic("min: empty list")
	}
	m := xs[0]
	for _, v := range xs[1:] {
		if v < m {
			m = v
		}
	}
	return m
}`}
	case "mochiListMaxI64":
		return &gotree.RawDecl{Code: `func mochiListMaxI64(xs []int64) int64 {
	if len(xs) == 0 {
		panic("max: empty list")
	}
	m := xs[0]
	for _, v := range xs[1:] {
		if v > m {
			m = v
		}
	}
	return m
}`}
	case "mochiListMinF64":
		return &gotree.RawDecl{Code: `func mochiListMinF64(xs []float64) float64 {
	if len(xs) == 0 {
		panic("min: empty list")
	}
	m := xs[0]
	for _, v := range xs[1:] {
		if v < m {
			m = v
		}
	}
	return m
}`}
	case "mochiListMaxF64":
		return &gotree.RawDecl{Code: `func mochiListMaxF64(xs []float64) float64 {
	if len(xs) == 0 {
		panic("max: empty list")
	}
	m := xs[0]
	for _, v := range xs[1:] {
		if v > m {
			m = v
		}
	}
	return m
}`}
	case "mochiListMinStr":
		return &gotree.RawDecl{Code: `func mochiListMinStr(xs []string) string {
	if len(xs) == 0 {
		panic("min: empty list")
	}
	m := xs[0]
	for _, v := range xs[1:] {
		if v < m {
			m = v
		}
	}
	return m
}`}
	case "mochiListMaxStr":
		return &gotree.RawDecl{Code: `func mochiListMaxStr(xs []string) string {
	if len(xs) == 0 {
		panic("max: empty list")
	}
	m := xs[0]
	for _, v := range xs[1:] {
		if v > m {
			m = v
		}
	}
	return m
}`}
	case "mochiStrIndex":
		return &gotree.RawDecl{Code: `func mochiStrIndex(s string, i int64) string {
	rs := []rune(s)
	n := int64(len(rs))
	if i < 0 {
		i += n
	}
	if i < 0 || i >= n {
		panic("string index out of range")
	}
	return string(rs[i])
}`}
	case "mochiStrSubstring":
		return &gotree.RawDecl{Code: `func mochiStrSubstring(s string, start, end int64) string {
	rs := []rune(s)
	n := int64(len(rs))
	if start < 0 {
		start = 0
	} else if start > n {
		start = n
	}
	if end < start {
		end = start
	} else if end > n {
		end = n
	}
	return string(rs[start:end])
}`}
	case "mochiStrReverse":
		return &gotree.RawDecl{Code: `func mochiStrReverse(s string) string {
	rs := []rune(s)
	for i, j := 0, len(rs)-1; i < j; i, j = i+1, j-1 {
		rs[i], rs[j] = rs[j], rs[i]
	}
	return string(rs)
}`}
	case "mochiAbsI64":
		return &gotree.RawDecl{Code: `func mochiAbsI64(x int64) int64 {
	if x < 0 {
		return -x
	}
	return x
}`}
	case "mochiAppendFile":
		return &gotree.RawDecl{Code: `func mochiAppendFile(path, content string) {
	f, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		return
	}
	defer f.Close()
	_, _ = f.WriteString(content)
}`}
	case "mochiReadFile":
		return &gotree.RawDecl{Code: `func mochiReadFile(path string) string {
	b, err := os.ReadFile(path)
	if err != nil {
		return ""
	}
	return string(b)
}`}
	case "mochiLines":
		return &gotree.RawDecl{Code: `func mochiLines(path string) []string {
	b, err := os.ReadFile(path)
	if err != nil {
		return nil
	}
	s := string(b)
	if s == "" {
		return nil
	}
	parts := strings.Split(s, "\n")
	if len(parts) > 0 && parts[len(parts)-1] == "" {
		parts = parts[:len(parts)-1]
	}
	return parts
}`}
	}
	return nil
}
