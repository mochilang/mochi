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

	for _, ad := range l.prog.Agents {
		decls, err := l.lowerAgentDecl(ad)
		if err != nil {
			return nil, fmt.Errorf("agent %s: %w", ad.Name, err)
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
	case aotir.TypeOMap:
		l.addHelper("mochiOMap")
		return l.lowerOMapType(p.KeyType, p.ValueType)
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
	case aotir.TypeOMap:
		l.addHelper("mochiOMap")
		return l.lowerOMapType(fn.ReturnKeyType, fn.ReturnValueType)
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

// lowerAgentDecl emits `type AgentName struct { Field T; ... }` plus
// one pointer-receiver method per intent. Phase 9.3 keeps agent fields
// scalar-only (matching the shared aotir verifier's restriction) so
// the field-type renderer reuses lowerType directly.
//
// Field access inside intent bodies appears in the IR as VarRefs whose
// Name has been prefixed with "__self->" by the shared C lowerer's
// emitName encoding. varRefExpr rewrites that to `self.Field` so the
// pointer receiver mutates state in place. AssignStmt LHS values with
// the same prefix flow through the matching rewrite in lowerAssignStmt.
//
// OnClose is ignored in Phase 9.3 for the Go backend: synchronous
// dispatch has no implicit teardown step, and the C lowerer's verifier
// already enforces termination ordering at compile time.
func (l *lowerer) lowerAgentDecl(ad *aotir.AgentDecl) ([]gotree.Decl, error) {
	fields := make([]gotree.Field, 0, len(ad.Fields))
	for _, f := range ad.Fields {
		ft, err := l.lowerType(f.Type)
		if err != nil {
			return nil, fmt.Errorf("field %s: %w", f.Name, err)
		}
		if ft == "" {
			return nil, fmt.Errorf("field %s: unit type not allowed", f.Name)
		}
		fields = append(fields, gotree.Field{
			Names: []string{exportIdent(f.Name)},
			Type:  &gotree.Ident{Name: ft},
		})
	}
	out := []gotree.Decl{&gotree.GenDecl{
		Tok: "type",
		Specs: []gotree.Spec{&gotree.TypeSpec{
			Name: ad.Name,
			Type: &gotree.StructType{Fields: fields},
		}},
	}}
	for _, intent := range ad.Intents {
		fn, err := l.lowerAgentIntentDecl(ad, intent)
		if err != nil {
			return nil, fmt.Errorf("intent %s: %w", intent.Name, err)
		}
		out = append(out, fn)
	}
	return out, nil
}

// lowerAgentIntentDecl emits one pointer-receiver method:
//
//	func (self *AgentName) IntentName(p1 T1, ...) R { body }
//
// The receiver name `self` matches agentSelfName, the prefix that
// varRefExpr strips when translating `__self->field` references.
func (l *lowerer) lowerAgentIntentDecl(ad *aotir.AgentDecl, intent aotir.AgentIntentDecl) (gotree.Decl, error) {
	params := make([]gotree.Field, 0, len(intent.Params))
	for _, p := range intent.Params {
		pt, err := l.lowerType(p.Type)
		if err != nil {
			return nil, fmt.Errorf("param %s: %w", p.Name, err)
		}
		if pt == "" {
			return nil, fmt.Errorf("param %s: unit not allowed", p.Name)
		}
		params = append(params, gotree.Field{
			Names: []string{mangleIdent(p.Name)},
			Type:  &gotree.Ident{Name: pt},
		})
	}
	ft := &gotree.FuncType{Params: params}
	if intent.ReturnType != aotir.TypeUnit {
		rt, err := l.lowerType(intent.ReturnType)
		if err != nil {
			return nil, fmt.Errorf("return type: %w", err)
		}
		if rt == "" {
			return nil, fmt.Errorf("return type cannot be unit")
		}
		ft.Results = []gotree.Field{{Type: &gotree.Ident{Name: rt}}}
	}
	body, err := l.lowerBlock(intent.Body)
	if err != nil {
		return nil, fmt.Errorf("body: %w", err)
	}
	recv := &gotree.Field{
		Names: []string{agentSelfName},
		Type:  &gotree.Ident{Name: "*" + ad.Name},
	}
	return &gotree.FuncDecl{
		Recv: recv,
		Name: exportIdent(intent.Name),
		Type: ft,
		Body: body,
	}, nil
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
	case "mochiLoadCSV":
		return &gotree.RawDecl{Code: `func mochiLoadCSV(path string) [][]string {
	f, err := os.Open(path)
	if err != nil {
		return nil
	}
	defer f.Close()
	r := csv.NewReader(f)
	r.FieldsPerRecord = -1
	rows, err := r.ReadAll()
	if err != nil {
		return nil
	}
	return rows
}`}
	case "mochiSaveCSV":
		return &gotree.RawDecl{Code: `func mochiSaveCSV(path string, data [][]string) {
	f, err := os.Create(path)
	if err != nil {
		return
	}
	defer f.Close()
	w := csv.NewWriter(f)
	_ = w.WriteAll(data)
	w.Flush()
}`}
	case "mochiPanicValue":
		return &gotree.RawDecl{Code: `type mochiPanicValue struct {
	code int64
	msg  string
}`}
	case "mochiPanic":
		return &gotree.RawDecl{Code: `func mochiPanic(code int64, msg string) {
	panic(mochiPanicValue{code: code, msg: msg})
}`}
	case "mochiOMap":
		return &gotree.RawDecl{Code: `type mochiOMap[K comparable, V any] struct {
	keys []K
	vals map[K]V
}`}
	case "mochiOMapNew":
		return &gotree.RawDecl{Code: `func mochiOMapNew[K comparable, V any]() *mochiOMap[K, V] {
	return &mochiOMap[K, V]{vals: map[K]V{}}
}`}
	case "mochiOMapSet":
		return &gotree.RawDecl{Code: `func mochiOMapSet[K comparable, V any](m *mochiOMap[K, V], k K, v V) {
	if _, ok := m.vals[k]; !ok {
		m.keys = append(m.keys, k)
	}
	m.vals[k] = v
}`}
	case "mochiOMapGet":
		return &gotree.RawDecl{Code: `func mochiOMapGet[K comparable, V any](m *mochiOMap[K, V], k K) V {
	return m.vals[k]
}`}
	case "mochiOMapHas":
		return &gotree.RawDecl{Code: `func mochiOMapHas[K comparable, V any](m *mochiOMap[K, V], k K) bool {
	_, ok := m.vals[k]
	return ok
}`}
	case "mochiOMapLen":
		return &gotree.RawDecl{Code: `func mochiOMapLen[K comparable, V any](m *mochiOMap[K, V]) int64 {
	return int64(len(m.keys))
}`}
	case "mochiTry":
		return &gotree.RawDecl{Code: `func mochiTry(try func(), catch func(code int64)) {
	defer func() {
		r := recover()
		if r == nil {
			return
		}
		switch v := r.(type) {
		case mochiPanicValue:
			catch(v.code)
		case runtime.Error:
			s := v.Error()
			switch {
			case strings.Contains(s, "out of range"):
				catch(4)
			case strings.Contains(s, "divide by zero"):
				catch(5)
			default:
				catch(9)
			}
		default:
			panic(r)
		}
	}()
	try()
}`}
	case "mochiSub":
		return &gotree.RawDecl{Code: `type mochiSub[T any] struct {
	ch    chan T
	drops bool
}`}
	case "mochiStream":
		return &gotree.RawDecl{Code: `type mochiStream[T any] struct {
	mu   sync.Mutex
	cap  int
	subs []*mochiSub[T]
}`}
	case "mochiStreamMake":
		return &gotree.RawDecl{Code: `func mochiStreamMake[T any](cap int) *mochiStream[T] {
	return &mochiStream[T]{cap: cap}
}`}
	case "mochiStreamSubscribe":
		return &gotree.RawDecl{Code: `func mochiStreamSubscribe[T any](s *mochiStream[T]) *mochiSub[T] {
	sub := &mochiSub[T]{ch: make(chan T, s.cap)}
	s.mu.Lock()
	s.subs = append(s.subs, sub)
	s.mu.Unlock()
	return sub
}`}
	case "mochiStreamSubscribeLimit":
		return &gotree.RawDecl{Code: `func mochiStreamSubscribeLimit[T any](s *mochiStream[T], n int) *mochiSub[T] {
	sub := &mochiSub[T]{ch: make(chan T, n), drops: true}
	s.mu.Lock()
	s.subs = append(s.subs, sub)
	s.mu.Unlock()
	return sub
}`}
	case "mochiStreamEmit":
		return &gotree.RawDecl{Code: `func mochiStreamEmit[T any](s *mochiStream[T], v T) {
	s.mu.Lock()
	subs := append([]*mochiSub[T](nil), s.subs...)
	s.mu.Unlock()
	for _, sub := range subs {
		if sub.drops {
			select {
			case sub.ch <- v:
			default:
			}
		} else {
			sub.ch <- v
		}
	}
}`}
	case "mochiSubRecv":
		return &gotree.RawDecl{Code: `func mochiSubRecv[T any](sub *mochiSub[T]) T {
	return <-sub.ch
}`}
	}
	return nil
}
