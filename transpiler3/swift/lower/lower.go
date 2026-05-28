// Package lower translates an aotir.Program into a sxtree.SourceFile.
// Entry point: Lower(prog, colours, className) → *sxtree.SourceFile.
package lower

import (
	"fmt"
	"maps"
	"math"
	"path/filepath"
	"strconv"
	"strings"
	"unicode"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/swift/colour"
	"mochi/transpiler3/swift/sxtree"
)

type lowerer struct {
	className    string
	colours      colour.ColourMap
	matchBindings map[string]string // field name → binding var name in current match arm
}

// Lower translates an aotir.Program into a sxtree.SourceFile.
func Lower(prog *aotir.Program, colours colour.ColourMap, className string) (*sxtree.SourceFile, error) {
	l := &lowerer{
		className: className,
		colours:   colours,
	}

	mainFn := prog.Functions[prog.Main]
	body, err := l.lowerBlock(mainFn.Body)
	if err != nil {
		return nil, err
	}

	mainFunc := &sxtree.FuncDecl{
		Modifiers: []string{"static"},
		Name:      "main",
		IsAsync:   colours[prog.Functions[prog.Main].Name] == colour.Red,
		Body:      body,
	}

	// Additional user-defined functions (non-main).
	members := []sxtree.Decl{mainFunc}
	for i, fn := range prog.Functions {
		if i == prog.Main {
			continue
		}
		fd, err := l.lowerFunction(fn)
		if err != nil {
			return nil, err
		}
		members = append(members, fd)
	}

	// Top-level type declarations: agent classes first, then enums, then structs, then the main struct.
	var topDecls []sxtree.Decl
	for _, ag := range prog.Agents {
		cd, err := l.lowerAgentDecl(ag)
		if err != nil {
			return nil, err
		}
		topDecls = append(topDecls, cd)
	}
	for _, ud := range prog.Unions {
		topDecls = append(topDecls, lowerUnionDecl(ud))
	}
	for _, rd := range prog.Records {
		topDecls = append(topDecls, lowerRecordDeclToStruct(rd))
	}

	structDecl := &sxtree.StructDecl{
		Attributes: []string{"@main"},
		Name:       className,
		Members:    members,
	}
	topDecls = append(topDecls, structDecl)

	sf := &sxtree.SourceFile{
		Name:    className,
		Imports: []string{"MochiRuntime"},
		Decls:   topDecls,
	}
	return sf, nil
}

// ClassName converts a Mochi source filename to a PascalCase struct name with "Mochi" suffix.
// "hello.mochi"       -> "HelloMochi"
// "hello_world.mochi" -> "HelloWorldMochi"
func ClassName(src string) string {
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
		runes := []rune(p)
		runes[0] = unicode.ToUpper(runes[0])
		sb.WriteString(string(runes))
	}
	if sb.Len() == 0 {
		return "MainMochi"
	}
	return sb.String() + "Mochi"
}

// lowerUnionDecl converts an aotir.UnionDecl to a Swift enum declaration.
func lowerUnionDecl(ud *aotir.UnionDecl) sxtree.Decl {
	cases := make([]sxtree.EnumCaseDecl, len(ud.Variants))
	for i, v := range ud.Variants {
		fields := make([]sxtree.EnumCaseField, len(v.Fields))
		for j, f := range v.Fields {
			fields[j] = sxtree.EnumCaseField{
				Label: f.Name,
				Type:  lowerVariantFieldType(f),
			}
		}
		cases[i] = sxtree.EnumCaseDecl{
			Name:   v.Name,
			Fields: fields,
		}
	}
	return &sxtree.EnumDecl{
		Modifiers: []string{"public"},
		Name:      ud.Name,
		Cases:     cases,
	}
}

func lowerVariantFieldType(f aotir.VariantField) string {
	switch f.FieldType {
	case aotir.TypeInt:
		return "Int64"
	case aotir.TypeFloat:
		return "Double"
	case aotir.TypeBool:
		return "Bool"
	case aotir.TypeString:
		return "String"
	case aotir.TypeRecord:
		if f.RecordName != "" {
			return f.RecordName
		}
		return "Any"
	default:
		return "Any"
	}
}

// lowerRecordDeclToStruct converts an aotir.RecordDecl to a Swift struct.
func lowerRecordDeclToStruct(rd *aotir.RecordDecl) sxtree.Decl {
	members := make([]sxtree.Decl, len(rd.Fields))
	for i, f := range rd.Fields {
		members[i] = &sxtree.VarDecl{
			IsVar:    false,
			Name:     f.Name,
			TypeName: lowerFieldTypeName(f),
		}
	}
	return &sxtree.StructDecl{
		Modifiers: []string{"public"},
		Name:      rd.Name,
		Protocols: []string{"Equatable"},
		Members:   members,
	}
}

func lowerFieldTypeName(f aotir.RecordField) string {
	switch f.Type {
	case aotir.TypeInt:
		return "Int64"
	case aotir.TypeFloat:
		return "Double"
	case aotir.TypeBool:
		return "Bool"
	case aotir.TypeString:
		return "String"
	case aotir.TypeRecord:
		if f.RecordName != "" {
			return f.RecordName
		}
		return "Any"
	default:
		return "Any"
	}
}

func (l *lowerer) lowerFunction(fn *aotir.Function) (*sxtree.FuncDecl, error) {
	bodyToLower := fn.Body
	if fn.IsLifted && len(fn.Captures) > 0 {
		bodyToLower = rewriteEnvRefs(fn.Body, fn.Captures)
	}
	body, err := l.lowerBlock(bodyToLower)
	if err != nil {
		return nil, err
	}
	retType := lowerReturnType(fn)
	params := lowerParams(fn.Params)
	// Prepend capture params for lifted functions (they are passed as separate args from FunLit).
	if fn.IsLifted && len(fn.Captures) > 0 {
		capParams := make([]sxtree.FuncParam, len(fn.Captures))
		for i, cap := range fn.Captures {
			capParams[i] = sxtree.FuncParam{
				Label:    "_",
				Name:     cap.FieldName,
				TypeName: swiftScalarType(cap.VarType),
			}
		}
		params = append(capParams, params...)
	}
	return &sxtree.FuncDecl{
		Modifiers:  []string{"static"},
		Name:       fn.Name,
		Params:     params,
		ReturnType: retType,
		IsAsync:    l.colours[fn.Name] == colour.Red,
		Body:       body,
	}, nil
}

// rewriteEnvRefs returns a copy of b where every VarRef whose Name starts with
// "__e->" is replaced with a VarRef using just the field name after "->".
func rewriteEnvRefs(b *aotir.Block, captures []aotir.FunCapture) *aotir.Block {
	if b == nil {
		return nil
	}
	renames := make(map[string]string, len(captures))
	for _, cap := range captures {
		renames["__e->"+cap.FieldName] = cap.FieldName
	}
	stmts := make([]aotir.Stmt, len(b.Statements))
	for i, s := range b.Statements {
		stmts[i] = rewriteStmtEnvRefs(s, renames)
	}
	return &aotir.Block{Statements: stmts}
}

func rewriteStmtEnvRefs(s aotir.Stmt, renames map[string]string) aotir.Stmt {
	switch s := s.(type) {
	case *aotir.ReturnStmt:
		if s.Value == nil {
			return s
		}
		return &aotir.ReturnStmt{Value: rewriteExprEnvRefs(s.Value, renames)}
	case *aotir.LetStmt:
		cp := *s
		if s.Init != nil {
			cp.Init = rewriteExprEnvRefs(s.Init, renames)
		}
		return &cp
	case *aotir.AssignStmt:
		cp := *s
		cp.Value = rewriteExprEnvRefs(s.Value, renames)
		return &cp
	case *aotir.CallStmt:
		args := make([]aotir.Expr, len(s.Args))
		for i, a := range s.Args {
			args[i] = rewriteExprEnvRefs(a, renames)
		}
		cp := *s
		cp.Args = args
		return &cp
	default:
		return s
	}
}

func rewriteExprEnvRefs(e aotir.Expr, renames map[string]string) aotir.Expr {
	switch e := e.(type) {
	case *aotir.VarRef:
		if newName, ok := renames[e.Name]; ok {
			cp := *e
			cp.Name = newName
			return &cp
		}
		return e
	case *aotir.BinaryExpr:
		cp := *e
		cp.Left = rewriteExprEnvRefs(e.Left, renames)
		cp.Right = rewriteExprEnvRefs(e.Right, renames)
		return &cp
	default:
		return e
	}
}

func lowerReturnType(fn *aotir.Function) string {
	switch fn.ReturnType {
	case aotir.TypeInt:
		return "Int64"
	case aotir.TypeFloat:
		return "Double"
	case aotir.TypeBool:
		return "Bool"
	case aotir.TypeString:
		return "String"
	case aotir.TypeRecord:
		if fn.ReturnRecordName != "" {
			return fn.ReturnRecordName
		}
		return "Any"
	case aotir.TypeUnion:
		if fn.ReturnUnionName != "" {
			return fn.ReturnUnionName
		}
		return "Any"
	case aotir.TypeList:
		return swiftListType(fn.ReturnElemType, fn.ReturnElemRecordName)
	case aotir.TypeMap:
		return swiftDictType(fn.ReturnKeyType, fn.ReturnValueType)
	case aotir.TypeSet:
		return "Set<" + swiftScalarType(fn.ReturnElemType) + ">"
	case aotir.TypeFun:
		return swiftFunType(fn.ReturnFunSig)
	case aotir.TypeUnit, aotir.TypeInvalid:
		return ""
	default:
		return ""
	}
}

func swiftScalarType(t aotir.Type) string {
	switch t {
	case aotir.TypeInt:
		return "Int64"
	case aotir.TypeFloat:
		return "Double"
	case aotir.TypeBool:
		return "Bool"
	case aotir.TypeString:
		return "String"
	default:
		return "Any"
	}
}

func swiftListType(elemType aotir.Type, elemRecordName string) string {
	if elemType == aotir.TypeRecord && elemRecordName != "" {
		return "[" + elemRecordName + "]"
	}
	return "[" + swiftScalarType(elemType) + "]"
}

func swiftDictType(keyType, valueType aotir.Type) string {
	return "[" + swiftScalarType(keyType) + ": " + swiftScalarType(valueType) + "]"
}

func swiftFunType(sig *aotir.FunSig) string {
	if sig == nil {
		return "(Any) -> Any"
	}
	params := make([]string, len(sig.ParamTypes))
	for i, p := range sig.ParamTypes {
		params[i] = swiftScalarType(p)
	}
	ret := swiftScalarType(sig.ReturnType)
	if sig.ReturnType == aotir.TypeUnit {
		ret = "Void"
	}
	return "(" + strings.Join(params, ", ") + ") -> " + ret
}

func lowerParams(params []aotir.Param) []sxtree.FuncParam {
	result := make([]sxtree.FuncParam, len(params))
	for i, p := range params {
		result[i] = sxtree.FuncParam{
			Label:    "_",
			Name:     p.Name,
			TypeName: lowerParamTypeName(p),
		}
	}
	return result
}

func lowerParamTypeName(p aotir.Param) string {
	switch p.Type {
	case aotir.TypeInt:
		return "Int64"
	case aotir.TypeFloat:
		return "Double"
	case aotir.TypeBool:
		return "Bool"
	case aotir.TypeString:
		return "String"
	case aotir.TypeRecord:
		if p.RecordName != "" {
			return p.RecordName
		}
		return "Any"
	case aotir.TypeUnion:
		if p.UnionName != "" {
			return p.UnionName
		}
		return "Any"
	case aotir.TypeList:
		return swiftListType(p.ElemType, p.ElemRecordName)
	case aotir.TypeMap:
		return swiftDictType(p.KeyType, p.ValueType)
	case aotir.TypeSet:
		return "Set<" + swiftScalarType(p.ElemType) + ">"
	case aotir.TypeFun:
		return swiftFunType(p.FunSig)
	default:
		return "Any"
	}
}

func (l *lowerer) lowerBlock(b *aotir.Block) ([]sxtree.Stmt, error) {
	if b == nil {
		return nil, nil
	}
	stmts := make([]sxtree.Stmt, 0, len(b.Statements))
	for _, s := range b.Statements {
		ss, err := l.lowerStmt(s)
		if err != nil {
			return nil, err
		}
		stmts = append(stmts, ss)
	}
	return stmts, nil
}

func (l *lowerer) lowerStmt(s aotir.Stmt) (sxtree.Stmt, error) {
	switch s := s.(type) {
	case *aotir.CallStmt:
		return l.lowerCallStmt(s)
	case *aotir.ReturnStmt:
		if s.Value == nil {
			return &sxtree.ReturnStmt{}, nil
		}
		v, err := l.lowerExpr(s.Value)
		if err != nil {
			return nil, err
		}
		return &sxtree.ReturnStmt{Value: v}, nil
	case *aotir.LetStmt:
		return l.lowerLetStmt(s)
	case *aotir.AssignStmt:
		v, err := l.lowerExpr(s.Value)
		if err != nil {
			return nil, err
		}
		return &sxtree.AssignStmt{Target: s.Name, Value: v}, nil
	case *aotir.IfStmt:
		return l.lowerIfStmt(s)
	case *aotir.WhileStmt:
		return l.lowerWhileStmt(s)
	case *aotir.ForEachStmt:
		return l.lowerForEachStmt(s)
	case *aotir.ForRangeStmt:
		return l.lowerForRangeStmt(s)
	case *aotir.BreakStmt:
		return &sxtree.BreakStmt{}, nil
	case *aotir.ContinueStmt:
		return &sxtree.ContinueStmt{}, nil
	case *aotir.RawCStmt:
		// C-specific setup code not needed for Swift.
		return &sxtree.RawSwiftStmt{Code: "// (raw C stmt omitted)"}, nil
	case *aotir.ClosureEnvStmt:
		// Swift closures capture from enclosing scope; no env struct needed.
		return &sxtree.RawSwiftStmt{Code: "// (closure env omitted)"}, nil
	case *aotir.MapPutStmt:
		return l.lowerMapPutStmt(s)
	case *aotir.ListSetStmt:
		return l.lowerListSetStmt(s)
	case *aotir.MatchStmt:
		return l.lowerMatchStmt(s)
	case *aotir.QueryScopeStmt:
		return l.lowerQueryScopeStmt(s)
	case *aotir.AgentIntentCallStmt:
		return l.lowerAgentIntentCallStmt(s)
	case *aotir.StreamEmitStmt:
		stream, err := l.lowerExpr(s.Stream)
		if err != nil {
			return nil, err
		}
		val, err := l.lowerExpr(s.Val)
		if err != nil {
			return nil, err
		}
		return &sxtree.RawSwiftStmt{
			Code: fmt.Sprintf("%s.emit(%s)", stream.SwiftExprString(), val.SwiftExprString()),
		}, nil
	// --- Phase 12 file I/O ---
	case *aotir.WriteFileStmt:
		path, err := l.lowerExpr(s.Path)
		if err != nil {
			return nil, err
		}
		content, err := l.lowerExpr(s.Content)
		if err != nil {
			return nil, err
		}
		return &sxtree.RawSwiftStmt{Code: fmt.Sprintf("mochiWriteFile(%s, %s)", path.SwiftExprString(), content.SwiftExprString())}, nil
	case *aotir.AppendFileStmt:
		path, err := l.lowerExpr(s.Path)
		if err != nil {
			return nil, err
		}
		content, err := l.lowerExpr(s.Content)
		if err != nil {
			return nil, err
		}
		return &sxtree.RawSwiftStmt{Code: fmt.Sprintf("mochiAppendFile(%s, %s)", path.SwiftExprString(), content.SwiftExprString())}, nil
	default:
		return nil, fmt.Errorf("swift/lower: unsupported statement %T", s)
	}
}

func (l *lowerer) lowerCallStmt(s *aotir.CallStmt) (sxtree.Stmt, error) {
	switch s.Func {
	case "mochi_print_str", "mochi_print_i64", "mochi_print_f64", "mochi_print_bool":
		if len(s.Args) != 1 {
			return nil, fmt.Errorf("swift/lower: %s wants 1 arg, got %d", s.Func, len(s.Args))
		}
		arg, err := l.lowerExpr(s.Args[0])
		if err != nil {
			return nil, err
		}
		call := &sxtree.CallExpr{
			Func: "mochiPrint",
			Args: []sxtree.Expr{arg},
		}
		return &sxtree.ExprStmt{X: call}, nil
	default:
		if !strings.HasPrefix(s.Func, "mochi_") {
			// User-defined function call.
			args := make([]sxtree.Expr, len(s.Args))
			for i, a := range s.Args {
				e, err := l.lowerExpr(a)
				if err != nil {
					return nil, err
				}
				args[i] = e
			}
			call := &sxtree.CallExpr{
				Func: l.className + "." + s.Func,
				Args: args,
			}
			return &sxtree.ExprStmt{X: call}, nil
		}
		return nil, fmt.Errorf("swift/lower: unsupported builtin %q", s.Func)
	}
}

func (l *lowerer) lowerLetStmt(s *aotir.LetStmt) (sxtree.Stmt, error) {
	var init sxtree.Expr
	var err error
	if s.Init != nil {
		init, err = l.lowerExpr(s.Init)
		if err != nil {
			return nil, err
		}
	}
	typeName := lowerLetTypeName(s)
	return &sxtree.LocalVarStmt{
		IsVar:    s.Mutable,
		Name:     s.Name,
		TypeName: typeName,
		Init:     init,
	}, nil
}

func lowerLetTypeName(s *aotir.LetStmt) string {
	switch s.VarType {
	case aotir.TypeInt:
		return "Int64"
	case aotir.TypeFloat:
		return "Double"
	case aotir.TypeBool:
		return "Bool"
	case aotir.TypeString:
		return "String"
	case aotir.TypeRecord:
		return s.RecordName
	case aotir.TypeUnion:
		return s.UnionName
	case aotir.TypeList:
		return swiftListType(s.ElemType, s.ElemRecordName)
	case aotir.TypeMap:
		return swiftDictType(s.KeyType, s.ValueType)
	case aotir.TypeSet:
		return "Set<" + swiftScalarType(s.ElemType) + ">"
	case aotir.TypeFun:
		return swiftFunType(s.FunSig)
	case aotir.TypeAgent:
		return s.AgentName
	case aotir.TypeStream:
		return "MochiStream<" + swiftStreamElemType(s.StreamElemType) + ">"
	case aotir.TypeSub:
		return "MochiSub<" + swiftStreamElemType(s.SubElemType) + ">"
	case aotir.TypeFuture:
		return "Task<" + swiftStreamElemType(s.FutureElemType) + ", Never>"
	default:
		return ""
	}
}

func swiftStreamElemType(t aotir.Type) string {
	switch t {
	case aotir.TypeInt:
		return "Int64"
	case aotir.TypeFloat:
		return "Double"
	case aotir.TypeBool:
		return "Bool"
	case aotir.TypeString:
		return "String"
	default:
		return "Any"
	}
}

func lowerTypeName(t aotir.Type) string {
	switch t {
	case aotir.TypeInt:
		return "Int64"
	case aotir.TypeFloat:
		return "Double"
	case aotir.TypeBool:
		return "Bool"
	case aotir.TypeString:
		return "String"
	default:
		return ""
	}
}

func (l *lowerer) lowerIfStmt(s *aotir.IfStmt) (sxtree.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	then, err := l.lowerBlock(s.Then)
	if err != nil {
		return nil, err
	}
	result := &sxtree.IfStmt{Cond: cond, Then: then}
	if s.Else != nil {
		elseStmts, err := l.lowerBlock(s.Else)
		if err != nil {
			return nil, err
		}
		result.Else = elseStmts
	}
	return result, nil
}

func (l *lowerer) lowerWhileStmt(s *aotir.WhileStmt) (sxtree.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	return &sxtree.WhileStmt{Cond: cond, Body: body}, nil
}

func (l *lowerer) lowerForEachStmt(s *aotir.ForEachStmt) (sxtree.Stmt, error) {
	iter, err := l.lowerExpr(s.List)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	return &sxtree.ForInStmt{
		Var:        s.Var,
		Collection: iter,
		Body:       body,
	}, nil
}

func (l *lowerer) lowerForRangeStmt(s *aotir.ForRangeStmt) (sxtree.Stmt, error) {
	start, err := l.lowerExpr(s.Start)
	if err != nil {
		return nil, err
	}
	end, err := l.lowerExpr(s.End)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	return &sxtree.ForRangeStmt{
		Var:   s.Var,
		Start: start,
		End:   end,
		Body:  body,
	}, nil
}

func (l *lowerer) lowerMapPutStmt(s *aotir.MapPutStmt) (sxtree.Stmt, error) {
	key, err := l.lowerExpr(s.Key)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	return &sxtree.IndexSetStmt{
		Receiver: s.Name,
		Index:    key,
		Value:    val,
	}, nil
}

func (l *lowerer) lowerListSetStmt(s *aotir.ListSetStmt) (sxtree.Stmt, error) {
	idx, err := l.lowerExpr(s.Index)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	// Swift list index must be Int, not Int64.
	castIdx := &sxtree.CallExpr{Func: "Int", Args: []sxtree.Expr{idx}}
	return &sxtree.IndexSetStmt{
		Receiver: s.Name,
		Index:    castIdx,
		Value:    val,
	}, nil
}

// multiStmt is a pseudo-statement that renders multiple statements at the same indent level.
// It is used when lowerMatchStmt needs to emit a var decl + switch together.
type multiStmt struct {
	stmts []sxtree.Stmt
}

func (m *multiStmt) SwiftString(ind int) string {
	parts := make([]string, len(m.stmts))
	for i, s := range m.stmts {
		parts[i] = s.SwiftString(ind)
	}
	return strings.Join(parts, "\n")
}

// lowerMatchStmt lowers an aotir.MatchStmt to a Swift switch statement.
func (l *lowerer) lowerMatchStmt(s *aotir.MatchStmt) (sxtree.Stmt, error) {
	target, err := l.lowerExpr(s.Target)
	if err != nil {
		return nil, err
	}

	var cases []sxtree.SwitchCase
	for _, arm := range s.Arms {
		c, err := l.lowerMatchArm(&arm, s.UnionName)
		if err != nil {
			return nil, fmt.Errorf("match arm %q: %w", arm.VariantName, err)
		}
		cases = append(cases, c)
	}
	if s.Default != nil {
		dc, err := l.lowerMatchDefaultArm(s.Default)
		if err != nil {
			return nil, fmt.Errorf("match default arm: %w", err)
		}
		cases = append(cases, dc)
	} else {
		// Add default: break to satisfy Swift's exhaustiveness.
		cases = append(cases, sxtree.SwitchCase{
			Pattern: "default",
			Body:    []sxtree.Stmt{&sxtree.BreakStmt{}},
		})
	}

	sw := &sxtree.SwitchStmt{Subject: target, Cases: cases}
	// The preceding LetStmt (Mutable=true, Init=nil) already declared ResultVar.
	// We only emit the switch; the variable declaration is handled by lowerLetStmt.
	return sw, nil
}


func (l *lowerer) lowerMatchArm(arm *aotir.MatchArm, _ string) (sxtree.SwitchCase, error) {
	// Build the pattern: .VariantName(let binding1, let binding2, ...)
	var pattern string
	if len(arm.Bindings) == 0 {
		pattern = "." + arm.VariantName
	} else {
		parts := make([]string, len(arm.Bindings))
		for i, b := range arm.Bindings {
			parts[i] = b.FieldName + ": let " + b.VarName
		}
		pattern = "." + arm.VariantName + "(" + strings.Join(parts, ", ") + ")"
	}

	// Set up binding map so VariantFieldAccess can resolve to the bound variable.
	saved := l.matchBindings
	l.matchBindings = make(map[string]string, len(arm.Bindings))
	for _, b := range arm.Bindings {
		l.matchBindings[b.FieldName] = b.VarName
	}
	defer func() { l.matchBindings = saved }()

	body, err := l.lowerBlock(arm.Body)
	if err != nil {
		return sxtree.SwitchCase{}, err
	}

	return sxtree.SwitchCase{Pattern: pattern, Body: body}, nil
}

func (l *lowerer) lowerMatchDefaultArm(arm *aotir.MatchArm) (sxtree.SwitchCase, error) {
	body, err := l.lowerBlock(arm.Body)
	if err != nil {
		return sxtree.SwitchCase{}, err
	}
	return sxtree.SwitchCase{Pattern: "default", Body: body}, nil
}

// lowerQueryScopeStmt lowers a QueryScopeStmt by lowering its body block.
// Swift's ARC handles allocation; no arena needed.
func (l *lowerer) lowerQueryScopeStmt(s *aotir.QueryScopeStmt) (sxtree.Stmt, error) {
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, fmt.Errorf("swift/lower: QueryScopeStmt body: %w", err)
	}
	if len(body) == 1 {
		return body[0], nil
	}
	return &multiStmt{stmts: body}, nil
}

func (l *lowerer) lowerExpr(e aotir.Expr) (sxtree.Expr, error) {
	switch e := e.(type) {
	case *aotir.StringLit:
		return &sxtree.StringLitExpr{Value: e.Value}, nil
	case *aotir.IntLit:
		return &sxtree.IntLitExpr{Value: e.Value}, nil
	case *aotir.FloatLit:
		return &sxtree.FloatLitExpr{Repr: formatDouble(e.Value)}, nil
	case *aotir.BoolLit:
		return &sxtree.BoolLitExpr{Value: e.Value}, nil
	case *aotir.VarRef:
		return &sxtree.IdentExpr{Name: e.Name}, nil
	case *aotir.UnionVarRef:
		return &sxtree.IdentExpr{Name: e.Name}, nil
	case *aotir.BinaryExpr:
		return l.lowerBinaryExpr(e)
	case *aotir.UnaryExpr:
		return l.lowerUnaryExpr(e)
	case *aotir.CallExpr:
		return l.lowerCallExpr(e)
	case *aotir.NumCastExpr:
		operand, err := l.lowerExpr(e.Operand)
		if err != nil {
			return nil, err
		}
		return &sxtree.CallExpr{
			Func: "mochiFloatToInt",
			Args: []sxtree.Expr{operand},
		}, nil
	case *aotir.StrLenExpr:
		recv, err := l.lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		return &sxtree.CallExpr{Func: "mochiStrLen", Args: []sxtree.Expr{recv}}, nil
	case *aotir.StrIndexExpr:
		recv, err := l.lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		idx, err := l.lowerExpr(e.Index)
		if err != nil {
			return nil, err
		}
		return &sxtree.CallExpr{Func: "mochiStrIndex", Args: []sxtree.Expr{recv, idx}}, nil
	case *aotir.StrContainsExpr:
		recv, err := l.lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		sub, err := l.lowerExpr(e.Sub)
		if err != nil {
			return nil, err
		}
		return &sxtree.CallExpr{Func: "mochiStrContains", Args: []sxtree.Expr{recv, sub}}, nil
	// --- Phase 3 collections ---
	case *aotir.ListLit:
		return l.lowerListLit(e)
	case *aotir.IndexExpr:
		return l.lowerIndexExpr(e)
	case *aotir.LenExpr:
		return l.lowerLenExpr(e)
	case *aotir.AppendExpr:
		return l.lowerAppendExpr(e)
	case *aotir.MapLit:
		return l.lowerMapLit(e)
	case *aotir.MapGetExpr:
		return l.lowerMapGetExpr(e)
	case *aotir.MapHasExpr:
		return l.lowerMapHasExpr(e)
	case *aotir.MapLenExpr:
		return l.lowerMapLenExpr(e)
	case *aotir.MapKeysExpr:
		return l.lowerMapKeysExpr(e)
	case *aotir.MapValuesExpr:
		return l.lowerMapValuesExpr(e)
	case *aotir.SetLiteralExpr:
		return l.lowerSetLiteralExpr(e)
	case *aotir.SetAddExpr:
		return l.lowerSetAddExpr(e)
	case *aotir.SetHasExpr:
		return l.lowerSetHasExpr(e)
	case *aotir.SetLenExpr:
		return l.lowerSetLenExpr(e)
	case *aotir.SetToListExpr:
		return l.lowerSetToListExpr(e)
	// --- Phase 4 records ---
	case *aotir.RecordLit:
		return l.lowerRecordLit(e)
	case *aotir.FieldAccess:
		return l.lowerFieldAccess(e)
	// --- Phase 5 sum types ---
	case *aotir.VariantLit:
		return l.lowerVariantLit(e)
	case *aotir.VariantFieldAccess:
		return l.lowerVariantFieldAccess(e)
	// --- Phase 6 closures/HOFs ---
	case *aotir.FunLit:
		return l.lowerFunLit(e)
	case *aotir.FunCallExpr:
		return l.lowerFunCallExpr(e)
	case *aotir.ListFilterExpr:
		return l.lowerListFilterExpr(e)
	case *aotir.ListMapExpr:
		return l.lowerListMapExpr(e)
	case *aotir.ListFoldlExpr:
		return l.lowerListFoldlExpr(e)
	// --- Phase 7/8 query/datalog ---
	case *aotir.ListSortAscExpr:
		return l.lowerListSortAscExpr(e)
	case *aotir.ListSliceExpr:
		return l.lowerListSliceExpr(e)
	case *aotir.DatalogQueryExpr:
		return l.lowerDatalogQueryExpr(e)
	// --- Phase 9 agents ---
	case *aotir.AgentLit:
		return l.lowerAgentLit(e)
	case *aotir.AgentIntentCallExpr:
		return l.lowerAgentIntentCallExpr(e)
	// --- Phase 10 streams ---
	case *aotir.StreamMakeExpr:
		cap, err := l.lowerExpr(e.Cap)
		if err != nil {
			return nil, err
		}
		elemType := swiftStreamElemType(e.ElemType)
		return &sxtree.RawSwiftExpr{
			Code: fmt.Sprintf("MochiStream<%s>(capacity: %s)", elemType, cap.SwiftExprString()),
		}, nil
	case *aotir.SubMakeExpr:
		stream, err := l.lowerExpr(e.Stream)
		if err != nil {
			return nil, err
		}
		return &sxtree.RawSwiftExpr{
			Code: fmt.Sprintf("%s.subscribe()", stream.SwiftExprString()),
		}, nil
	case *aotir.SubMakeLimitExpr:
		stream, err := l.lowerExpr(e.Stream)
		if err != nil {
			return nil, err
		}
		return &sxtree.RawSwiftExpr{
			Code: fmt.Sprintf("%s.subscribe()", stream.SwiftExprString()),
		}, nil
	case *aotir.SubRecvExpr:
		sub, err := l.lowerExpr(e.Sub)
		if err != nil {
			return nil, err
		}
		return &sxtree.RawSwiftExpr{
			Code: fmt.Sprintf("%s.recv()", sub.SwiftExprString()),
		}, nil
	// --- math builtins ---
	case *aotir.MathCallExpr:
		return l.lowerMathCallExpr(e)
	// --- str builtins ---
	case *aotir.StrSubstringExpr:
		return l.lowerStrSubstringExpr(e)
	case *aotir.StrReverseExpr:
		return l.lowerStrReverseExpr(e)
	case *aotir.StrConvertExpr:
		return l.lowerStrConvertExpr(e)
	case *aotir.StrUpperExpr:
		return l.lowerStrUpperExpr(e)
	case *aotir.StrLowerExpr:
		return l.lowerStrLowerExpr(e)
	case *aotir.StrSplitExpr:
		return l.lowerStrSplitExpr(e)
	case *aotir.StrJoinExpr:
		return l.lowerStrJoinExpr(e)
	case *aotir.ListMinExpr:
		return l.lowerListMinExpr(e)
	case *aotir.ListMaxExpr:
		return l.lowerListMaxExpr(e)
	case *aotir.ListSumExpr:
		return l.lowerListSumExpr(e)
	case *aotir.ListContainsExpr:
		return l.lowerListContainsExpr(e)
	// --- Phase 11 async/await ---
	case *aotir.AsyncExpr:
		body, err := l.lowerExpr(e.Body)
		if err != nil {
			return nil, err
		}
		elemType := swiftStreamElemType(e.ElemType)
		return &sxtree.RawSwiftExpr{
			Code: fmt.Sprintf("Task<%s, Never> { %s }", elemType, body.SwiftExprString()),
		}, nil
	case *aotir.AwaitExpr:
		fut, err := l.lowerExpr(e.Future)
		if err != nil {
			return nil, err
		}
		return &sxtree.RawSwiftExpr{
			Code: fmt.Sprintf("await %s.value", fut.SwiftExprString()),
		}, nil
	// --- Phase 12 file I/O ---
	case *aotir.ReadFileExpr:
		path, err := l.lowerExpr(e.Path)
		if err != nil {
			return nil, err
		}
		return &sxtree.RawSwiftExpr{Code: fmt.Sprintf("mochiReadFile(%s)", path.SwiftExprString())}, nil
	case *aotir.LinesExpr:
		path, err := l.lowerExpr(e.Path)
		if err != nil {
			return nil, err
		}
		return &sxtree.RawSwiftExpr{Code: fmt.Sprintf("mochiLines(%s)", path.SwiftExprString())}, nil
	// --- Phase 13 LLM ---
	case *aotir.LLMGenerateExpr:
		model, err := l.lowerExpr(e.Model)
		if err != nil {
			return nil, err
		}
		prompt, err := l.lowerExpr(e.Prompt)
		if err != nil {
			return nil, err
		}
		return &sxtree.RawSwiftExpr{
			Code: fmt.Sprintf("mochiLLMGenerate(%q, %s, %s)", e.Provider, model.SwiftExprString(), prompt.SwiftExprString()),
		}, nil
	default:
		return nil, fmt.Errorf("swift/lower: unsupported expression %T", e)
	}
}

func (l *lowerer) lowerBinaryExpr(e *aotir.BinaryExpr) (sxtree.Expr, error) {
	left, err := l.lowerExpr(e.Left)
	if err != nil {
		return nil, err
	}
	right, err := l.lowerExpr(e.Right)
	if err != nil {
		return nil, err
	}
	if e.Op == aotir.BinStrCat {
		// Swift string concat is +
		return &sxtree.BinaryExpr{Left: left, Op: "+", Right: right}, nil
	}
	if e.Op == aotir.BinEqList || e.Op == aotir.BinNeList {
		// Use mochiListEq helper from runtime.
		eqCall := &sxtree.CallExpr{Func: "mochiListEq", Args: []sxtree.Expr{left, right}}
		if e.Op == aotir.BinNeList {
			return &sxtree.UnaryExpr{Op: "!", Operand: eqCall}, nil
		}
		return eqCall, nil
	}
	op := lowerBinOp(e.Op)
	return &sxtree.BinaryExpr{Left: left, Op: op, Right: right}, nil
}

func (l *lowerer) lowerUnaryExpr(e *aotir.UnaryExpr) (sxtree.Expr, error) {
	operand, err := l.lowerExpr(e.Operand)
	if err != nil {
		return nil, err
	}
	switch e.Op {
	case aotir.UnNegI64, aotir.UnNegF64:
		return &sxtree.UnaryExpr{Op: "-", Operand: operand}, nil
	case aotir.UnNotBool:
		return &sxtree.UnaryExpr{Op: "!", Operand: operand}, nil
	default:
		return nil, fmt.Errorf("swift/lower: unsupported unary op %v", e.Op)
	}
}

func (l *lowerer) lowerCallExpr(e *aotir.CallExpr) (sxtree.Expr, error) {
	// Handle __await_all__ built-in.
	if e.Func == "__await_all__" && len(e.Args) == 1 {
		arg, err := l.lowerExpr(e.Args[0])
		if err != nil {
			return nil, err
		}
		return &sxtree.RawSwiftExpr{
			Code: fmt.Sprintf("await mochiAwaitAll(%s)", arg.SwiftExprString()),
		}, nil
	}
	args := make([]sxtree.Expr, len(e.Args))
	for i, a := range e.Args {
		se, err := l.lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args[i] = se
	}
	return &sxtree.CallExpr{
		Func: l.className + "." + e.Func,
		Args: args,
	}, nil
}

// --- Phase 3 collection lowering helpers ---

func (l *lowerer) lowerListLit(e *aotir.ListLit) (sxtree.Expr, error) {
	elems := make([]sxtree.Expr, len(e.Elems))
	for i, elem := range e.Elems {
		se, err := l.lowerExpr(elem)
		if err != nil {
			return nil, err
		}
		elems[i] = se
	}
	return &sxtree.ArrayLitExpr{Elems: elems}, nil
}

func (l *lowerer) lowerIndexExpr(e *aotir.IndexExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	idx, err := l.lowerExpr(e.Index)
	if err != nil {
		return nil, err
	}
	// Swift array indexing requires Int, not Int64.
	castIdx := &sxtree.CallExpr{Func: "Int", Args: []sxtree.Expr{idx}}
	return &sxtree.SubscriptExpr{Receiver: recv, Index: castIdx}, nil
}

func (l *lowerer) lowerLenExpr(e *aotir.LenExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	countMember := &sxtree.MemberExpr{Receiver: recv, Member: "count"}
	return &sxtree.CallExpr{Func: "Int64", Args: []sxtree.Expr{countMember}}, nil
}

func (l *lowerer) lowerAppendExpr(e *aotir.AppendExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(e.Value)
	if err != nil {
		return nil, err
	}
	// Use runtime helper: mochiAppend(xs, elem) -> [T]
	return &sxtree.CallExpr{Func: "mochiAppend", Args: []sxtree.Expr{recv, val}}, nil
}

func (l *lowerer) lowerMapLit(e *aotir.MapLit) (sxtree.Expr, error) {
	if len(e.Keys) == 0 {
		// Empty dict literal: [:]
		return &sxtree.RawSwiftExpr{Code: "[:]"}, nil
	}
	// Build a dictionary literal: [k1: v1, k2: v2, ...]
	parts := make([]string, len(e.Keys))
	for i := range e.Keys {
		k, err := l.lowerExpr(e.Keys[i])
		if err != nil {
			return nil, err
		}
		v, err := l.lowerExpr(e.Values[i])
		if err != nil {
			return nil, err
		}
		parts[i] = k.SwiftExprString() + ": " + v.SwiftExprString()
	}
	return &sxtree.RawSwiftExpr{Code: "[" + strings.Join(parts, ", ") + "]"}, nil
}

func (l *lowerer) lowerMapGetExpr(e *aotir.MapGetExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	key, err := l.lowerExpr(e.Key)
	if err != nil {
		return nil, err
	}
	// m[key]! — force-unwrap optional (Mochi semantics panic on missing key)
	sub := &sxtree.SubscriptExpr{Receiver: recv, Index: key}
	return &sxtree.RawSwiftExpr{Code: sub.SwiftExprString() + "!"}, nil
}

func (l *lowerer) lowerMapHasExpr(e *aotir.MapHasExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	key, err := l.lowerExpr(e.Key)
	if err != nil {
		return nil, err
	}
	// m[key] != nil
	sub := &sxtree.SubscriptExpr{Receiver: recv, Index: key}
	return &sxtree.RawSwiftExpr{Code: "(" + sub.SwiftExprString() + " != nil)"}, nil
}

func (l *lowerer) lowerMapLenExpr(e *aotir.MapLenExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	countMember := &sxtree.MemberExpr{Receiver: recv, Member: "count"}
	return &sxtree.CallExpr{Func: "Int64", Args: []sxtree.Expr{countMember}}, nil
}

func (l *lowerer) lowerMapKeysExpr(e *aotir.MapKeysExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	// mochiMapKeys sorts keys for deterministic output.
	return &sxtree.CallExpr{Func: "mochiMapKeys", Args: []sxtree.Expr{recv}}, nil
}

func (l *lowerer) lowerMapValuesExpr(e *aotir.MapValuesExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	// mochiMapValues returns values sorted by key.
	return &sxtree.CallExpr{Func: "mochiMapValues", Args: []sxtree.Expr{recv}}, nil
}

func (l *lowerer) lowerSetLiteralExpr(e *aotir.SetLiteralExpr) (sxtree.Expr, error) {
	elems := make([]sxtree.Expr, len(e.Elems))
	for i, elem := range e.Elems {
		se, err := l.lowerExpr(elem)
		if err != nil {
			return nil, err
		}
		elems[i] = se
	}
	// Set([elem1, elem2, ...])
	arr := &sxtree.ArrayLitExpr{Elems: elems}
	return &sxtree.CallExpr{Func: "Set", Args: []sxtree.Expr{arr}}, nil
}

func (l *lowerer) lowerSetAddExpr(e *aotir.SetAddExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	elem, err := l.lowerExpr(e.Elem)
	if err != nil {
		return nil, err
	}
	// Use runtime helper: mochiSetAdd(s, elem) -> Set<T>
	return &sxtree.CallExpr{Func: "mochiSetAdd", Args: []sxtree.Expr{recv, elem}}, nil
}

func (l *lowerer) lowerSetHasExpr(e *aotir.SetHasExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	elem, err := l.lowerExpr(e.Elem)
	if err != nil {
		return nil, err
	}
	return &sxtree.MethodCallExpr{Receiver: recv, Method: "contains", Args: []sxtree.Expr{elem}}, nil
}

func (l *lowerer) lowerSetLenExpr(e *aotir.SetLenExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	countMember := &sxtree.MemberExpr{Receiver: recv, Member: "count"}
	return &sxtree.CallExpr{Func: "Int64", Args: []sxtree.Expr{countMember}}, nil
}

func (l *lowerer) lowerSetToListExpr(e *aotir.SetToListExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	// Array(set) — convert set to array (sorted for determinism).
	return &sxtree.MethodCallExpr{Receiver: recv, Method: "sorted", Args: nil}, nil
}

// --- Phase 4 record lowering helpers ---

func (l *lowerer) lowerRecordLit(e *aotir.RecordLit) (sxtree.Expr, error) {
	// Swift struct init: TypeName(field1: val1, field2: val2)
	labeledArgs := make([]string, len(e.Fields))
	for i, f := range e.Fields {
		v, err := l.lowerExpr(f.Value)
		if err != nil {
			return nil, err
		}
		labeledArgs[i] = f.Name + ": " + v.SwiftExprString()
	}
	return &sxtree.RawSwiftExpr{
		Code: e.TypeName + "(" + strings.Join(labeledArgs, ", ") + ")",
	}, nil
}

func (l *lowerer) lowerFieldAccess(e *aotir.FieldAccess) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &sxtree.MemberExpr{Receiver: recv, Member: e.FieldName}, nil
}

// --- Phase 5 sum type lowering helpers ---

func (l *lowerer) lowerVariantLit(e *aotir.VariantLit) (sxtree.Expr, error) {
	if len(e.Fields) == 0 {
		// Nullary variant: .VariantName
		return &sxtree.RawSwiftExpr{Code: e.UnionName + "." + e.VariantName}, nil
	}
	// Variant with fields: TypeName.VariantName(field1: val1, ...)
	labeledArgs := make([]string, len(e.Fields))
	for i, f := range e.Fields {
		v, err := l.lowerExpr(f.Value)
		if err != nil {
			return nil, err
		}
		labeledArgs[i] = f.Name + ": " + v.SwiftExprString()
	}
	return &sxtree.RawSwiftExpr{
		Code: e.UnionName + "." + e.VariantName + "(" + strings.Join(labeledArgs, ", ") + ")",
	}, nil
}

func (l *lowerer) lowerVariantFieldAccess(e *aotir.VariantFieldAccess) (sxtree.Expr, error) {
	// Inside a match arm, VariantFieldAccess resolves to the bound variable name.
	if l.matchBindings != nil {
		if varName, ok := l.matchBindings[e.FieldName]; ok {
			return &sxtree.IdentExpr{Name: varName}, nil
		}
	}
	// Outside a match arm: receiver.fieldName (unusual but handle gracefully).
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &sxtree.MemberExpr{Receiver: recv, Member: e.FieldName}, nil
}

// --- Phase 6 closure/HOF lowering helpers ---

func (l *lowerer) lowerFunLit(e *aotir.FunLit) (sxtree.Expr, error) {
	sig := e.Sig
	if sig == nil {
		return nil, fmt.Errorf("swift/lower: FunLit %q has nil Sig", e.FuncName)
	}

	// Build param names.
	paramNames := make([]string, len(sig.ParamTypes))
	for i := range sig.ParamTypes {
		paramNames[i] = fmt.Sprintf("__p%d", i)
	}

	// Build args for the static call: captures first, then params.
	callArgs := make([]sxtree.Expr, 0, len(e.Captures)+len(paramNames))
	for _, cap := range e.Captures {
		callArgs = append(callArgs, &sxtree.IdentExpr{Name: cap.SrcName})
	}
	for _, pn := range paramNames {
		callArgs = append(callArgs, &sxtree.IdentExpr{Name: pn})
	}

	callExpr := &sxtree.CallExpr{
		Func: l.className + "." + e.FuncName,
		Args: callArgs,
	}

	// Build the closure.
	var body []sxtree.Stmt
	if sig.ReturnType == aotir.TypeUnit {
		body = []sxtree.Stmt{&sxtree.ExprStmt{X: callExpr}}
	} else {
		body = []sxtree.Stmt{&sxtree.ReturnStmt{Value: callExpr}}
	}

	return &sxtree.ClosureExpr{Params: paramNames, Body: body}, nil
}

func (l *lowerer) lowerFunCallExpr(e *aotir.FunCallExpr) (sxtree.Expr, error) {
	callee, err := l.lowerExpr(e.Callee)
	if err != nil {
		return nil, err
	}
	args := make([]sxtree.Expr, len(e.Args))
	for i, a := range e.Args {
		a2, err := l.lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args[i] = a2
	}
	// callee(args...) — Swift closure call.
	argStrs := make([]string, len(args))
	for i, a := range args {
		argStrs[i] = a.SwiftExprString()
	}
	return &sxtree.RawSwiftExpr{
		Code: callee.SwiftExprString() + "(" + strings.Join(argStrs, ", ") + ")",
	}, nil
}

func (l *lowerer) lowerListFilterExpr(e *aotir.ListFilterExpr) (sxtree.Expr, error) {
	list, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, err
	}
	fn, err := l.lowerExpr(e.Fn)
	if err != nil {
		return nil, err
	}
	return &sxtree.MethodCallExpr{Receiver: list, Method: "filter", Args: []sxtree.Expr{fn}}, nil
}

func (l *lowerer) lowerListMapExpr(e *aotir.ListMapExpr) (sxtree.Expr, error) {
	list, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, err
	}
	fn, err := l.lowerExpr(e.Fn)
	if err != nil {
		return nil, err
	}
	return &sxtree.MethodCallExpr{Receiver: list, Method: "map", Args: []sxtree.Expr{fn}}, nil
}

func (l *lowerer) lowerListFoldlExpr(e *aotir.ListFoldlExpr) (sxtree.Expr, error) {
	list, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, err
	}
	fn, err := l.lowerExpr(e.Fn)
	if err != nil {
		return nil, err
	}
	init, err := l.lowerExpr(e.Init)
	if err != nil {
		return nil, err
	}
	// Swift reduce: list.reduce(init, fn)
	return &sxtree.MethodCallExpr{Receiver: list, Method: "reduce", Args: []sxtree.Expr{init, fn}}, nil
}

func (l *lowerer) lowerListSortAscExpr(e *aotir.ListSortAscExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &sxtree.MethodCallExpr{Receiver: recv, Method: "sorted", Args: nil}, nil
}

func (l *lowerer) lowerListSliceExpr(e *aotir.ListSliceExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	start, err := l.lowerExpr(e.Start)
	if err != nil {
		return nil, err
	}

	// Detect skip-only sentinel: 1<<62 - 1 = 4611686018427387903
	if lit, ok := e.End.(*aotir.IntLit); ok && lit.Value == 1<<62-1 {
		// Array(xs.dropFirst(Int(start)))
		castStart := &sxtree.CallExpr{Func: "Int", Args: []sxtree.Expr{start}}
		dropped := &sxtree.MethodCallExpr{Receiver: recv, Method: "dropFirst", Args: []sxtree.Expr{castStart}}
		return &sxtree.CallExpr{Func: "Array", Args: []sxtree.Expr{dropped}}, nil
	}

	end, err := l.lowerExpr(e.End)
	if err != nil {
		return nil, err
	}
	// Array(xs.dropFirst(Int(start)).prefix(Int(end - start)))
	castStartRaw := start.SwiftExprString()
	castEndRaw := end.SwiftExprString()
	code := fmt.Sprintf("Array(%s.dropFirst(Int(%s)).prefix(Int(%s) - Int(%s)))",
		recv.SwiftExprString(), castStartRaw, castEndRaw, castStartRaw)
	return &sxtree.RawSwiftExpr{Code: code}, nil
}

// --- Phase 8 datalog ---

func (l *lowerer) lowerDatalogQueryExpr(e *aotir.DatalogQueryExpr) (sxtree.Expr, error) {
	results := datalogEval(e)
	elems := make([]sxtree.Expr, len(results))
	for i, r := range results {
		elems[i] = &sxtree.StringLitExpr{Value: r}
	}
	return &sxtree.ArrayLitExpr{Elems: elems}, nil
}

// datalogEval performs semi-naive bottom-up evaluation.
func datalogEval(e *aotir.DatalogQueryExpr) []string {
	if e.Prog == nil {
		return nil
	}
	state := map[string][][]string{}
	for _, f := range e.Prog.Facts {
		args := make([]string, len(f.Args))
		copy(args, f.Args)
		state[f.Name] = append(state[f.Name], args)
	}
	for {
		changed := false
		for _, rule := range e.Prog.Rules {
			newTuples := dlDeriveRule(rule, state)
			for _, t := range newTuples {
				if !dlTupleInRelation(state[rule.HeadName], t) {
					state[rule.HeadName] = append(state[rule.HeadName], t)
					changed = true
				}
			}
		}
		if !changed {
			break
		}
	}
	rel := state[e.QueryName]
	var out []string
	for _, tuple := range rel {
		if len(tuple) != len(e.QueryArgs) {
			continue
		}
		match := true
		for i, qa := range e.QueryArgs {
			if qa != "" {
				expected := qa
				if len(expected) >= 2 && expected[0] == '"' && expected[len(expected)-1] == '"' {
					expected = expected[1 : len(expected)-1]
				}
				if tuple[i] != expected {
					match = false
					break
				}
			}
		}
		if match {
			for i, qa := range e.QueryArgs {
				if qa == "" {
					out = append(out, tuple[i])
				}
			}
		}
	}
	return out
}

func dlDeriveRule(rule aotir.DatalogRule, state map[string][][]string) [][]string {
	results := []map[string]string{{}}
	for _, lit := range rule.Body {
		if lit.IsNeq {
			var next []map[string]string
			for _, env := range results {
				a, aok := env[lit.NeqA]
				b, bok := env[lit.NeqB]
				if !aok || !bok || a != b {
					next = append(next, env)
				}
			}
			results = next
			continue
		}
		if lit.IsNot {
			var next []map[string]string
			for _, env := range results {
				matched := false
				for _, t := range state[lit.Name] {
					if len(t) != len(lit.Args) {
						continue
					}
					ok := true
					for i, arg := range lit.Args {
						if dlResolveArg(arg, env) != t[i] {
							ok = false
							break
						}
					}
					if ok {
						matched = true
						break
					}
				}
				if !matched {
					next = append(next, env)
				}
			}
			results = next
			continue
		}
		var next []map[string]string
		for _, env := range results {
			for _, t := range state[lit.Name] {
				if len(t) != len(lit.Args) {
					continue
				}
				newEnv := dlCopyEnv(env)
				ok := true
				for i, arg := range lit.Args {
					if dlIsVar(arg) {
						if existing, found := newEnv[arg]; found {
							if existing != t[i] {
								ok = false
								break
							}
						} else {
							newEnv[arg] = t[i]
						}
					} else {
						if dlResolveArg(arg, env) != t[i] {
							ok = false
							break
						}
					}
				}
				if ok {
					next = append(next, newEnv)
				}
			}
		}
		results = next
	}
	var heads [][]string
	for _, env := range results {
		head := make([]string, len(rule.HeadArgs))
		valid := true
		for i, arg := range rule.HeadArgs {
			if dlIsVar(arg) {
				v, ok := env[arg]
				if !ok {
					valid = false
					break
				}
				head[i] = v
			} else {
				head[i] = dlResolveArg(arg, env)
			}
		}
		if valid {
			heads = append(heads, head)
		}
	}
	return heads
}

func dlResolveArg(arg string, env map[string]string) string {
	if len(arg) >= 2 && arg[0] == '"' && arg[len(arg)-1] == '"' {
		return arg[1 : len(arg)-1]
	}
	if v, ok := env[arg]; ok {
		return v
	}
	return arg
}

func dlIsVar(arg string) bool {
	return len(arg) > 0 && arg[0] != '"'
}

func dlTupleInRelation(rel [][]string, t []string) bool {
	for _, r := range rel {
		if len(r) != len(t) {
			continue
		}
		match := true
		for i := range r {
			if r[i] != t[i] {
				match = false
				break
			}
		}
		if match {
			return true
		}
	}
	return false
}

func dlCopyEnv(env map[string]string) map[string]string {
	cp := make(map[string]string, len(env))
	maps.Copy(cp, env)
	return cp
}

// --- Phase 9 agent lowering ---

// rewriteAgentSelfRefs rewrites "__self->field" VarRef/AssignStmt names to bare
// field names so that intent bodies work as Swift class methods (implicit self).
func rewriteAgentSelfRefs(b *aotir.Block, fields []aotir.RecordField) *aotir.Block {
	if b == nil {
		return nil
	}
	// Build rename map: "__self->field" -> "field"
	renames := make(map[string]string, len(fields))
	for _, f := range fields {
		renames["__self->"+f.Name] = f.Name
	}
	stmts := make([]aotir.Stmt, len(b.Statements))
	for i, s := range b.Statements {
		stmts[i] = rewriteAgentStmt(s, renames)
	}
	return &aotir.Block{Statements: stmts}
}

func rewriteAgentStmt(s aotir.Stmt, renames map[string]string) aotir.Stmt {
	switch s := s.(type) {
	case *aotir.ReturnStmt:
		if s.Value == nil {
			return s
		}
		return &aotir.ReturnStmt{Value: rewriteAgentExpr(s.Value, renames)}
	case *aotir.LetStmt:
		cp := *s
		if s.Init != nil {
			cp.Init = rewriteAgentExpr(s.Init, renames)
		}
		return &cp
	case *aotir.AssignStmt:
		cp := *s
		if newName, ok := renames[s.Name]; ok {
			cp.Name = newName
		}
		cp.Value = rewriteAgentExpr(s.Value, renames)
		return &cp
	case *aotir.CallStmt:
		args := make([]aotir.Expr, len(s.Args))
		for i, a := range s.Args {
			args[i] = rewriteAgentExpr(a, renames)
		}
		cp := *s
		cp.Args = args
		return &cp
	case *aotir.AgentIntentCallStmt:
		recv := rewriteAgentExpr(s.Receiver, renames)
		args := make([]aotir.Expr, len(s.Args))
		for i, a := range s.Args {
			args[i] = rewriteAgentExpr(a, renames)
		}
		cp := *s
		cp.Receiver = recv
		cp.Args = args
		return &cp
	case *aotir.IfStmt:
		cp := *s
		cp.Cond = rewriteAgentExpr(s.Cond, renames)
		cp.Then = rewriteAgentBlock(s.Then, renames)
		if s.Else != nil {
			cp.Else = rewriteAgentBlock(s.Else, renames)
		}
		return &cp
	case *aotir.WhileStmt:
		cp := *s
		cp.Cond = rewriteAgentExpr(s.Cond, renames)
		cp.Body = rewriteAgentBlock(s.Body, renames)
		return &cp
	default:
		return s
	}
}

func rewriteAgentBlock(b *aotir.Block, renames map[string]string) *aotir.Block {
	if b == nil {
		return nil
	}
	stmts := make([]aotir.Stmt, len(b.Statements))
	for i, s := range b.Statements {
		stmts[i] = rewriteAgentStmt(s, renames)
	}
	return &aotir.Block{Statements: stmts}
}

func rewriteAgentExpr(e aotir.Expr, renames map[string]string) aotir.Expr {
	if e == nil {
		return nil
	}
	switch e := e.(type) {
	case *aotir.VarRef:
		if newName, ok := renames[e.Name]; ok {
			cp := *e
			cp.Name = newName
			return &cp
		}
		return e
	case *aotir.UnionVarRef:
		if newName, ok := renames[e.Name]; ok {
			cp := *e
			cp.Name = newName
			return &cp
		}
		return e
	case *aotir.BinaryExpr:
		cp := *e
		cp.Left = rewriteAgentExpr(e.Left, renames)
		cp.Right = rewriteAgentExpr(e.Right, renames)
		return &cp
	case *aotir.UnaryExpr:
		cp := *e
		cp.Operand = rewriteAgentExpr(e.Operand, renames)
		return &cp
	case *aotir.AgentIntentCallExpr:
		cp := *e
		cp.Receiver = rewriteAgentExpr(e.Receiver, renames)
		args := make([]aotir.Expr, len(e.Args))
		for i, a := range e.Args {
			args[i] = rewriteAgentExpr(a, renames)
		}
		cp.Args = args
		return &cp
	default:
		return e
	}
}

// lowerAgentDecl converts an AgentDecl to a Swift final class.
func (l *lowerer) lowerAgentDecl(ag *aotir.AgentDecl) (*sxtree.ClassDecl, error) {
	// Build properties.
	props := make([]sxtree.ClassProp, len(ag.Fields))
	for i, f := range ag.Fields {
		props[i] = sxtree.ClassProp{
			Name:     f.Name,
			TypeName: lowerFieldTypeName(f),
		}
	}

	// Build init params.
	initParams := make([]sxtree.FuncParam, len(ag.Fields))
	for i, f := range ag.Fields {
		initParams[i] = sxtree.FuncParam{
			Name:     f.Name,
			TypeName: lowerFieldTypeName(f),
		}
	}

	// Build methods from intents.
	methods := make([]sxtree.FuncDecl, len(ag.Intents))
	for i, intent := range ag.Intents {
		// Rewrite "__self->field" references to bare field names for Swift.
		cleanBody := rewriteAgentSelfRefs(intent.Body, ag.Fields)
		body, err := l.lowerBlock(cleanBody)
		if err != nil {
			return nil, fmt.Errorf("swift/lower: agent %s intent %s: %w", ag.Name, intent.Name, err)
		}

		params := make([]sxtree.FuncParam, len(intent.Params))
		for j, p := range intent.Params {
			params[j] = sxtree.FuncParam{
				Label:    "_",
				Name:     p.Name,
				TypeName: swiftScalarType(p.Type),
			}
		}

		retType := ""
		if intent.ReturnType != aotir.TypeUnit && intent.ReturnType != aotir.TypeInvalid {
			retType = swiftScalarType(intent.ReturnType)
		}

		methods[i] = sxtree.FuncDecl{
			Modifiers:  []string{"public"},
			Name:       intent.Name,
			Params:     params,
			ReturnType: retType,
			Body:       body,
		}
	}

	return &sxtree.ClassDecl{
		Name:    ag.Name,
		Props:   props,
		Inits:   initParams,
		Methods: methods,
	}, nil
}

// lowerAgentLit lowers AgentLit to ClassName(field1: val1, ...).
func (l *lowerer) lowerAgentLit(e *aotir.AgentLit) (sxtree.Expr, error) {
	labeledArgs := make([]string, len(e.Fields))
	for i, f := range e.Fields {
		v, err := l.lowerExpr(f.Value)
		if err != nil {
			return nil, err
		}
		labeledArgs[i] = f.Name + ": " + v.SwiftExprString()
	}
	return &sxtree.RawSwiftExpr{
		Code: e.AgentName + "(" + strings.Join(labeledArgs, ", ") + ")",
	}, nil
}

// lowerAgentIntentCallExpr lowers a value-returning intent call: receiver.method(args...).
func (l *lowerer) lowerAgentIntentCallExpr(e *aotir.AgentIntentCallExpr) (sxtree.Expr, error) {
	if e.SpawnedRef {
		return nil, fmt.Errorf("swift/lower: spawn not yet supported in Swift")
	}
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	args := make([]sxtree.Expr, len(e.Args))
	for i, a := range e.Args {
		ae, err := l.lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args[i] = ae
	}
	return &sxtree.MethodCallExpr{Receiver: recv, Method: e.IntentName, Args: args}, nil
}

// lowerAgentIntentCallStmt lowers a void intent call as a statement.
func (l *lowerer) lowerAgentIntentCallStmt(s *aotir.AgentIntentCallStmt) (sxtree.Stmt, error) {
	if s.SpawnedRef {
		return nil, fmt.Errorf("swift/lower: spawn not yet supported in Swift")
	}
	recv, err := l.lowerExpr(s.Receiver)
	if err != nil {
		return nil, err
	}
	args := make([]sxtree.Expr, len(s.Args))
	for i, a := range s.Args {
		ae, err := l.lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args[i] = ae
	}
	call := &sxtree.MethodCallExpr{Receiver: recv, Method: s.IntentName, Args: args}
	return &sxtree.ExprStmt{X: call}, nil
}

// --- Math and string builtins ---

func (l *lowerer) lowerMathCallExpr(e *aotir.MathCallExpr) (sxtree.Expr, error) {
	arg, err := l.lowerExpr(e.Arg)
	if err != nil {
		return nil, err
	}
	switch e.Func {
	case "abs_i64":
		return &sxtree.CallExpr{Func: "mochiAbsInt", Args: []sxtree.Expr{arg}}, nil
	case "abs_f64":
		return &sxtree.CallExpr{Func: "mochiAbsFloat", Args: []sxtree.Expr{arg}}, nil
	case "floor":
		return &sxtree.CallExpr{Func: "mochiFloor", Args: []sxtree.Expr{arg}}, nil
	case "ceil":
		return &sxtree.CallExpr{Func: "mochiCeil", Args: []sxtree.Expr{arg}}, nil
	default:
		return nil, fmt.Errorf("swift/lower: unknown MathCallExpr func %q", e.Func)
	}
}

func (l *lowerer) lowerStrSubstringExpr(e *aotir.StrSubstringExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	start, err := l.lowerExpr(e.Start)
	if err != nil {
		return nil, err
	}
	end, err := l.lowerExpr(e.End)
	if err != nil {
		return nil, err
	}
	return &sxtree.CallExpr{Func: "mochiStrSubstring", Args: []sxtree.Expr{recv, start, end}}, nil
}

func (l *lowerer) lowerStrReverseExpr(e *aotir.StrReverseExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &sxtree.CallExpr{Func: "mochiStrReverse", Args: []sxtree.Expr{recv}}, nil
}

func (l *lowerer) lowerStrConvertExpr(e *aotir.StrConvertExpr) (sxtree.Expr, error) {
	operand, err := l.lowerExpr(e.Operand)
	if err != nil {
		return nil, err
	}
	return &sxtree.CallExpr{Func: "mochiStr", Args: []sxtree.Expr{operand}}, nil
}

func (l *lowerer) lowerStrUpperExpr(e *aotir.StrUpperExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &sxtree.MethodCallExpr{Receiver: recv, Method: "uppercased", Args: nil}, nil
}

func (l *lowerer) lowerStrLowerExpr(e *aotir.StrLowerExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &sxtree.MethodCallExpr{Receiver: recv, Method: "lowercased", Args: nil}, nil
}

func (l *lowerer) lowerStrSplitExpr(e *aotir.StrSplitExpr) (sxtree.Expr, error) {
	str, err := l.lowerExpr(e.Str)
	if err != nil {
		return nil, err
	}
	sep, err := l.lowerExpr(e.Sep)
	if err != nil {
		return nil, err
	}
	return &sxtree.CallExpr{Func: "mochiStrSplit", Args: []sxtree.Expr{str, sep}}, nil
}

func (l *lowerer) lowerStrJoinExpr(e *aotir.StrJoinExpr) (sxtree.Expr, error) {
	list, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, err
	}
	sep, err := l.lowerExpr(e.Sep)
	if err != nil {
		return nil, err
	}
	return &sxtree.CallExpr{Func: "mochiStrJoin", Args: []sxtree.Expr{list, sep}}, nil
}

func (l *lowerer) lowerListMinExpr(e *aotir.ListMinExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &sxtree.RawSwiftExpr{Code: recv.SwiftExprString() + ".min()!"}, nil
}

func (l *lowerer) lowerListMaxExpr(e *aotir.ListMaxExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &sxtree.RawSwiftExpr{Code: recv.SwiftExprString() + ".max()!"}, nil
}

func (l *lowerer) lowerListSumExpr(e *aotir.ListSumExpr) (sxtree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &sxtree.MethodCallExpr{Receiver: recv, Method: "reduce", Args: []sxtree.Expr{
		&sxtree.IntLitExpr{Value: 0},
		&sxtree.RawSwiftExpr{Code: "+"},
	}}, nil
}

func (l *lowerer) lowerListContainsExpr(e *aotir.ListContainsExpr) (sxtree.Expr, error) {
	list, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(e.Value)
	if err != nil {
		return nil, err
	}
	return &sxtree.MethodCallExpr{Receiver: list, Method: "contains", Args: []sxtree.Expr{val}}, nil
}

func lowerBinOp(op aotir.BinOp) string {
	switch op {
	case aotir.BinAddI64, aotir.BinAddF64:
		return "+"
	case aotir.BinSubI64, aotir.BinSubF64:
		return "-"
	case aotir.BinMulI64, aotir.BinMulF64:
		return "*"
	case aotir.BinDivI64, aotir.BinDivF64:
		return "/"
	case aotir.BinModI64:
		return "%"
	case aotir.BinEqI64, aotir.BinEqF64, aotir.BinEqBool, aotir.BinEqStr, aotir.BinEqRec:
		return "=="
	case aotir.BinNeI64, aotir.BinNeF64, aotir.BinNeBool, aotir.BinNeStr, aotir.BinNeRec:
		return "!="
	case aotir.BinLtI64, aotir.BinLtF64:
		return "<"
	case aotir.BinLeI64, aotir.BinLeF64:
		return "<="
	case aotir.BinGtI64, aotir.BinGtF64:
		return ">"
	case aotir.BinGeI64, aotir.BinGeF64:
		return ">="
	case aotir.BinAndBool:
		return "&&"
	case aotir.BinOrBool:
		return "||"
	default:
		return "?"
	}
}

// formatDouble formats a float64 the same way Go's strconv.FormatFloat 'g' does
// for compatibility with vm3 output.
func formatDouble(v float64) string {
	if math.IsNaN(v) {
		return "Double.nan"
	}
	if math.IsInf(v, 1) {
		return "Double.infinity"
	}
	if math.IsInf(v, -1) {
		return "-Double.infinity"
	}
	s := strconv.FormatFloat(v, 'g', -1, 64)
	// Ensure it looks like a float literal (has a dot or exponent).
	if !strings.Contains(s, ".") && !strings.Contains(s, "e") && !strings.Contains(s, "E") {
		s += ".0"
	}
	return s
}
