// Package lower translates an aotir.Program into a ktree.SourceFile.
// Entry point: Lower(prog) → *ktree.SourceFile.
package lower

import (
	"fmt"
	"math"
	"path/filepath"
	"strconv"
	"strings"
	"unicode"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/kotlin/ktree"
)

type lowerer struct {
	matchBindings map[string]string            // field name → binding var name in current match arm
	agents        map[string]*aotir.AgentDecl  // name → agent decl, for agent lowering
	javaFuncs     map[string]*aotir.JavaFuncDecl // mochiName → Java func decl
	usesLLM       bool                         // true if any LLMGenerateExpr was lowered
	usesHTTP      bool                         // true if any HttpGetExpr was lowered
}

// Lower translates an aotir.Program into a ktree.SourceFile.
func Lower(prog *aotir.Program) (*ktree.SourceFile, error) {
	l := &lowerer{}
	// Track agent declarations for use in lowerExpr (AgentLit, AgentSpawn etc.)
	l.agents = make(map[string]*aotir.AgentDecl, len(prog.Agents))
	for _, ad := range prog.Agents {
		l.agents[ad.Name] = ad
	}
	// Track Java FFI function declarations for call-site redirect.
	l.javaFuncs = make(map[string]*aotir.JavaFuncDecl, len(prog.JavaFuncs))
	for _, jf := range prog.JavaFuncs {
		l.javaFuncs[jf.MochiName] = jf
	}

	mainFn := prog.Functions[prog.Main]
	body, err := l.lowerBlock(mainFn.Body)
	if err != nil {
		return nil, err
	}

	mainFunc := &ktree.FunDecl{
		Name: "main",
		Body: body,
	}

	// Additional user-defined functions (non-main).
	var topDecls []ktree.Decl

	// Type declarations: data classes and sealed classes first.
	for _, rd := range prog.Records {
		topDecls = append(topDecls, lowerRecordDecl(rd))
	}
	for _, ud := range prog.Unions {
		topDecls = append(topDecls, lowerUnionDecl(ud))
	}

	// Agent class declarations.
	for _, ad := range prog.Agents {
		agentDecl, err := lowerAgentDecl(ad)
		if err != nil {
			return nil, fmt.Errorf("kotlin/lower: agent %q: %w", ad.Name, err)
		}
		topDecls = append(topDecls, agentDecl)
	}

	// User-defined functions (non-main).
	for i, fn := range prog.Functions {
		if i == prog.Main {
			continue
		}
		fd, err := l.lowerFunction(fn)
		if err != nil {
			return nil, err
		}
		topDecls = append(topDecls, fd)
	}

	// Helpers used by the generated code.
	topDecls = append(topDecls, mochiHelpers()...)

	// Phase 13: LLM helpers.
	if l.usesLLM {
		topDecls = append(topDecls, mochiLLMHelpers()...)
	}

	// Phase 14: HTTP fetch helpers.
	if l.usesHTTP {
		topDecls = append(topDecls, mochiHttpHelpers()...)
	}

	// Main function last.
	topDecls = append(topDecls, mainFunc)

	sf := &ktree.SourceFile{
		Name:    "Main",
		Package: "mochi.user",
		Decls:   topDecls,
	}
	return sf, nil
}

// FileName converts a Mochi source filename to "Main".
func FileName(_ string) string {
	return "Main"
}

// BaseName converts a Mochi source filename to a Kotlin-friendly name.
func BaseName(src string) string {
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
		return "Main"
	}
	return sb.String()
}

// mochiHelpers returns the helper functions emitted in every generated file.
func mochiHelpers() []ktree.Decl {
	return []ktree.Decl{
		&ktree.RawKtDecl{Code: `
fun mochiStrIndex(s: String, i: Long): String {
    val codePoints = s.codePoints().toArray()
    if (i < 0 || i >= codePoints.size) return ""
    return String(Character.toChars(codePoints[i.toInt()]))
}

fun mochiStrLen(s: String): Long = s.codePoints().count()

fun mochiStrSubstring(s: String, start: Long, end: Long): String {
    val codePoints = s.codePoints().toArray()
    val from = start.toInt().coerceIn(0, codePoints.size)
    val to = end.toInt().coerceIn(0, codePoints.size)
    if (from >= to) return ""
    return String(codePoints, from, to - from)
}

fun mochiStrContains(s: String, sub: String): Boolean = s.contains(sub)

fun mochiStrReverse(s: String): String {
    val codePoints = s.codePoints().toArray()
    return String(codePoints.reversedArray(), 0, codePoints.size)
}

fun mochiStr(x: Any?): String = when (x) {
    is Boolean -> if (x) "true" else "false"
    is Long -> x.toString()
    is Double -> mochiDoubleToStr(x)
    else -> x.toString()
}

fun mochiDoubleToStr(v: Double): String {
    if (v.isNaN()) return "nan"
    if (v.isInfinite()) return if (v > 0) "inf" else "-inf"
    val s = v.toBigDecimal().stripTrailingZeros().toPlainString()
    return if ('.' !in s && 'e' !in s && 'E' !in s) "$s.0" else s
}

fun mochiFloorDiv(a: Long, b: Long): Long = Math.floorDiv(a, b)
fun mochiFloorMod(a: Long, b: Long): Long = Math.floorMod(a, b)

fun mochiAbsInt(x: Long): Long = Math.abs(x)
fun mochiAbsFloat(x: Double): Double = Math.abs(x)
fun mochiFloor(x: Double): Double = Math.floor(x)
fun mochiCeil(x: Double): Double = Math.ceil(x)
fun mochiFloatToInt(x: Double): Long = x.toLong()

fun mochiStrSplit(s: String, sep: String): MutableList<String> {
    if (sep.isEmpty()) {
        return s.codePoints().toArray().map { String(Character.toChars(it)) }.toMutableList()
    }
    return s.split(sep).toMutableList()
}

fun mochiStrJoin(xs: MutableList<String>, sep: String): String = xs.joinToString(sep)
`},
	}
}

// lowerRecordDecl converts an aotir.RecordDecl to a Kotlin data class.
func lowerRecordDecl(rd *aotir.RecordDecl) ktree.Decl {
	fields := make([]ktree.DataClassField, len(rd.Fields))
	for i, f := range rd.Fields {
		fields[i] = ktree.DataClassField{
			Name:     f.Name,
			TypeName: lowerFieldTypeName(f),
		}
	}
	return &ktree.DataClassDecl{
		Name:   rd.Name,
		Fields: fields,
	}
}

// lowerUnionDecl converts an aotir.UnionDecl to a Kotlin sealed class.
func lowerUnionDecl(ud *aotir.UnionDecl) ktree.Decl {
	subclasses := make([]ktree.SealedSubclass, len(ud.Variants))
	for i, v := range ud.Variants {
		fields := make([]ktree.DataClassField, len(v.Fields))
		for j, f := range v.Fields {
			fields[j] = ktree.DataClassField{
				Name:     f.Name,
				TypeName: lowerVariantFieldTypeName(f),
			}
		}
		subclasses[i] = ktree.SealedSubclass{
			Name:   v.Name,
			Fields: fields,
		}
	}
	return &ktree.SealedClassDecl{
		Name:       ud.Name,
		Subclasses: subclasses,
	}
}

func lowerFieldTypeName(f aotir.RecordField) string {
	switch f.Type {
	case aotir.TypeInt:
		return "Long"
	case aotir.TypeFloat:
		return "Double"
	case aotir.TypeBool:
		return "Boolean"
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

func lowerVariantFieldTypeName(f aotir.VariantField) string {
	switch f.FieldType {
	case aotir.TypeInt:
		return "Long"
	case aotir.TypeFloat:
		return "Double"
	case aotir.TypeBool:
		return "Boolean"
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

func (l *lowerer) lowerFunction(fn *aotir.Function) (*ktree.FunDecl, error) {
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
	// Prepend capture params for lifted functions.
	if fn.IsLifted && len(fn.Captures) > 0 {
		capParams := make([]ktree.FunParam, len(fn.Captures))
		for i, cap := range fn.Captures {
			capParams[i] = ktree.FunParam{
				Name:     cap.FieldName,
				TypeName: ktScalarType(cap.VarType),
			}
		}
		params = append(capParams, params...)
	}
	return &ktree.FunDecl{
		Modifiers:  []string{"private"},
		Name:       fn.Name,
		Params:     params,
		ReturnType: retType,
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
		return "Long"
	case aotir.TypeFloat:
		return "Double"
	case aotir.TypeBool:
		return "Boolean"
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
		return ktListType(fn.ReturnElemType, fn.ReturnElemRecordName)
	case aotir.TypeMap:
		return ktMapType(fn.ReturnKeyType, fn.ReturnValueType)
	case aotir.TypeSet:
		return "LinkedHashSet<" + ktScalarType(fn.ReturnElemType) + ">"
	case aotir.TypeFun:
		return ktFunType(fn.ReturnFunSig)
	case aotir.TypeUnit, aotir.TypeInvalid:
		return ""
	default:
		return ""
	}
}

func ktScalarType(t aotir.Type) string {
	switch t {
	case aotir.TypeInt:
		return "Long"
	case aotir.TypeFloat:
		return "Double"
	case aotir.TypeBool:
		return "Boolean"
	case aotir.TypeString:
		return "String"
	default:
		return "Any"
	}
}

func ktListType(elemType aotir.Type, elemRecordName string) string {
	if elemType == aotir.TypeRecord && elemRecordName != "" {
		return "MutableList<" + elemRecordName + ">"
	}
	return "MutableList<" + ktScalarType(elemType) + ">"
}

func ktMapType(keyType, valueType aotir.Type) string {
	return "LinkedHashMap<" + ktScalarType(keyType) + ", " + ktScalarType(valueType) + ">"
}

func ktFunType(sig *aotir.FunSig) string {
	if sig == nil {
		return "(Any) -> Any"
	}
	params := make([]string, len(sig.ParamTypes))
	for i, p := range sig.ParamTypes {
		params[i] = ktScalarType(p)
	}
	ret := ktScalarType(sig.ReturnType)
	if sig.ReturnType == aotir.TypeUnit {
		ret = "Unit"
	}
	return "(" + strings.Join(params, ", ") + ") -> " + ret
}

func lowerParams(params []aotir.Param) []ktree.FunParam {
	result := make([]ktree.FunParam, len(params))
	for i, p := range params {
		result[i] = ktree.FunParam{
			Name:     p.Name,
			TypeName: lowerParamTypeName(p),
		}
	}
	return result
}

func lowerParamTypeName(p aotir.Param) string {
	switch p.Type {
	case aotir.TypeInt:
		return "Long"
	case aotir.TypeFloat:
		return "Double"
	case aotir.TypeBool:
		return "Boolean"
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
		return ktListType(p.ElemType, p.ElemRecordName)
	case aotir.TypeMap:
		return ktMapType(p.KeyType, p.ValueType)
	case aotir.TypeSet:
		return "LinkedHashSet<" + ktScalarType(p.ElemType) + ">"
	case aotir.TypeFun:
		return ktFunType(p.FunSig)
	default:
		return "Any"
	}
}

func (l *lowerer) lowerBlock(b *aotir.Block) ([]ktree.Stmt, error) {
	if b == nil {
		return nil, nil
	}
	stmts := make([]ktree.Stmt, 0, len(b.Statements))
	raw := b.Statements
	for i := 0; i < len(raw); i++ {
		s := raw[i]
		// If we see an uninitialized mutable LetStmt for a variable that is immediately
		// used as the ResultVar of the next MatchStmt, skip the LetStmt — our
		// WhenAssignStmt will declare and initialize it as a val in one shot.
		if let, ok := s.(*aotir.LetStmt); ok && let.Mutable && let.Init == nil {
			if i+1 < len(raw) {
				if ms, ok2 := raw[i+1].(*aotir.MatchStmt); ok2 && ms.ResultVar == let.Name {
					// Skip this bare declaration; the WhenAssignStmt handles it.
					continue
				}
			}
		}
		ss, err := l.lowerStmt(s)
		if err != nil {
			return nil, err
		}
		stmts = append(stmts, ss)
	}
	return stmts, nil
}

func (l *lowerer) lowerStmt(s aotir.Stmt) (ktree.Stmt, error) {
	switch s := s.(type) {
	case *aotir.CallStmt:
		return l.lowerCallStmt(s)
	case *aotir.ReturnStmt:
		if s.Value == nil {
			return &ktree.ReturnStmt{}, nil
		}
		v, err := l.lowerExpr(s.Value)
		if err != nil {
			return nil, err
		}
		return &ktree.ReturnStmt{Value: v}, nil
	case *aotir.LetStmt:
		return l.lowerLetStmt(s)
	case *aotir.AssignStmt:
		v, err := l.lowerExpr(s.Value)
		if err != nil {
			return nil, err
		}
		return &ktree.AssignStmt{Target: s.Name, Value: v}, nil
	case *aotir.IfStmt:
		return l.lowerIfStmt(s)
	case *aotir.WhileStmt:
		return l.lowerWhileStmt(s)
	case *aotir.ForEachStmt:
		return l.lowerForEachStmt(s)
	case *aotir.ForRangeStmt:
		return l.lowerForRangeStmt(s)
	case *aotir.BreakStmt:
		return &ktree.BreakStmt{}, nil
	case *aotir.ContinueStmt:
		return &ktree.ContinueStmt{}, nil
	case *aotir.RawCStmt:
		return &ktree.RawKtStmt{Code: "// (raw C stmt omitted)"}, nil
	case *aotir.ClosureEnvStmt:
		return &ktree.RawKtStmt{Code: "// (closure env omitted)"}, nil
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
	case *aotir.ChanSendStmt:
		return l.lowerChanSendStmt(s)
	default:
		return nil, fmt.Errorf("kotlin/lower: unsupported statement %T", s)
	}
}

func (l *lowerer) lowerCallStmt(s *aotir.CallStmt) (ktree.Stmt, error) {
	switch s.Func {
	case "mochi_print_str":
		if len(s.Args) != 1 {
			return nil, fmt.Errorf("kotlin/lower: %s wants 1 arg, got %d", s.Func, len(s.Args))
		}
		arg, err := l.lowerExpr(s.Args[0])
		if err != nil {
			return nil, err
		}
		call := &ktree.CallExpr{Func: "println", Args: []ktree.Expr{arg}}
		return &ktree.ExprStmt{X: call}, nil
	case "mochi_print_i64":
		if len(s.Args) != 1 {
			return nil, fmt.Errorf("kotlin/lower: %s wants 1 arg, got %d", s.Func, len(s.Args))
		}
		arg, err := l.lowerExpr(s.Args[0])
		if err != nil {
			return nil, err
		}
		call := &ktree.CallExpr{Func: "println", Args: []ktree.Expr{arg}}
		return &ktree.ExprStmt{X: call}, nil
	case "mochi_print_f64":
		if len(s.Args) != 1 {
			return nil, fmt.Errorf("kotlin/lower: %s wants 1 arg, got %d", s.Func, len(s.Args))
		}
		arg, err := l.lowerExpr(s.Args[0])
		if err != nil {
			return nil, err
		}
		// Use mochiDoubleToStr to match vm3 output format.
		call := &ktree.CallExpr{
			Func: "println",
			Args: []ktree.Expr{&ktree.CallExpr{Func: "mochiDoubleToStr", Args: []ktree.Expr{arg}}},
		}
		return &ktree.ExprStmt{X: call}, nil
	case "mochi_print_bool":
		if len(s.Args) != 1 {
			return nil, fmt.Errorf("kotlin/lower: %s wants 1 arg, got %d", s.Func, len(s.Args))
		}
		arg, err := l.lowerExpr(s.Args[0])
		if err != nil {
			return nil, err
		}
		// Boolean.toString() returns "true"/"false" in lowercase.
		call := &ktree.CallExpr{Func: "println", Args: []ktree.Expr{arg}}
		return &ktree.ExprStmt{X: call}, nil
	default:
		// Phase 12: if this is a Java FFI call, redirect to the Java method.
		if decl, ok := l.javaFuncs[s.Func]; ok {
			fakeExpr := &aotir.JavaCallExpr{Decl: decl, Args: s.Args, Result: aotir.TypeUnit}
			e, err := l.lowerJavaCallExpr(fakeExpr)
			if err != nil {
				return nil, err
			}
			return &ktree.ExprStmt{X: e}, nil
		}
		if !strings.HasPrefix(s.Func, "mochi_") {
			// User-defined function call.
			args := make([]ktree.Expr, len(s.Args))
			for i, a := range s.Args {
				e, err := l.lowerExpr(a)
				if err != nil {
					return nil, err
				}
				args[i] = e
			}
			call := &ktree.CallExpr{
				Func: s.Func,
				Args: args,
			}
			return &ktree.ExprStmt{X: call}, nil
		}
		return nil, fmt.Errorf("kotlin/lower: unsupported builtin %q", s.Func)
	}
}

func (l *lowerer) lowerLetStmt(s *aotir.LetStmt) (ktree.Stmt, error) {
	var init ktree.Expr
	var err error
	if s.Init != nil {
		init, err = l.lowerExpr(s.Init)
		if err != nil {
			return nil, err
		}
	}
	typeName := lowerLetTypeName(s)
	return &ktree.LocalVarStmt{
		IsVar:    s.Mutable,
		Name:     s.Name,
		TypeName: typeName,
		Init:     init,
	}, nil
}

func lowerLetTypeName(s *aotir.LetStmt) string {
	switch s.VarType {
	case aotir.TypeInt:
		return "Long"
	case aotir.TypeFloat:
		return "Double"
	case aotir.TypeBool:
		return "Boolean"
	case aotir.TypeString:
		return "String"
	case aotir.TypeRecord:
		return s.RecordName
	case aotir.TypeUnion:
		return s.UnionName
	case aotir.TypeList:
		return ktListType(s.ElemType, s.ElemRecordName)
	case aotir.TypeMap:
		return ktMapType(s.KeyType, s.ValueType)
	case aotir.TypeSet:
		return "LinkedHashSet<" + ktScalarType(s.ElemType) + ">"
	case aotir.TypeFun:
		return ktFunType(s.FunSig)
	case aotir.TypeChan:
		return lowerChanLetTypeName(s.ChanElemType)
	case aotir.TypeFuture:
		return lowerFutureLetTypeName(s.FutureElemType)
	case aotir.TypeAgent:
		return s.AgentName + "Agent"
	default:
		return ""
	}
}

func (l *lowerer) lowerIfStmt(s *aotir.IfStmt) (ktree.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	then, err := l.lowerBlock(s.Then)
	if err != nil {
		return nil, err
	}
	result := &ktree.IfStmt{Cond: cond, Then: then}
	if s.Else != nil {
		elseStmts, err := l.lowerBlock(s.Else)
		if err != nil {
			return nil, err
		}
		result.Else = elseStmts
	}
	return result, nil
}

func (l *lowerer) lowerWhileStmt(s *aotir.WhileStmt) (ktree.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	return &ktree.WhileStmt{Cond: cond, Body: body}, nil
}

func (l *lowerer) lowerForEachStmt(s *aotir.ForEachStmt) (ktree.Stmt, error) {
	iter, err := l.lowerExpr(s.List)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	return &ktree.ForInStmt{
		Var:        s.Var,
		Collection: iter,
		Body:       body,
	}, nil
}

func (l *lowerer) lowerForRangeStmt(s *aotir.ForRangeStmt) (ktree.Stmt, error) {
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
	return &ktree.ForRangeStmt{
		Var:   s.Var,
		Start: start,
		End:   end,
		Body:  body,
	}, nil
}

func (l *lowerer) lowerMapPutStmt(s *aotir.MapPutStmt) (ktree.Stmt, error) {
	key, err := l.lowerExpr(s.Key)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	return &ktree.IndexSetStmt{
		Receiver: s.Name,
		Index:    key,
		Value:    val,
	}, nil
}

func (l *lowerer) lowerListSetStmt(s *aotir.ListSetStmt) (ktree.Stmt, error) {
	idx, err := l.lowerExpr(s.Index)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	// Kotlin list index must be Int.
	castIdx := &ktree.RawKtExpr{Code: "(" + idx.KtExprString() + ").toInt()"}
	return &ktree.IndexSetStmt{
		Receiver: s.Name,
		Index:    castIdx,
		Value:    val,
	}, nil
}

// multiStmt is a pseudo-statement that renders multiple statements at the same indent level.
type multiStmt struct {
	stmts []ktree.Stmt
}

func (m *multiStmt) KtString(ind int) string {
	parts := make([]string, len(m.stmts))
	for i, s := range m.stmts {
		parts[i] = s.KtString(ind)
	}
	return strings.Join(parts, "\n")
}

// lowerMatchStmt lowers an aotir.MatchStmt to a Kotlin when statement.
// When s.ResultVar is non-empty (match-as-expression), it emits a WhenAssignStmt
// so that Kotlin's definite-assignment rules are satisfied.
func (l *lowerer) lowerMatchStmt(s *aotir.MatchStmt) (ktree.Stmt, error) {
	target, err := l.lowerExpr(s.Target)
	if err != nil {
		return nil, err
	}

	// For field bindings, we need the actual subject expression string.
	subjectCode := target.KtExprString()

	isExpr := s.ResultVar != ""

	var cases []ktree.WhenCase
	for _, arm := range s.Arms {
		c, err := l.lowerMatchArm(&arm, s.UnionName, subjectCode)
		if err != nil {
			return nil, fmt.Errorf("match arm %q: %w", arm.VariantName, err)
		}
		if isExpr {
			// Strip the trailing AssignStmt to ResultVar; use the RHS as ResultExpr.
			c = stripResultAssign(c, s.ResultVar)
		}
		cases = append(cases, c)
	}
	if s.Default != nil {
		dc, err := l.lowerMatchDefaultArm(s.Default)
		if err != nil {
			return nil, fmt.Errorf("match default arm: %w", err)
		}
		if isExpr {
			dc = stripResultAssign(dc, s.ResultVar)
		}
		cases = append(cases, dc)
	} else if !isExpr {
		cases = append(cases, ktree.WhenCase{
			Pattern: "else",
			Body:    []ktree.Stmt{},
		})
	} else {
		// For expression form, add else -> error to satisfy exhaustiveness.
		cases = append(cases, ktree.WhenCase{
			Pattern:    "else",
			ResultExpr: &ktree.RawKtExpr{Code: `error("unmatched")`},
		})
	}

	if isExpr {
		typeName := matchResultTypeName(s)
		return &ktree.WhenAssignStmt{
			IsVar:    false,
			Target:   s.ResultVar,
			TypeName: typeName,
			Subject:  target,
			Cases:    cases,
		}, nil
	}

	return &ktree.WhenStmt{Subject: target, Cases: cases}, nil
}

// stripResultAssign removes the last statement in a WhenCase if it is an
// AssignStmt to resultVar, and moves the RHS to WhenCase.ResultExpr.
func stripResultAssign(c ktree.WhenCase, resultVar string) ktree.WhenCase {
	if len(c.Body) == 0 {
		return c
	}
	last := c.Body[len(c.Body)-1]
	if as, ok := last.(*ktree.AssignStmt); ok && as.Target == resultVar {
		c.Body = c.Body[:len(c.Body)-1]
		c.ResultExpr = as.Value
	}
	return c
}

// matchResultTypeName derives the Kotlin type annotation for the result variable of a match expression.
func matchResultTypeName(s *aotir.MatchStmt) string {
	switch s.ResultType {
	case aotir.TypeInt:
		return "Long"
	case aotir.TypeFloat:
		return "Double"
	case aotir.TypeBool:
		return "Boolean"
	case aotir.TypeString:
		return "String"
	case aotir.TypeUnion:
		if s.ResultUnionName != "" {
			return s.ResultUnionName
		}
		return "Any"
	case aotir.TypeRecord:
		if s.ResultRecordName != "" {
			return s.ResultRecordName
		}
		return "Any"
	default:
		return "Any"
	}
}

func (l *lowerer) lowerMatchArm(arm *aotir.MatchArm, unionName string, subjectCode string) (ktree.WhenCase, error) {
	// Pattern: UnionName.VariantName
	pattern := unionName + "." + arm.VariantName

	// Set up binding map.
	saved := l.matchBindings
	l.matchBindings = make(map[string]string, len(arm.Bindings))
	for _, b := range arm.Bindings {
		l.matchBindings[b.FieldName] = b.VarName
	}

	// Add val bindings as local var stmts in the body using the actual subject.
	var extraStmts []ktree.Stmt
	if len(arm.Bindings) > 0 {
		castExpr := "(" + subjectCode + " as " + unionName + "." + arm.VariantName + ")"
		for _, b := range arm.Bindings {
			extraStmts = append(extraStmts, &ktree.LocalVarStmt{
				IsVar: false,
				Name:  b.VarName,
				Init: &ktree.RawKtExpr{
					Code: castExpr + "." + b.FieldName,
				},
			})
		}
	}

	body, err := l.lowerBlock(arm.Body)
	if err != nil {
		return ktree.WhenCase{}, err
	}
	defer func() { l.matchBindings = saved }()

	return ktree.WhenCase{Pattern: pattern, Body: append(extraStmts, body...)}, nil
}

func (l *lowerer) lowerMatchDefaultArm(arm *aotir.MatchArm) (ktree.WhenCase, error) {
	body, err := l.lowerBlock(arm.Body)
	if err != nil {
		return ktree.WhenCase{}, err
	}
	return ktree.WhenCase{Pattern: "else", Body: body}, nil
}

// lowerQueryScopeStmt lowers a QueryScopeStmt by lowering its body block.
func (l *lowerer) lowerQueryScopeStmt(s *aotir.QueryScopeStmt) (ktree.Stmt, error) {
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, fmt.Errorf("kotlin/lower: QueryScopeStmt body: %w", err)
	}
	if len(body) == 1 {
		return body[0], nil
	}
	return &multiStmt{stmts: body}, nil
}

func (l *lowerer) lowerExpr(e aotir.Expr) (ktree.Expr, error) {
	switch e := e.(type) {
	case *aotir.StringLit:
		return &ktree.StringLitExpr{Value: e.Value}, nil
	case *aotir.IntLit:
		return &ktree.LongLitExpr{Value: e.Value}, nil
	case *aotir.FloatLit:
		return &ktree.DoubleLitExpr{Repr: formatDouble(e.Value)}, nil
	case *aotir.BoolLit:
		return &ktree.BoolLitExpr{Value: e.Value}, nil
	case *aotir.VarRef:
		return &ktree.IdentExpr{Name: e.Name}, nil
	case *aotir.UnionVarRef:
		return &ktree.IdentExpr{Name: e.Name}, nil
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
		return &ktree.CallExpr{
			Func: "mochiFloatToInt",
			Args: []ktree.Expr{operand},
		}, nil
	case *aotir.StrLenExpr:
		recv, err := l.lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		return &ktree.CallExpr{Func: "mochiStrLen", Args: []ktree.Expr{recv}}, nil
	case *aotir.StrIndexExpr:
		recv, err := l.lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		idx, err := l.lowerExpr(e.Index)
		if err != nil {
			return nil, err
		}
		return &ktree.CallExpr{Func: "mochiStrIndex", Args: []ktree.Expr{recv, idx}}, nil
	case *aotir.StrContainsExpr:
		recv, err := l.lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		sub, err := l.lowerExpr(e.Sub)
		if err != nil {
			return nil, err
		}
		return &ktree.CallExpr{Func: "mochiStrContains", Args: []ktree.Expr{recv, sub}}, nil
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
	case *aotir.RecordLit:
		return l.lowerRecordLit(e)
	case *aotir.FieldAccess:
		return l.lowerFieldAccess(e)
	case *aotir.VariantLit:
		return l.lowerVariantLit(e)
	case *aotir.VariantFieldAccess:
		return l.lowerVariantFieldAccess(e)
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
	case *aotir.ListSortAscExpr:
		return l.lowerListSortAscExpr(e)
	case *aotir.ListSliceExpr:
		return l.lowerListSliceExpr(e)
	case *aotir.MathCallExpr:
		return l.lowerMathCallExpr(e)
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
	case *aotir.DatalogQueryExpr:
		return l.lowerDatalogQueryExpr(e)
	case *aotir.AgentLit:
		return l.lowerAgentLitExpr(e)
	case *aotir.AgentSpawnExpr:
		return l.lowerAgentSpawnExpr(e)
	case *aotir.AgentIntentCallExpr:
		return l.lowerAgentIntentCallExpr(e)
	case *aotir.ChanMakeExpr:
		return l.lowerChanMakeExpr(e)
	case *aotir.ChanRecvExpr:
		return l.lowerChanRecvExpr(e)
	case *aotir.AsyncExpr:
		return l.lowerAsyncExpr(e)
	case *aotir.AwaitExpr:
		return l.lowerAwaitExpr(e)
	case *aotir.JavaCallExpr:
		return l.lowerJavaCallExpr(e)
	case *aotir.LLMGenerateExpr:
		l.usesLLM = true
		return l.lowerLLMGenerateExpr(e)
	case *aotir.HttpGetExpr:
		l.usesHTTP = true
		return l.lowerHttpGetExpr(e)
	case *aotir.JsonDecodeExpr:
		return l.lowerJsonDecodeExpr(e)
	default:
		return nil, fmt.Errorf("kotlin/lower: unsupported expression %T", e)
	}
}

func (l *lowerer) lowerBinaryExpr(e *aotir.BinaryExpr) (ktree.Expr, error) {
	left, err := l.lowerExpr(e.Left)
	if err != nil {
		return nil, err
	}
	right, err := l.lowerExpr(e.Right)
	if err != nil {
		return nil, err
	}
	if e.Op == aotir.BinStrCat {
		return &ktree.BinaryExpr{Left: left, Op: "+", Right: right}, nil
	}
	// Floor division and modulo for integers.
	if e.Op == aotir.BinDivI64 {
		return &ktree.CallExpr{Func: "mochiFloorDiv", Args: []ktree.Expr{left, right}}, nil
	}
	if e.Op == aotir.BinModI64 {
		return &ktree.CallExpr{Func: "mochiFloorMod", Args: []ktree.Expr{left, right}}, nil
	}
	if e.Op == aotir.BinEqList || e.Op == aotir.BinNeList {
		eqCall := &ktree.BinaryExpr{Left: left, Op: "==", Right: right}
		if e.Op == aotir.BinNeList {
			return &ktree.UnaryExpr{Op: "!", Operand: eqCall}, nil
		}
		return eqCall, nil
	}
	op := lowerBinOp(e.Op)
	return &ktree.BinaryExpr{Left: left, Op: op, Right: right}, nil
}

func (l *lowerer) lowerUnaryExpr(e *aotir.UnaryExpr) (ktree.Expr, error) {
	operand, err := l.lowerExpr(e.Operand)
	if err != nil {
		return nil, err
	}
	switch e.Op {
	case aotir.UnNegI64, aotir.UnNegF64:
		return &ktree.UnaryExpr{Op: "-", Operand: operand}, nil
	case aotir.UnNotBool:
		return &ktree.UnaryExpr{Op: "!", Operand: operand}, nil
	default:
		return nil, fmt.Errorf("kotlin/lower: unsupported unary op %v", e.Op)
	}
}

func (l *lowerer) lowerCallExpr(e *aotir.CallExpr) (ktree.Expr, error) {
	args := make([]ktree.Expr, len(e.Args))
	for i, a := range e.Args {
		se, err := l.lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args[i] = se
	}
	// Phase 12: if this is a Java FFI call, redirect to the Java method.
	if decl, ok := l.javaFuncs[e.Func]; ok {
		return l.lowerJavaCallExpr(&aotir.JavaCallExpr{Decl: decl, Args: e.Args, Result: e.Result})
	}
	return &ktree.CallExpr{
		Func: e.Func,
		Args: args,
	}, nil
}

// --- Collection lowering helpers ---

func (l *lowerer) lowerListLit(e *aotir.ListLit) (ktree.Expr, error) {
	elems := make([]ktree.Expr, len(e.Elems))
	for i, elem := range e.Elems {
		se, err := l.lowerExpr(elem)
		if err != nil {
			return nil, err
		}
		elems[i] = se
	}
	return &ktree.ListLitExpr{
		ElemType: ktScalarType(e.ElemType),
		Elems:    elems,
	}, nil
}

func (l *lowerer) lowerIndexExpr(e *aotir.IndexExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	idx, err := l.lowerExpr(e.Index)
	if err != nil {
		return nil, err
	}
	// Kotlin list indexing requires Int.
	castIdx := &ktree.RawKtExpr{Code: "(" + idx.KtExprString() + ").toInt()"}
	return &ktree.SubscriptExpr{Receiver: recv, Index: castIdx}, nil
}

func (l *lowerer) lowerLenExpr(e *aotir.LenExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	// .size.toLong()
	sizeMember := &ktree.MemberExpr{Receiver: recv, Member: "size"}
	return &ktree.RawKtExpr{Code: sizeMember.KtExprString() + ".toLong()"}, nil
}

func (l *lowerer) lowerAppendExpr(e *aotir.AppendExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(e.Value)
	if err != nil {
		return nil, err
	}
	// Kotlin: create new list = old + [elem]
	// Use (recv.toMutableList().apply { add(val) }) for functional append.
	code := fmt.Sprintf("(%s.toMutableList().also { it.add(%s) })", recv.KtExprString(), val.KtExprString())
	return &ktree.RawKtExpr{Code: code}, nil
}

func (l *lowerer) lowerMapLit(e *aotir.MapLit) (ktree.Expr, error) {
	keys := make([]ktree.Expr, len(e.Keys))
	vals := make([]ktree.Expr, len(e.Values))
	for i := range e.Keys {
		k, err := l.lowerExpr(e.Keys[i])
		if err != nil {
			return nil, err
		}
		v, err := l.lowerExpr(e.Values[i])
		if err != nil {
			return nil, err
		}
		keys[i] = k
		vals[i] = v
	}
	return &ktree.MapLitExpr{Keys: keys, Values: vals}, nil
}

func (l *lowerer) lowerMapGetExpr(e *aotir.MapGetExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	key, err := l.lowerExpr(e.Key)
	if err != nil {
		return nil, err
	}
	// m[key]!! — non-null assertion (Mochi panics on missing key)
	return &ktree.RawKtExpr{Code: recv.KtExprString() + "[" + key.KtExprString() + "]!!"}, nil
}

func (l *lowerer) lowerMapHasExpr(e *aotir.MapHasExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	key, err := l.lowerExpr(e.Key)
	if err != nil {
		return nil, err
	}
	return &ktree.RawKtExpr{Code: recv.KtExprString() + ".containsKey(" + key.KtExprString() + ")"}, nil
}

func (l *lowerer) lowerMapLenExpr(e *aotir.MapLenExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &ktree.RawKtExpr{Code: recv.KtExprString() + ".size.toLong()"}, nil
}

func (l *lowerer) lowerMapKeysExpr(e *aotir.MapKeysExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &ktree.RawKtExpr{Code: recv.KtExprString() + ".keys.toMutableList()"}, nil
}

func (l *lowerer) lowerMapValuesExpr(e *aotir.MapValuesExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &ktree.RawKtExpr{Code: recv.KtExprString() + ".values.toMutableList()"}, nil
}

func (l *lowerer) lowerSetLiteralExpr(e *aotir.SetLiteralExpr) (ktree.Expr, error) {
	elems := make([]ktree.Expr, len(e.Elems))
	for i, elem := range e.Elems {
		se, err := l.lowerExpr(elem)
		if err != nil {
			return nil, err
		}
		elems[i] = se
	}
	return &ktree.SetLitExpr{
		ElemType: ktScalarType(e.ElemType),
		Elems:    elems,
	}, nil
}

func (l *lowerer) lowerSetAddExpr(e *aotir.SetAddExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	elem, err := l.lowerExpr(e.Elem)
	if err != nil {
		return nil, err
	}
	code := fmt.Sprintf("(%s.toMutableSet().also { it.add(%s) } as LinkedHashSet)", recv.KtExprString(), elem.KtExprString())
	return &ktree.RawKtExpr{Code: code}, nil
}

func (l *lowerer) lowerSetHasExpr(e *aotir.SetHasExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	elem, err := l.lowerExpr(e.Elem)
	if err != nil {
		return nil, err
	}
	return &ktree.MethodCallExpr{Receiver: recv, Method: "contains", Args: []ktree.Expr{elem}}, nil
}

func (l *lowerer) lowerSetLenExpr(e *aotir.SetLenExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &ktree.RawKtExpr{Code: recv.KtExprString() + ".size.toLong()"}, nil
}

func (l *lowerer) lowerSetToListExpr(e *aotir.SetToListExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &ktree.RawKtExpr{Code: recv.KtExprString() + ".toMutableList()"}, nil
}

// --- Record lowering helpers ---

func (l *lowerer) lowerRecordLit(e *aotir.RecordLit) (ktree.Expr, error) {
	labeledArgs := make([]string, len(e.Fields))
	for i, f := range e.Fields {
		v, err := l.lowerExpr(f.Value)
		if err != nil {
			return nil, err
		}
		labeledArgs[i] = f.Name + " = " + v.KtExprString()
	}
	return &ktree.RawKtExpr{
		Code: e.TypeName + "(" + strings.Join(labeledArgs, ", ") + ")",
	}, nil
}

func (l *lowerer) lowerFieldAccess(e *aotir.FieldAccess) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &ktree.MemberExpr{Receiver: recv, Member: e.FieldName}, nil
}

// --- Sum type lowering helpers ---

func (l *lowerer) lowerVariantLit(e *aotir.VariantLit) (ktree.Expr, error) {
	if len(e.Fields) == 0 {
		return &ktree.RawKtExpr{Code: e.UnionName + "." + e.VariantName}, nil
	}
	labeledArgs := make([]string, len(e.Fields))
	for i, f := range e.Fields {
		v, err := l.lowerExpr(f.Value)
		if err != nil {
			return nil, err
		}
		labeledArgs[i] = f.Name + " = " + v.KtExprString()
	}
	return &ktree.RawKtExpr{
		Code: e.UnionName + "." + e.VariantName + "(" + strings.Join(labeledArgs, ", ") + ")",
	}, nil
}

func (l *lowerer) lowerVariantFieldAccess(e *aotir.VariantFieldAccess) (ktree.Expr, error) {
	if l.matchBindings != nil {
		if varName, ok := l.matchBindings[e.FieldName]; ok {
			return &ktree.IdentExpr{Name: varName}, nil
		}
	}
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &ktree.MemberExpr{Receiver: recv, Member: e.FieldName}, nil
}

// --- Closure/HOF lowering helpers ---

func (l *lowerer) lowerFunLit(e *aotir.FunLit) (ktree.Expr, error) {
	sig := e.Sig
	if sig == nil {
		return nil, fmt.Errorf("kotlin/lower: FunLit %q has nil Sig", e.FuncName)
	}

	paramNames := make([]string, len(sig.ParamTypes))
	for i := range sig.ParamTypes {
		paramNames[i] = fmt.Sprintf("__p%d", i)
	}

	callArgs := make([]ktree.Expr, 0, len(e.Captures)+len(paramNames))
	for _, cap := range e.Captures {
		callArgs = append(callArgs, &ktree.IdentExpr{Name: cap.SrcName})
	}
	for _, pn := range paramNames {
		callArgs = append(callArgs, &ktree.IdentExpr{Name: pn})
	}

	callExpr := &ktree.CallExpr{
		Func: e.FuncName,
		Args: callArgs,
	}

	// Kotlin lambdas use the last expression as the implicit return value;
	// `return` is not valid inside a lambda body.
	body := []ktree.Stmt{&ktree.ExprStmt{X: callExpr}}

	return &ktree.ClosureLitExpr{Params: paramNames, Body: body}, nil
}

func (l *lowerer) lowerFunCallExpr(e *aotir.FunCallExpr) (ktree.Expr, error) {
	callee, err := l.lowerExpr(e.Callee)
	if err != nil {
		return nil, err
	}
	args := make([]ktree.Expr, len(e.Args))
	for i, a := range e.Args {
		a2, err := l.lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args[i] = a2
	}
	argStrs := make([]string, len(args))
	for i, a := range args {
		argStrs[i] = a.KtExprString()
	}
	return &ktree.RawKtExpr{
		Code: callee.KtExprString() + ".invoke(" + strings.Join(argStrs, ", ") + ")",
	}, nil
}

func (l *lowerer) lowerListFilterExpr(e *aotir.ListFilterExpr) (ktree.Expr, error) {
	list, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, err
	}
	fn, err := l.lowerExpr(e.Fn)
	if err != nil {
		return nil, err
	}
	// Pass fn directly as a function value; avoid trailing-lambda syntax so that
	// an inline lambda literal is not nested inside another lambda block.
	fnStr := fn.KtExprString()
	code := fmt.Sprintf("(%s.filter(%s).toMutableList())", list.KtExprString(), fnStr)
	return &ktree.RawKtExpr{Code: code}, nil
}

func (l *lowerer) lowerListMapExpr(e *aotir.ListMapExpr) (ktree.Expr, error) {
	list, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, err
	}
	fn, err := l.lowerExpr(e.Fn)
	if err != nil {
		return nil, err
	}
	// Pass fn directly as a function value.
	code := fmt.Sprintf("(%s.map(%s).toMutableList())", list.KtExprString(), fn.KtExprString())
	return &ktree.RawKtExpr{Code: code}, nil
}

func (l *lowerer) lowerListFoldlExpr(e *aotir.ListFoldlExpr) (ktree.Expr, error) {
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
	// Pass fn directly as a function value via fold with explicit operation reference.
	// fold(init, operation) where operation: (acc, element) -> acc
	code := fmt.Sprintf("(%s.fold(%s, %s))", list.KtExprString(), init.KtExprString(), fn.KtExprString())
	return &ktree.RawKtExpr{Code: code}, nil
}

func (l *lowerer) lowerListSortAscExpr(e *aotir.ListSortAscExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &ktree.RawKtExpr{Code: recv.KtExprString() + ".sorted().toMutableList()"}, nil
}

func (l *lowerer) lowerListSliceExpr(e *aotir.ListSliceExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	start, err := l.lowerExpr(e.Start)
	if err != nil {
		return nil, err
	}

	if lit, ok := e.End.(*aotir.IntLit); ok && lit.Value == 1<<62-1 {
		code := fmt.Sprintf("(%s.drop((%s).toInt()).toMutableList())", recv.KtExprString(), start.KtExprString())
		return &ktree.RawKtExpr{Code: code}, nil
	}

	end, err := l.lowerExpr(e.End)
	if err != nil {
		return nil, err
	}
	code := fmt.Sprintf("(%s.subList((%s).toInt(), (%s).toInt()).toMutableList())", recv.KtExprString(), start.KtExprString(), end.KtExprString())
	return &ktree.RawKtExpr{Code: code}, nil
}

// --- Math and string builtins ---

func (l *lowerer) lowerMathCallExpr(e *aotir.MathCallExpr) (ktree.Expr, error) {
	arg, err := l.lowerExpr(e.Arg)
	if err != nil {
		return nil, err
	}
	switch e.Func {
	case "abs_i64":
		return &ktree.CallExpr{Func: "mochiAbsInt", Args: []ktree.Expr{arg}}, nil
	case "abs_f64":
		return &ktree.CallExpr{Func: "mochiAbsFloat", Args: []ktree.Expr{arg}}, nil
	case "floor":
		return &ktree.CallExpr{Func: "mochiFloor", Args: []ktree.Expr{arg}}, nil
	case "ceil":
		return &ktree.CallExpr{Func: "mochiCeil", Args: []ktree.Expr{arg}}, nil
	default:
		return nil, fmt.Errorf("kotlin/lower: unknown MathCallExpr func %q", e.Func)
	}
}

func (l *lowerer) lowerStrSubstringExpr(e *aotir.StrSubstringExpr) (ktree.Expr, error) {
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
	return &ktree.CallExpr{Func: "mochiStrSubstring", Args: []ktree.Expr{recv, start, end}}, nil
}

func (l *lowerer) lowerStrReverseExpr(e *aotir.StrReverseExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &ktree.CallExpr{Func: "mochiStrReverse", Args: []ktree.Expr{recv}}, nil
}

func (l *lowerer) lowerStrConvertExpr(e *aotir.StrConvertExpr) (ktree.Expr, error) {
	operand, err := l.lowerExpr(e.Operand)
	if err != nil {
		return nil, err
	}
	return &ktree.CallExpr{Func: "mochiStr", Args: []ktree.Expr{operand}}, nil
}

func (l *lowerer) lowerStrUpperExpr(e *aotir.StrUpperExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &ktree.MethodCallExpr{Receiver: recv, Method: "uppercase", Args: nil}, nil
}

func (l *lowerer) lowerStrLowerExpr(e *aotir.StrLowerExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &ktree.MethodCallExpr{Receiver: recv, Method: "lowercase", Args: nil}, nil
}

func (l *lowerer) lowerStrSplitExpr(e *aotir.StrSplitExpr) (ktree.Expr, error) {
	str, err := l.lowerExpr(e.Str)
	if err != nil {
		return nil, err
	}
	sep, err := l.lowerExpr(e.Sep)
	if err != nil {
		return nil, err
	}
	return &ktree.CallExpr{Func: "mochiStrSplit", Args: []ktree.Expr{str, sep}}, nil
}

func (l *lowerer) lowerStrJoinExpr(e *aotir.StrJoinExpr) (ktree.Expr, error) {
	list, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, err
	}
	sep, err := l.lowerExpr(e.Sep)
	if err != nil {
		return nil, err
	}
	return &ktree.CallExpr{Func: "mochiStrJoin", Args: []ktree.Expr{list, sep}}, nil
}

func (l *lowerer) lowerListMinExpr(e *aotir.ListMinExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &ktree.RawKtExpr{Code: recv.KtExprString() + ".min()"}, nil
}

func (l *lowerer) lowerListMaxExpr(e *aotir.ListMaxExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &ktree.RawKtExpr{Code: recv.KtExprString() + ".max()"}, nil
}

func (l *lowerer) lowerListSumExpr(e *aotir.ListSumExpr) (ktree.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &ktree.RawKtExpr{Code: recv.KtExprString() + ".sum()"}, nil
}

func (l *lowerer) lowerListContainsExpr(e *aotir.ListContainsExpr) (ktree.Expr, error) {
	list, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(e.Value)
	if err != nil {
		return nil, err
	}
	return &ktree.MethodCallExpr{Receiver: list, Method: "contains", Args: []ktree.Expr{val}}, nil
}

func lowerBinOp(op aotir.BinOp) string {
	switch op {
	case aotir.BinAddI64, aotir.BinAddF64:
		return "+"
	case aotir.BinSubI64, aotir.BinSubF64:
		return "-"
	case aotir.BinMulI64, aotir.BinMulF64:
		return "*"
	case aotir.BinDivF64:
		return "/"
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

// formatDouble formats a float64 for Kotlin output, matching vm3 format.
func formatDouble(v float64) string {
	if math.IsNaN(v) {
		return "Double.NaN"
	}
	if math.IsInf(v, 1) {
		return "Double.POSITIVE_INFINITY"
	}
	if math.IsInf(v, -1) {
		return "Double.NEGATIVE_INFINITY"
	}
	s := strconv.FormatFloat(v, 'g', -1, 64)
	if !strings.Contains(s, ".") && !strings.Contains(s, "e") && !strings.Contains(s, "E") {
		s += ".0"
	}
	return s
}
