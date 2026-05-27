package lower

import (
	"fmt"
	"strings"
	"unicode"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// lowerer carries compilation context shared across expression and statement
// lowering, specifically the className needed to emit static calls to
// user-defined functions in the same class, a registry of record declarations
// so FieldAccess and RecordLit can resolve field types, and a registry of
// union declarations so VariantLit and MatchStmt can resolve union info.
type lowerer struct {
	className string
	records   map[string]*aotir.RecordDecl  // name -> decl; populated by Lower
	unions    map[string]*aotir.UnionDecl   // name -> decl; populated by Lower (Phase 5)
	agents    map[string]*aotir.AgentDecl   // name -> decl; populated by Lower (Phase 9)
	javaFuncs map[string]*aotir.JavaFuncDecl // mochiName -> decl; populated by Lower (Phase 12)
}

// Lower translates an aotir.Program into one CompilationUnit per record/union
// type plus one CompilationUnit for the main class. The first element of the
// returned slice is always the main class CU; subsequent elements are record
// type CUs (one per RecordDecl in prog.Records, in source order) followed by
// sum type CUs (one per UnionDecl in prog.Unions, in source order).
func Lower(prog *aotir.Program, className string) ([]*javasrc.CompilationUnit, error) {
	l := &lowerer{
		className: className,
		records:   make(map[string]*aotir.RecordDecl, len(prog.Records)),
		unions:    make(map[string]*aotir.UnionDecl, len(prog.Unions)),
		agents:    make(map[string]*aotir.AgentDecl, len(prog.Agents)),
		javaFuncs: make(map[string]*aotir.JavaFuncDecl, len(prog.JavaFuncs)),
	}
	for _, rd := range prog.Records {
		l.records[rd.Name] = rd
	}
	for _, ud := range prog.Unions {
		l.unions[ud.Name] = ud
	}
	for _, ad := range prog.Agents {
		l.agents[ad.Name] = ad
	}
	for _, jf := range prog.JavaFuncs {
		l.javaFuncs[jf.MochiName] = jf
	}

	mainFn := prog.Functions[prog.Main]

	body, err := l.lowerBlock(mainFn.Body)
	if err != nil {
		return nil, err
	}

	mainMethod := &javasrc.MethodDecl{
		Modifiers:  []string{"public", "static"},
		ReturnType: javasrc.TypeVoid,
		Name:       "main",
		Params:     []javasrc.Param{{Type: &javasrc.TypeRef{Name: "String", Array: true}, Name: "args"}},
		Body:       &body,
	}

	members := []javasrc.Member{mainMethod}

	// Emit user-defined functions as additional static methods.
	for i, fn := range prog.Functions {
		if i == prog.Main {
			continue
		}
		method, err := l.lowerFunction(fn)
		if err != nil {
			return nil, err
		}
		members = append(members, method)
	}

	classDecl := &javasrc.ClassDecl{
		Modifiers: []string{"public", "final"},
		Name:      className,
		Members:   members,
	}

	mainCU := &javasrc.CompilationUnit{
		Package: "dev.mochi.user",
		Imports: []string{"java.util.Objects"},
		Types:   []javasrc.TypeDecl{classDecl},
	}

	cus := []*javasrc.CompilationUnit{mainCU}

	// Emit each record type as a separate CompilationUnit (separate .java file).
	for _, rd := range prog.Records {
		rcu, err := l.lowerRecordDecl(rd)
		if err != nil {
			return nil, fmt.Errorf("record %q: %w", rd.Name, err)
		}
		cus = append(cus, rcu)
	}

	// Emit each union/sum type as a separate CompilationUnit (separate .java file).
	for _, ud := range prog.Unions {
		ucu, err := l.lowerSumTypeDecl(ud)
		if err != nil {
			return nil, fmt.Errorf("union %q: %w", ud.Name, err)
		}
		cus = append(cus, ucu)
	}

	// Emit each agent as a separate CompilationUnit (MochiAgent_<Name>.java).
	for _, ad := range prog.Agents {
		acu, err := l.lowerAgentDecl(ad)
		if err != nil {
			return nil, fmt.Errorf("agent %q: %w", ad.Name, err)
		}
		cus = append(cus, acu)
	}

	return cus, nil
}

// lowerRecordDecl lowers an aotir.RecordDecl to a javasrc.CompilationUnit
// containing a single public record type.
func (l *lowerer) lowerRecordDecl(rd *aotir.RecordDecl) (*javasrc.CompilationUnit, error) {
	components := make([]javasrc.RecordComponent, len(rd.Fields))
	for i, f := range rd.Fields {
		ft, err := lowerFieldType(f)
		if err != nil {
			return nil, fmt.Errorf("field %q: %w", f.Name, err)
		}
		components[i] = javasrc.RecordComponent{Type: ft, Name: f.Name}
	}

	recDecl := &javasrc.RecordDecl{
		Modifiers:  []string{"public"},
		Name:       rd.Name,
		Components: components,
	}

	return &javasrc.CompilationUnit{
		Package: "dev.mochi.user",
		Types:   []javasrc.TypeDecl{recDecl},
	}, nil
}

// lowerFieldType maps a RecordField to a Java TypeRef for a record component.
func lowerFieldType(f aotir.RecordField) (javasrc.TypeRef, error) {
	switch f.Type {
	case aotir.TypeInt:
		return javasrc.TypeLong, nil
	case aotir.TypeFloat:
		return javasrc.TypeDouble, nil
	case aotir.TypeBool:
		return javasrc.TypeBoolean, nil
	case aotir.TypeString:
		return javasrc.TypeString, nil
	case aotir.TypeRecord:
		if f.RecordName == "" {
			return javasrc.TypeRef{}, fmt.Errorf("TypeRecord field %q has empty RecordName", f.Name)
		}
		return javasrc.TypeRef{Name: f.RecordName}, nil
	default:
		return javasrc.TypeRef{}, fmt.Errorf("unsupported field type %v", f.Type)
	}
}

// lowerFunction translates a user-defined aotir.Function to a static MethodDecl.
// For lifted closure functions (IsLifted==true), captured variables are prepended
// as extra parameters (before the declared params) so that the Java lambda
// can pass them as regular arguments rather than through a C env struct pointer.
func (l *lowerer) lowerFunction(fn *aotir.Function) (*javasrc.MethodDecl, error) {
	retType, err := l.lowerReturnType(fn)
	if err != nil {
		return nil, err
	}

	// For lifted functions, prepend captured-variable params.
	// The C backend uses __e->field to access them; for JVM we make them
	// explicit params and rewrite the body below.
	var captureParams []javasrc.Param
	if fn.IsLifted && len(fn.Captures) > 0 {
		captureParams = make([]javasrc.Param, len(fn.Captures))
		for i, cap := range fn.Captures {
			pt, err := lowerType(cap.VarType)
			if err != nil {
				return nil, fmt.Errorf("capture %q: %w", cap.FieldName, err)
			}
			captureParams[i] = javasrc.Param{Type: &pt, Name: cap.FieldName}
		}
	}

	declParams := make([]javasrc.Param, len(fn.Params))
	for i, p := range fn.Params {
		var pt javasrc.TypeRef
		switch p.Type {
		case aotir.TypeList:
			pt = lowerListType(p.ElemType)
		case aotir.TypeMap:
			pt = lowerMapType(p.KeyType, p.ValueType)
		case aotir.TypeSet:
			pt = lowerSetType(p.ElemType)
		case aotir.TypeRecord:
			pt = javasrc.TypeRef{Name: p.RecordName}
		case aotir.TypeUnion:
			pt = javasrc.TypeRef{Name: p.UnionName}
		case aotir.TypeFun:
			pt = funcTypeRef(p.FunSig)
		default:
			pt, err = lowerType(p.Type)
			if err != nil {
				return nil, err
			}
		}
		declParams[i] = javasrc.Param{Type: &pt, Name: p.Name}
	}

	// Combine capture params + declared params.
	allParams := append(captureParams, declParams...)

	// For lifted functions, rewrite the body so __e->field VarRefs become
	// plain field-name references (the C env-struct access pattern does not
	// apply on JVM; the capture vars are now plain parameters).
	bodyToLower := fn.Body
	if fn.IsLifted && len(fn.Captures) > 0 {
		bodyToLower = rewriteEnvRefs(fn.Body, fn.Captures)
	}

	body, err := l.lowerBlock(bodyToLower)
	if err != nil {
		return nil, err
	}

	return &javasrc.MethodDecl{
		Modifiers:  []string{"public", "static"},
		ReturnType: retType,
		Name:       fn.Name,
		Params:     allParams,
		Body:       &body,
	}, nil
}

// rewriteEnvRefs returns a copy of b where every VarRef whose Name starts with
// "__e->" is replaced with a VarRef using just the field name (the suffix after "->").
// This converts C env-struct access to plain-variable access for JVM emission.
func rewriteEnvRefs(b *aotir.Block, captures []aotir.FunCapture) *aotir.Block {
	if b == nil {
		return nil
	}
	// Build a map from emitName "__e->field" -> fieldName.
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
		return &aotir.AssignStmt{Name: s.Name, Value: rewriteExprEnvRefs(s.Value, renames)}
	case *aotir.CallStmt:
		args := make([]aotir.Expr, len(s.Args))
		for i, a := range s.Args {
			args[i] = rewriteExprEnvRefs(a, renames)
		}
		return &aotir.CallStmt{Func: s.Func, Args: args}
	case *aotir.IfStmt:
		cp := *s
		cp.Cond = rewriteExprEnvRefs(s.Cond, renames)
		cp.Then = rewriteEnvRefsBlock(s.Then, renames)
		if s.Else != nil {
			cp.Else = rewriteEnvRefsBlock(s.Else, renames)
		}
		return &cp
	case *aotir.WhileStmt:
		return &aotir.WhileStmt{
			Cond: rewriteExprEnvRefs(s.Cond, renames),
			Body: rewriteEnvRefsBlock(s.Body, renames),
		}
	case *aotir.ForEachStmt:
		cp := *s
		cp.List = rewriteExprEnvRefs(s.List, renames)
		cp.Body = rewriteEnvRefsBlock(s.Body, renames)
		return &cp
	default:
		return s
	}
}

func rewriteEnvRefsBlock(b *aotir.Block, renames map[string]string) *aotir.Block {
	if b == nil {
		return nil
	}
	stmts := make([]aotir.Stmt, len(b.Statements))
	for i, s := range b.Statements {
		stmts[i] = rewriteStmtEnvRefs(s, renames)
	}
	return &aotir.Block{Statements: stmts}
}

func rewriteExprEnvRefs(e aotir.Expr, renames map[string]string) aotir.Expr {
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
	case *aotir.BinaryExpr:
		return &aotir.BinaryExpr{
			Op:     e.Op,
			Left:   rewriteExprEnvRefs(e.Left, renames),
			Right:  rewriteExprEnvRefs(e.Right, renames),
			Result: e.Result,
		}
	case *aotir.UnaryExpr:
		return &aotir.UnaryExpr{
			Op:      e.Op,
			Operand: rewriteExprEnvRefs(e.Operand, renames),
			Result:  e.Result,
		}
	case *aotir.CallExpr:
		args := make([]aotir.Expr, len(e.Args))
		for i, a := range e.Args {
			args[i] = rewriteExprEnvRefs(a, renames)
		}
		cp := *e
		cp.Args = args
		return &cp
	case *aotir.FunCallExpr:
		callee := rewriteExprEnvRefs(e.Callee, renames)
		args := make([]aotir.Expr, len(e.Args))
		for i, a := range e.Args {
			args[i] = rewriteExprEnvRefs(a, renames)
		}
		return &aotir.FunCallExpr{Callee: callee, Args: args, Result: e.Result}
	default:
		return e
	}
}

// lowerReturnType maps a Function's return type (with record/union support) to a TypeRef.
func (l *lowerer) lowerReturnType(fn *aotir.Function) (javasrc.TypeRef, error) {
	switch fn.ReturnType {
	case aotir.TypeRecord:
		return javasrc.TypeRef{Name: fn.ReturnRecordName}, nil
	case aotir.TypeUnion:
		return javasrc.TypeRef{Name: fn.ReturnUnionName}, nil
	case aotir.TypeList:
		return lowerListType(fn.ReturnElemType), nil
	case aotir.TypeMap:
		return lowerMapType(fn.ReturnKeyType, fn.ReturnValueType), nil
	case aotir.TypeSet:
		return lowerSetType(fn.ReturnElemType), nil
	case aotir.TypeFun:
		return funcTypeRef(fn.ReturnFunSig), nil
	default:
		return lowerType(fn.ReturnType)
	}
}

// ClassName converts a Mochi source filename to a Java class name.
// "hello.mochi" -> "HelloMochi"
// "my_program.mochi" -> "MyProgramMochi"
func ClassName(src string) string {
	// Strip directory prefix
	for i := len(src) - 1; i >= 0; i-- {
		if src[i] == '/' || src[i] == '\\' {
			src = src[i+1:]
			break
		}
	}
	// Strip .mochi extension
	src = strings.TrimSuffix(src, ".mochi")
	// Convert snake_case to PascalCase
	parts := strings.Split(src, "_")
	var sb strings.Builder
	for _, p := range parts {
		if len(p) == 0 {
			continue
		}
		runes := []rune(p)
		runes[0] = unicode.ToUpper(runes[0])
		sb.WriteString(string(runes))
	}
	sb.WriteString("Mochi")
	return sb.String()
}
