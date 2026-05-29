// Package lower translates an aotir.Program into a rtree.File.
// Entry point: Lower(prog, colours, crateName) (*rtree.File, error).
package lower

import (
	"fmt"
	"path/filepath"
	"strings"
	"unicode"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/rust/colour"
	"mochi/transpiler3/rust/rtree"
)

// CrateName converts a Mochi source filename to a snake_case crate name.
// "hello.mochi"       -> "hello"
// "hello_world.mochi" -> "hello_world"
// "hello-world.mochi" -> "hello_world"
func CrateName(src string) string {
	src = filepath.Base(src)
	src = strings.TrimSuffix(src, ".mochi")
	var sb strings.Builder
	for i, r := range src {
		switch {
		case r == '-':
			sb.WriteByte('_')
		case unicode.IsUpper(r):
			if i > 0 {
				sb.WriteByte('_')
			}
			sb.WriteRune(unicode.ToLower(r))
		default:
			sb.WriteRune(r)
		}
	}
	out := sb.String()
	if out == "" {
		out = "mochi_out"
	}
	return out
}

type lowerer struct {
	colours colour.ColourMap
	prog    *aotir.Program
	// matchBindings maps a variant field name to the pattern-variable
	// name bound in the current match arm; used so VariantFieldAccess
	// resolves to the in-scope binding identifier.
	matchBindings map[string]string
	// unions indexes union decls by name so the lowerer can stamp
	// canonical union names onto EnumVariantLits and patterns.
	unions map[string]*aotir.UnionDecl
	// agents indexes agent decls by name so intent calls can resolve
	// the Rust type token for `&mut` argument annotations.
	agents map[string]*aotir.AgentDecl
}

// Lower translates an aotir.Program into a rtree.File.
func Lower(prog *aotir.Program, colours colour.ColourMap, crateName string) (*rtree.File, error) {
	l := &lowerer{colours: colours, prog: prog}
	l.unions = make(map[string]*aotir.UnionDecl, len(prog.Unions))
	for _, ud := range prog.Unions {
		l.unions[ud.Name] = ud
	}
	l.agents = make(map[string]*aotir.AgentDecl, len(prog.Agents))
	for _, ad := range prog.Agents {
		l.agents[ad.Name] = ad
	}

	mainFn := prog.Functions[prog.Main]
	mainBody, err := l.lowerBlock(mainFn.Body)
	if err != nil {
		return nil, err
	}

	mainItem := &rtree.FnDecl{
		Name: "main",
		Body: mainBody,
	}

	file := &rtree.File{
		Name: crateName,
		Uses: []*rtree.Use{},
	}

	// Records first so types are in scope.
	for _, rd := range prog.Records {
		file.Items = append(file.Items, lowerRecordDecl(rd))
	}
	// Unions next; emit declaration-order to keep diffable output.
	for _, ud := range prog.Unions {
		file.Items = append(file.Items, lowerUnionDecl(ud))
	}
	// Agents: struct + free fns for each intent. The struct comes first
	// so intent bodies (free fns) can name it; the intent fns are then
	// callable from main and from other intent bodies.
	for _, ad := range prog.Agents {
		structItem, intentFns, err := l.lowerAgentDecl(ad)
		if err != nil {
			return nil, err
		}
		file.Items = append(file.Items, structItem)
		for _, fn := range intentFns {
			file.Items = append(file.Items, fn)
		}
	}

	file.Items = append(file.Items, mainItem)

	for i, fn := range prog.Functions {
		if i == prog.Main {
			continue
		}
		fd, err := l.lowerFunction(fn)
		if err != nil {
			return nil, err
		}
		file.Items = append(file.Items, fd)
	}

	return file, nil
}

// lowerRecordDecl translates an aotir.RecordDecl into a Rust StructDecl.
// Phase 4 derives Debug, Clone, PartialEq for every record. Eq is added
// when no float fields are present.
func lowerRecordDecl(rd *aotir.RecordDecl) rtree.Item {
	derives := []string{"Debug", "Clone", "PartialEq"}
	hasFloat := false
	fields := make([]rtree.StructField, 0, len(rd.Fields))
	for _, f := range rd.Fields {
		if f.Type == aotir.TypeFloat {
			hasFloat = true
		}
		fields = append(fields, rtree.StructField{
			Visibility: "",
			Name:       f.Name,
			TypeName:   rustFieldTypeName(f.Type, f.RecordName),
		})
	}
	if !hasFloat {
		derives = append(derives, "Eq", "Hash")
	}
	return &rtree.StructDecl{
		Derives: derives,
		Name:    rd.Name,
		Fields:  fields,
	}
}

func rustFieldTypeName(t aotir.Type, recordName string) string {
	if t == aotir.TypeRecord && recordName != "" {
		return recordName
	}
	return rustTypeName(t)
}

// rustNamedTypeName resolves a Rust type name that may carry a nominal
// identity. recordName is used for TypeRecord, unionName for TypeUnion.
// Other types fall back to rustTypeName. For TypeAgent the agent name
// is encoded in the same recordName slot when the caller knows it; the
// fallback path handles VarRef/LetStmt where AgentName lives separately
// (see lowerLetStmt).
func rustNamedTypeName(t aotir.Type, recordName, unionName string) string {
	switch t {
	case aotir.TypeRecord:
		if recordName != "" {
			return recordName
		}
	case aotir.TypeUnion:
		if unionName != "" {
			return unionName
		}
	case aotir.TypeAgent:
		if recordName != "" {
			return recordName
		}
	}
	return rustTypeName(t)
}

// lowerUnionDecl translates an aotir.UnionDecl into a Rust enum
// declaration. Derives mirror lowerRecordDecl: Debug, Clone, PartialEq
// always; Eq+Hash are added only when no variant field is a float so
// the derive holds.
func lowerUnionDecl(ud *aotir.UnionDecl) rtree.Item {
	derives := []string{"Debug", "Clone", "PartialEq"}
	hasFloat := false
	variants := make([]rtree.EnumVariant, 0, len(ud.Variants))
	for _, v := range ud.Variants {
		fields := make([]rtree.StructField, 0, len(v.Fields))
		for _, f := range v.Fields {
			if f.FieldType == aotir.TypeFloat {
				hasFloat = true
			}
			fields = append(fields, rtree.StructField{
				Name:     f.Name,
				TypeName: rustNamedTypeName(f.FieldType, f.RecordName, f.UnionName),
			})
		}
		variants = append(variants, rtree.EnumVariant{Name: v.Name, Fields: fields})
	}
	if !hasFloat {
		derives = append(derives, "Eq", "Hash")
	}
	return &rtree.EnumDecl{
		Derives:  derives,
		Name:     ud.Name,
		Variants: variants,
	}
}

// lowerAgentDecl translates an aotir.AgentDecl into a Rust StructDecl
// plus a free function per intent. The struct mirrors the agent's
// fields (Debug + Clone derives; PartialEq is omitted because agents
// are state holders and not compared for equality in Phase 9.3). Each
// intent becomes a free fn named `mochi_agent_NAME__INTENT` with first
// parameter `__self: &mut TypeName` so the intent body can mutate the
// agent's state through a Rust mutable reference. Field accesses and
// assignments are rewritten from `__self->field` to `__self.field`
// in lowerExpr / lowerAssignStmt.
func (l *lowerer) lowerAgentDecl(ad *aotir.AgentDecl) (rtree.Item, []*rtree.FnDecl, error) {
	derives := []string{"Debug", "Clone"}
	fields := make([]rtree.StructField, 0, len(ad.Fields))
	for _, f := range ad.Fields {
		fields = append(fields, rtree.StructField{
			Name:     f.Name,
			TypeName: rustFieldTypeName(f.Type, f.RecordName),
		})
	}
	structItem := &rtree.StructDecl{
		Derives: derives,
		Name:    ad.Name,
		Fields:  fields,
	}

	intentFns := make([]*rtree.FnDecl, 0, len(ad.Intents))
	for _, intent := range ad.Intents {
		fnName := "mochi_agent_" + ad.Name + "__" + intent.Name
		params := make([]rtree.FnParam, 0, len(intent.Params)+1)
		params = append(params, rtree.FnParam{
			Name:     "__self",
			TypeName: "&mut " + ad.Name,
		})
		for _, p := range intent.Params {
			params = append(params, rtree.FnParam{
				Name:     p.Name,
				TypeName: rustTypeName(p.Type),
			})
		}
		retTy := ""
		if intent.ReturnType != aotir.TypeUnit {
			retTy = rustTypeName(intent.ReturnType)
		}
		body, err := l.lowerBlock(intent.Body)
		if err != nil {
			return nil, nil, fmt.Errorf("agent %q intent %q: %w", ad.Name, intent.Name, err)
		}
		intentFns = append(intentFns, &rtree.FnDecl{
			Name:       fnName,
			Params:     params,
			ReturnType: retTy,
			Body:       body,
		})
	}
	return structItem, intentFns, nil
}

func (l *lowerer) lowerFunction(fn *aotir.Function) (*rtree.FnDecl, error) {
	body, err := l.lowerBlock(fn.Body)
	if err != nil {
		return nil, err
	}
	params := make([]rtree.FnParam, 0, len(fn.Captures)+len(fn.Params))
	// Lifted closures: captures are passed as real Rust parameters
	// prepended to the formal parameter list. The C ABI uses a single
	// env pointer; the Rust ABI is direct positional arguments which
	// matches the Box<dyn Fn> call sites the lowerer constructs below.
	for _, c := range fn.Captures {
		params = append(params, rtree.FnParam{
			Name:     c.FieldName,
			TypeName: rustNamedTypeName(c.VarType, "", ""),
		})
	}
	for _, p := range fn.Params {
		params = append(params, rtree.FnParam{
			Name:     p.Name,
			TypeName: rustParamTypeFromParam(p),
		})
	}
	retTy := ""
	if fn.ReturnType != aotir.TypeUnit {
		retTy = rustReturnTypeFromFunction(fn)
	}
	return &rtree.FnDecl{
		Name:       fn.Name,
		Params:     params,
		ReturnType: retTy,
		Body:       body,
		IsAsync:    l.colours[fn.Name] == colour.Red,
	}, nil
}

// rustFnType renders the Rust trait-object form of a function type:
// `Box<dyn Fn(P0, P1, ...) -> R>` (or with no `->` clause when R is
// unit). The caller is responsible for ensuring this is the right form
// for the context (let binding, fn param, fn return).
func rustFnType(sig *aotir.FunSig) string {
	if sig == nil {
		return ""
	}
	parts := make([]string, len(sig.ParamTypes))
	for i, t := range sig.ParamTypes {
		parts[i] = rustTypeName(t)
	}
	ret := ""
	if sig.ReturnType != aotir.TypeUnit {
		ret = " -> " + rustTypeName(sig.ReturnType)
	}
	return fmt.Sprintf("Box<dyn Fn(%s)%s>", strings.Join(parts, ", "), ret)
}

// rustParamType extends rustNamedTypeName by handling TypeFun via the
// caller-supplied FunSig. When sig is nil but t is TypeFun the result
// falls back to the unit-type token, which is a lower-layer bug since
// every TypeFun should carry a signature.
func rustParamType(t aotir.Type, recordName, unionName string, sig *aotir.FunSig) string {
	if t == aotir.TypeFun && sig != nil {
		return rustFnType(sig)
	}
	return rustNamedTypeName(t, recordName, unionName)
}

func (l *lowerer) lowerBlock(b *aotir.Block) ([]rtree.Stmt, error) {
	if b == nil {
		return nil, nil
	}
	out := make([]rtree.Stmt, 0, len(b.Statements))
	i := 0
	for i < len(b.Statements) {
		s := b.Statements[i]
		// ClosureEnvStmt is a C-specific env-alloc step; the Rust
		// translation uses move-closures so no env struct exists.
		if _, ok := s.(*aotir.ClosureEnvStmt); ok {
			i++
			continue
		}
		// QueryScopeStmt is a C arena wrapper around the desugared
		// query body (ForEach + optional sort/slice assigns). Rust
		// does not need arenas, so we inline the body's statements
		// into the current block.
		if qs, ok := s.(*aotir.QueryScopeStmt); ok {
			bodyStmts, err := l.lowerBlock(qs.Body)
			if err != nil {
				return nil, err
			}
			out = append(out, bodyStmts...)
			i++
			continue
		}
		// RawCStmt is the C-specific setup for a DatalogQueryExpr.
		// The Rust path evaluates Datalog at compile time and emits
		// a static Vec<String>, so the setup is dropped.
		if _, ok := s.(*aotir.RawCStmt); ok {
			i++
			continue
		}
		// Peep: `let mut X` followed by `MatchStmt{ResultVar: X}` collapses
		// to `let mut X = match ... { ... };` which is idiomatic Rust.
		if ls, ok := s.(*aotir.LetStmt); ok && ls.Init == nil && i+1 < len(b.Statements) {
			if ms, ok := b.Statements[i+1].(*aotir.MatchStmt); ok && ms.ResultVar == ls.Name {
				matchExpr, err := l.lowerMatchExpr(ms)
				if err != nil {
					return nil, err
				}
				out = append(out, &rtree.LetStmt{
					Mutable: ls.Mutable,
					Name:    ls.Name,
					Value:   matchExpr,
				})
				i += 2
				continue
			}
		}
		stmt, err := l.lowerStmt(s)
		if err != nil {
			return nil, err
		}
		out = append(out, stmt)
		i++
	}
	return out, nil
}

func (l *lowerer) lowerStmt(s aotir.Stmt) (rtree.Stmt, error) {
	switch n := s.(type) {
	case *aotir.CallStmt:
		return l.lowerCallStmt(n)
	case *aotir.LetStmt:
		return l.lowerLetStmt(n)
	case *aotir.AssignStmt:
		return l.lowerAssignStmt(n)
	case *aotir.IfStmt:
		return l.lowerIfStmt(n)
	case *aotir.WhileStmt:
		return l.lowerWhileStmt(n)
	case *aotir.ForRangeStmt:
		return l.lowerForRangeStmt(n)
	case *aotir.BreakStmt:
		return &rtree.BreakStmt{}, nil
	case *aotir.ContinueStmt:
		return &rtree.ContinueStmt{}, nil
	case *aotir.ReturnStmt:
		return l.lowerReturnStmt(n)
	case *aotir.ForEachStmt:
		return l.lowerForEachStmt(n)
	case *aotir.MapPutStmt:
		return l.lowerMapPutStmt(n)
	case *aotir.ListSetStmt:
		return l.lowerListSetStmt(n)
	case *aotir.MatchStmt:
		return l.lowerMatchStmt(n)
	case *aotir.AgentIntentCallStmt:
		expr, err := l.lowerAgentIntentCall(n.AgentName, n.IntentName, n.Receiver, n.Args)
		if err != nil {
			return nil, err
		}
		return &rtree.ExprStmt{Expr: expr}, nil
	}
	return nil, fmt.Errorf("rust lower: unsupported stmt %T", s)
}

// lowerMatchStmt handles a match in statement position. When ResultVar
// is set the C lowerer already emitted a preceding `let mut X` with
// nil Init; lowerBlock combines the pair into a `let mut X = match`
// statement. Falling through here means either the result is unused
// (each arm is a unit-returning side effect) or the surrounding code
// has already declared the variable. In the second case we emit an
// AssignStmt wrapping the MatchExpr; in the first case we wrap as an
// ExprStmt that evaluates the match for side effects.
func (l *lowerer) lowerMatchStmt(s *aotir.MatchStmt) (rtree.Stmt, error) {
	matchExpr, err := l.lowerMatchExpr(s)
	if err != nil {
		return nil, err
	}
	if s.ResultVar != "" {
		return &rtree.AssignStmt{Target: &rtree.Ident{Name: s.ResultVar}, Value: matchExpr}, nil
	}
	return &rtree.ExprStmt{Expr: matchExpr}, nil
}

// lowerMatchExpr lowers an aotir.MatchStmt into a rtree.MatchExpr.
// For ResultVar match each arm body ends with an AssignStmt to the
// result variable; the lowerer strips that and lifts its RHS to the
// arm tail so the match itself produces the value. For non-result
// matches the body stays as side-effect statements.
func (l *lowerer) lowerMatchExpr(s *aotir.MatchStmt) (*rtree.MatchExpr, error) {
	target, err := l.lowerExpr(s.Target)
	if err != nil {
		return nil, err
	}
	arms := make([]rtree.MatchArm, 0, len(s.Arms)+1)
	for i := range s.Arms {
		arm, err := l.lowerMatchArm(s.UnionName, &s.Arms[i], s.ResultVar)
		if err != nil {
			return nil, fmt.Errorf("match arm %q: %w", s.Arms[i].VariantName, err)
		}
		arms = append(arms, arm)
	}
	if s.Default != nil {
		arm, err := l.lowerMatchArm(s.UnionName, s.Default, s.ResultVar)
		if err != nil {
			return nil, fmt.Errorf("match default arm: %w", err)
		}
		arm.Pat = "_"
		arms = append(arms, arm)
	}
	return &rtree.MatchExpr{Target: target, Arms: arms}, nil
}

func (l *lowerer) lowerMatchArm(unionName string, arm *aotir.MatchArm, resultVar string) (rtree.MatchArm, error) {
	pat := matchArmPattern(unionName, arm)

	saved := l.matchBindings
	l.matchBindings = make(map[string]string, len(arm.Bindings))
	for _, b := range arm.Bindings {
		l.matchBindings[b.FieldName] = b.VarName
	}
	body, err := l.lowerBlock(arm.Body)
	l.matchBindings = saved
	if err != nil {
		return rtree.MatchArm{}, err
	}

	var tail rtree.Expr
	if resultVar != "" && len(body) > 0 {
		if as, ok := body[len(body)-1].(*rtree.AssignStmt); ok {
			if id, ok := as.Target.(*rtree.Ident); ok && id.Name == resultVar {
				tail = as.Value
				body = body[:len(body)-1]
			}
		}
	}
	return rtree.MatchArm{Pat: pat, Body: body, Tail: tail}, nil
}

// matchArmPattern renders the Rust pattern for one match arm.
// Unit variants render as `Enum::Variant`; field-bearing variants
// render with named-field destructuring. Pattern variable names use
// the field-name shorthand when binding name matches the field name.
func matchArmPattern(unionName string, arm *aotir.MatchArm) string {
	if arm.VariantName == "" {
		return "_"
	}
	if len(arm.Bindings) == 0 {
		return unionName + "::" + arm.VariantName
	}
	parts := make([]string, len(arm.Bindings))
	for i, b := range arm.Bindings {
		if b.VarName == b.FieldName {
			parts[i] = b.FieldName
		} else {
			parts[i] = b.FieldName + ": " + b.VarName
		}
	}
	return unionName + "::" + arm.VariantName + " { " + strings.Join(parts, ", ") + " }"
}

func (l *lowerer) lowerForEachStmt(s *aotir.ForEachStmt) (rtree.Stmt, error) {
	iter, err := l.lowerExpr(s.List)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	// Iterate by value via `.clone()` to keep semantics simple for primitives.
	// For Strings, `.iter().cloned()` works; for primitives, `.iter().copied()`.
	wrapped := &rtree.RawExpr{Code: iter.RustExpr() + ".iter().cloned()"}
	return &rtree.ForEachStmt{Var: s.Var, Iter: wrapped, Body: body}, nil
}

func (l *lowerer) lowerMapPutStmt(s *aotir.MapPutStmt) (rtree.Stmt, error) {
	k, err := l.lowerExpr(s.Key)
	if err != nil {
		return nil, err
	}
	v, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	return &rtree.ExprStmt{Expr: &rtree.MethodCall{
		Receiver: &rtree.Ident{Name: s.Name},
		Method:   "insert",
		Args:     []rtree.Expr{k, v},
	}}, nil
}

func (l *lowerer) lowerListSetStmt(s *aotir.ListSetStmt) (rtree.Stmt, error) {
	idx, err := l.lowerExpr(s.Index)
	if err != nil {
		return nil, err
	}
	v, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	return &rtree.AssignStmt{
		Target: &rtree.IndexExpr{
			Receiver: &rtree.Ident{Name: s.Name},
			Index:    &rtree.CastExpr{Expr: idx, TypeName: "usize"},
		},
		Value: v,
	}, nil
}

func (l *lowerer) lowerLetStmt(s *aotir.LetStmt) (rtree.Stmt, error) {
	// Agent-typed bindings must be mutable because intent calls take
	// `&mut self` references. Mochi's `let c = Agent{...}` is treated as
	// mutable in Rust since intents inherently mutate agent state, and
	// Rust would otherwise reject the `&mut c` borrow at the call site.
	mutable := s.Mutable || s.VarType == aotir.TypeAgent
	if s.Init == nil {
		// Bare declaration. Rust requires a type annotation here since
		// there is no initializer to infer from.
		return &rtree.LetStmt{
			Mutable:  mutable,
			Name:     s.Name,
			TypeName: rustParamType(s.VarType, s.RecordName, s.UnionName, s.FunSig),
		}, nil
	}
	v, err := l.lowerExpr(s.Init)
	if err != nil {
		return nil, err
	}
	// Closure-typed bindings need an explicit `Box<dyn Fn(...)->...>`
	// annotation so the Box<closure>/Box<fn item> on the RHS coerces
	// into the trait-object shape used at call sites and downstream
	// function parameters. Empty collection literals (vec![], etc.)
	// likewise need an explicit element type for inference.
	typeName := ""
	if s.VarType == aotir.TypeFun && s.FunSig != nil {
		typeName = rustFnType(s.FunSig)
	}
	if ll, ok := s.Init.(*aotir.ListLit); ok && len(ll.Elems) == 0 {
		typeName = rustCollectionTypeName(s.VarType, s.RecordName, s.UnionName,
			s.ElemType, "", aotir.TypeInvalid,
			aotir.TypeInvalid, aotir.TypeInvalid,
			aotir.TypeInvalid, aotir.TypeInvalid, aotir.TypeInvalid,
			nil)
	}
	return &rtree.LetStmt{
		Mutable:  mutable,
		Name:     s.Name,
		TypeName: typeName,
		Value:    v,
	}, nil
}

func (l *lowerer) lowerAssignStmt(s *aotir.AssignStmt) (rtree.Stmt, error) {
	v, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	if rest, ok := strings.CutPrefix(s.Name, "__self->"); ok {
		return &rtree.AssignStmt{
			Target: &rtree.FieldAccess{Receiver: &rtree.Ident{Name: "__self"}, Field: rest},
			Value:  v,
		}, nil
	}
	return &rtree.AssignStmt{Target: &rtree.Ident{Name: s.Name}, Value: v}, nil
}

func (l *lowerer) lowerIfStmt(s *aotir.IfStmt) (rtree.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	then, err := l.lowerBlock(s.Then)
	if err != nil {
		return nil, err
	}
	var els []rtree.Stmt
	if s.Else != nil {
		els, err = l.lowerBlock(s.Else)
		if err != nil {
			return nil, err
		}
	}
	return &rtree.IfStmt{Cond: cond, Then: then, Else: els}, nil
}

func (l *lowerer) lowerWhileStmt(s *aotir.WhileStmt) (rtree.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	return &rtree.WhileStmt{Cond: cond, Body: body}, nil
}

func (l *lowerer) lowerForRangeStmt(s *aotir.ForRangeStmt) (rtree.Stmt, error) {
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
	return &rtree.ForRangeStmt{Var: s.Var, Start: start, End: end, Body: body}, nil
}

func (l *lowerer) lowerReturnStmt(s *aotir.ReturnStmt) (rtree.Stmt, error) {
	if s.Value == nil {
		return &rtree.ReturnStmt{}, nil
	}
	v, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	return &rtree.ReturnStmt{Value: v}, nil
}

func (l *lowerer) lowerCallStmt(c *aotir.CallStmt) (rtree.Stmt, error) {
	if mapped, ok := builtinCall(c.Func); ok {
		args := make([]rtree.Expr, 0, len(c.Args))
		for _, a := range c.Args {
			ea, err := l.lowerExpr(a)
			if err != nil {
				return nil, err
			}
			args = append(args, ea)
		}
		return &rtree.ExprStmt{Expr: &rtree.CallExpr{Func: mapped, Args: args}}, nil
	}
	// User-defined function call as a statement.
	args := make([]rtree.Expr, 0, len(c.Args))
	for _, a := range c.Args {
		ea, err := l.lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args = append(args, ea)
	}
	return &rtree.ExprStmt{Expr: &rtree.CallExpr{Func: c.Func, Args: args}}, nil
}

// builtinCall returns the Rust runtime path for a mangled C-lowerer builtin
// name. The second return is false for non-builtins.
func builtinCall(name string) (string, bool) {
	switch name {
	case "mochi_print_str":
		return "mochi_runtime::io::print_str", true
	case "mochi_print_i64":
		return "mochi_runtime::io::print_i64", true
	case "mochi_print_f64":
		return "mochi_runtime::io::print_f64", true
	case "mochi_print_bool":
		return "mochi_runtime::io::print_bool", true
	}
	return "", false
}

func (l *lowerer) lowerExpr(e aotir.Expr) (rtree.Expr, error) {
	switch n := e.(type) {
	case *aotir.StringLit:
		return &rtree.StringLit{Value: n.Value}, nil
	case *aotir.IntLit:
		return &rtree.IntLit{Value: n.Value}, nil
	case *aotir.FloatLit:
		return &rtree.FloatLit{Value: n.Value}, nil
	case *aotir.BoolLit:
		return &rtree.BoolLit{Value: n.Value}, nil
	case *aotir.VarRef:
		// Lifted closure bodies see captured names as `__e->FieldName`
		// in aotir, since the C lowerer rewrites them through the env
		// pointer. The Rust ABI prepends captures as plain parameters
		// (see lowerFunction), so the env prefix is stripped here.
		// Agent intent bodies likewise see field accesses as
		// `__self->FieldName`, which we rewrite to a Rust FieldAccess
		// on the `__self: &mut TypeName` parameter.
		if rest, ok := strings.CutPrefix(n.Name, "__self->"); ok {
			fa := &rtree.FieldAccess{Receiver: &rtree.Ident{Name: "__self"}, Field: rest}
			if isOwnedType(n.VarType) {
				return &rtree.CloneExpr{Expr: fa}, nil
			}
			return fa, nil
		}
		name := strings.TrimPrefix(n.Name, "__e->")
		ident := &rtree.Ident{Name: name}
		if isOwnedType(n.VarType) {
			return &rtree.CloneExpr{Expr: ident}, nil
		}
		return ident, nil
	case *aotir.BinaryExpr:
		return l.lowerBinaryExpr(n)
	case *aotir.UnaryExpr:
		return l.lowerUnaryExpr(n)
	case *aotir.CallExpr:
		return l.lowerCallExpr(n)
	case *aotir.NumCastExpr:
		op, err := l.lowerExpr(n.Operand)
		if err != nil {
			return nil, err
		}
		return &rtree.CallExpr{Func: "mochi_runtime::conv::float_to_int", Args: []rtree.Expr{op}}, nil
	case *aotir.StrLenExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.CallExpr{Func: "mochi_runtime::strings::len", Args: []rtree.Expr{r}}, nil
	case *aotir.StrIndexExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		i, err := l.lowerExpr(n.Index)
		if err != nil {
			return nil, err
		}
		return &rtree.CallExpr{Func: "mochi_runtime::strings::index", Args: []rtree.Expr{r, i}}, nil
	case *aotir.StrContainsExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		s, err := l.lowerExpr(n.Sub)
		if err != nil {
			return nil, err
		}
		return &rtree.CallExpr{Func: "mochi_runtime::strings::contains", Args: []rtree.Expr{r, s}}, nil
	case *aotir.StrSubstringExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		st, err := l.lowerExpr(n.Start)
		if err != nil {
			return nil, err
		}
		en, err := l.lowerExpr(n.End)
		if err != nil {
			return nil, err
		}
		return &rtree.CallExpr{Func: "mochi_runtime::strings::substring", Args: []rtree.Expr{r, st, en}}, nil
	case *aotir.StrReverseExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.CallExpr{Func: "mochi_runtime::strings::reverse", Args: []rtree.Expr{r}}, nil
	case *aotir.MathCallExpr:
		op, err := l.lowerExpr(n.Arg)
		if err != nil {
			return nil, err
		}
		switch n.Func {
		case "abs_i64":
			return &rtree.MethodCall{Receiver: op, Method: "abs"}, nil
		case "abs_f64":
			return &rtree.MethodCall{Receiver: op, Method: "abs"}, nil
		case "floor":
			return &rtree.MethodCall{Receiver: op, Method: "floor"}, nil
		case "ceil":
			return &rtree.MethodCall{Receiver: op, Method: "ceil"}, nil
		}
		return nil, fmt.Errorf("rust lower: unknown math fn %s", n.Func)
	case *aotir.ListLit:
		elems := make([]rtree.Expr, 0, len(n.Elems))
		for _, el := range n.Elems {
			le, err := l.lowerExpr(el)
			if err != nil {
				return nil, err
			}
			elems = append(elems, le)
		}
		return &rtree.MacroVecLit{Elems: elems}, nil
	case *aotir.IndexExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		i, err := l.lowerExpr(n.Index)
		if err != nil {
			return nil, err
		}
		// xs[i as usize].clone() handles both Copy primitives and owned Strings.
		return &rtree.CloneExpr{Expr: &rtree.IndexExpr{
			Receiver: r,
			Index:    &rtree.CastExpr{Expr: i, TypeName: "usize"},
		}}, nil
	case *aotir.LenExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.CastExpr{Expr: &rtree.MethodCall{Receiver: r, Method: "len"}, TypeName: "i64"}, nil
	case *aotir.AppendExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		v, err := l.lowerExpr(n.Value)
		if err != nil {
			return nil, err
		}
		// Functional append: { let mut __t = xs.clone(); __t.push(v); __t }
		return &rtree.BlockExpr{
			Stmts: []rtree.Stmt{
				&rtree.LetStmt{Mutable: true, Name: "__t", Value: &rtree.CloneExpr{Expr: r}},
				&rtree.ExprStmt{Expr: &rtree.MethodCall{
					Receiver: &rtree.Ident{Name: "__t"},
					Method:   "push",
					Args:     []rtree.Expr{v},
				}},
			},
			Tail: &rtree.Ident{Name: "__t"},
		}, nil
	case *aotir.ListSortAscExpr:
		return l.lowerListSortAscExpr(n)
	case *aotir.ListSliceExpr:
		return l.lowerListSliceExpr(n)
	case *aotir.StrConvertExpr:
		op, err := l.lowerExpr(n.Operand)
		if err != nil {
			return nil, err
		}
		// str(x) renders any scalar to its textual form. For strings
		// this is identity; .to_string() preserves an owned String
		// (cloning if needed).
		return &rtree.MethodCall{Receiver: op, Method: "to_string"}, nil
	case *aotir.DatalogQueryExpr:
		return lowerDatalogQueryExpr(n), nil
	case *aotir.MapLit:
		return l.lowerMapLit(n)
	case *aotir.MapGetExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		k, err := l.lowerExpr(n.Key)
		if err != nil {
			return nil, err
		}
		// m.get(&k).cloned().unwrap_or_default()
		return &rtree.MethodCall{
			Receiver: &rtree.MethodCall{
				Receiver: &rtree.MethodCall{
					Receiver: r,
					Method:   "get",
					Args:     []rtree.Expr{&rtree.RefExpr{Expr: k}},
				},
				Method: "cloned",
			},
			Method: "unwrap_or_default",
		}, nil
	case *aotir.MapHasExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		k, err := l.lowerExpr(n.Key)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{
			Receiver: r,
			Method:   "contains_key",
			Args:     []rtree.Expr{&rtree.RefExpr{Expr: k}},
		}, nil
	case *aotir.MapLenExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.CastExpr{Expr: &rtree.MethodCall{Receiver: r, Method: "len"}, TypeName: "i64"}, nil
	case *aotir.MapKeysExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		// sorted Vec of cloned keys for deterministic iteration order
		return &rtree.RawExpr{Code: "{ let mut __k: Vec<_> = " + r.RustExpr() + ".keys().cloned().collect(); __k.sort(); __k }"}, nil
	case *aotir.MapValuesExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.RawExpr{Code: "{ let mut __kv: Vec<_> = " + r.RustExpr() + ".iter().collect(); __kv.sort_by(|a,b| a.0.cmp(b.0)); __kv.into_iter().map(|(_,v)| v.clone()).collect::<Vec<_>>() }"}, nil
	case *aotir.SetLiteralExpr:
		elems := make([]rtree.Expr, 0, len(n.Elems))
		for _, el := range n.Elems {
			le, err := l.lowerExpr(el)
			if err != nil {
				return nil, err
			}
			elems = append(elems, le)
		}
		// HashSet::from_iter(vec![...])
		return &rtree.RawExpr{
			Code: "std::collections::HashSet::<_>::from_iter(" + (&rtree.MacroVecLit{Elems: elems}).RustExpr() + ")",
		}, nil
	case *aotir.SetAddExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		v, err := l.lowerExpr(n.Elem)
		if err != nil {
			return nil, err
		}
		return &rtree.BlockExpr{
			Stmts: []rtree.Stmt{
				&rtree.LetStmt{Mutable: true, Name: "__s", Value: &rtree.CloneExpr{Expr: r}},
				&rtree.ExprStmt{Expr: &rtree.MethodCall{
					Receiver: &rtree.Ident{Name: "__s"},
					Method:   "insert",
					Args:     []rtree.Expr{v},
				}},
			},
			Tail: &rtree.Ident{Name: "__s"},
		}, nil
	case *aotir.SetHasExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		v, err := l.lowerExpr(n.Elem)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{
			Receiver: r,
			Method:   "contains",
			Args:     []rtree.Expr{&rtree.RefExpr{Expr: v}},
		}, nil
	case *aotir.SetLenExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.CastExpr{Expr: &rtree.MethodCall{Receiver: r, Method: "len"}, TypeName: "i64"}, nil
	case *aotir.ListContainsExpr:
		r, err := l.lowerExpr(n.List)
		if err != nil {
			return nil, err
		}
		v, err := l.lowerExpr(n.Value)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{
			Receiver: r,
			Method:   "contains",
			Args:     []rtree.Expr{&rtree.RefExpr{Expr: v}},
		}, nil
	case *aotir.ListSumExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.RawExpr{Code: r.RustExpr() + ".iter().copied().sum::<i64>()"}, nil
	case *aotir.ListMinExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.RawExpr{Code: "*" + r.RustExpr() + ".iter().min().unwrap()"}, nil
	case *aotir.ListMaxExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.RawExpr{Code: "*" + r.RustExpr() + ".iter().max().unwrap()"}, nil
	case *aotir.RecordLit:
		fields := make([]rtree.StructLitField, 0, len(n.Fields))
		for _, f := range n.Fields {
			v, err := l.lowerExpr(f.Value)
			if err != nil {
				return nil, err
			}
			fields = append(fields, rtree.StructLitField{Name: f.Name, Value: v})
		}
		return &rtree.StructLit{TypeName: n.TypeName, Fields: fields}, nil
	case *aotir.FieldAccess:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		fa := &rtree.FieldAccess{Receiver: r, Field: n.FieldName}
		if n.Result == aotir.TypeString {
			// Owned String field: clone on read to avoid moving out of receiver.
			return &rtree.CloneExpr{Expr: fa}, nil
		}
		return fa, nil
	case *aotir.VariantLit:
		fields := make([]rtree.StructLitField, 0, len(n.Fields))
		for _, f := range n.Fields {
			v, err := l.lowerExpr(f.Value)
			if err != nil {
				return nil, err
			}
			fields = append(fields, rtree.StructLitField{Name: f.Name, Value: v})
		}
		return &rtree.EnumVariantLit{
			EnumName:    n.UnionName,
			VariantName: n.VariantName,
			Fields:      fields,
		}, nil
	case *aotir.UnionVarRef:
		// Union values are owning; clone on read so the receiver stays usable.
		return &rtree.CloneExpr{Expr: &rtree.Ident{Name: n.Name}}, nil
	case *aotir.FunLit:
		return l.lowerFunLit(n), nil
	case *aotir.FunCallExpr:
		return l.lowerFunCallExpr(n)
	case *aotir.VariantFieldAccess:
		// Inside a match arm the binding name shadows the field access.
		if l.matchBindings != nil {
			if varName, ok := l.matchBindings[n.FieldName]; ok {
				ident := rtree.Expr(&rtree.Ident{Name: varName})
				if isOwnedType(n.Result) {
					return &rtree.CloneExpr{Expr: ident}, nil
				}
				return ident, nil
			}
		}
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		fa := &rtree.FieldAccess{Receiver: r, Field: n.FieldName}
		if isOwnedType(n.Result) {
			return &rtree.CloneExpr{Expr: fa}, nil
		}
		return fa, nil
	case *aotir.AgentLit:
		fields := make([]rtree.StructLitField, 0, len(n.Fields))
		for _, f := range n.Fields {
			v, err := l.lowerExpr(f.Value)
			if err != nil {
				return nil, fmt.Errorf("agent %q field %q: %w", n.AgentName, f.Name, err)
			}
			fields = append(fields, rtree.StructLitField{Name: f.Name, Value: v})
		}
		return &rtree.StructLit{TypeName: n.AgentName, Fields: fields}, nil
	case *aotir.AgentIntentCallExpr:
		return l.lowerAgentIntentCall(n.AgentName, n.IntentName, n.Receiver, n.Args)
	}
	return nil, fmt.Errorf("rust lower: unsupported expr %T", e)
}

// lowerAgentIntentCall builds a Rust call to the per-intent free
// function: `mochi_agent_NAME__INTENT(&mut receiver, args...)`. The
// receiver may be a VarRef, a __self field access, or any other Expr.
// When the receiver is a VarRef we emit `&mut name` directly so the
// borrow checker sees a plain mutable borrow of the agent binding.
func (l *lowerer) lowerAgentIntentCall(agentName, intentName string, receiver aotir.Expr, callArgs []aotir.Expr) (rtree.Expr, error) {
	args := make([]rtree.Expr, 0, len(callArgs)+1)
	recvExpr, err := l.lowerExpr(receiver)
	if err != nil {
		return nil, fmt.Errorf("agent %q intent %q receiver: %w", agentName, intentName, err)
	}
	// The lowered VarRef returns a CloneExpr for owned types; for an
	// agent receiver we want a mutable borrow of the binding, not a
	// clone, so peel off the clone and emit `&mut name`.
	if ce, ok := recvExpr.(*rtree.CloneExpr); ok {
		recvExpr = ce.Expr
	}
	args = append(args, &rtree.RawExpr{Code: "&mut " + recvExpr.RustExpr()})
	for i, a := range callArgs {
		ea, err := l.lowerExpr(a)
		if err != nil {
			return nil, fmt.Errorf("agent %q intent %q arg %d: %w", agentName, intentName, i, err)
		}
		args = append(args, ea)
	}
	return &rtree.CallExpr{
		Func: "mochi_agent_" + agentName + "__" + intentName,
		Args: args,
	}, nil
}

// lowerFunLit builds the Rust expression that constructs a Box<dyn Fn>
// from a lifted function plus its captured environment.
//
//   * Non-capturing: `Box::new(__anon_N)`. The function item coerces to
//     the trait object when assigned into a `Box<dyn Fn(...) -> R>`
//     binding or argument.
//   * Capturing: `Box::new({ let cap = cap.clone(); ... move |a0, a1|
//     __anon_N(cap.clone(), ..., a0, a1) })`. Owned captures are cloned
//     into the closure environment (so the outer scope keeps its copy)
//     and re-cloned per call (so the closure stays `Fn` rather than
//     `FnOnce`). Copy primitives skip the prelude clone.
func (l *lowerer) lowerFunLit(n *aotir.FunLit) rtree.Expr {
	if len(n.Captures) == 0 {
		return &rtree.CallExpr{
			Func: "Box::new",
			Args: []rtree.Expr{&rtree.Ident{Name: n.FuncName}},
		}
	}
	var prelude []rtree.Stmt
	for _, c := range n.Captures {
		if isOwnedType(c.VarType) {
			prelude = append(prelude, &rtree.LetStmt{
				Name: c.FieldName,
				Value: &rtree.MethodCall{
					Receiver: &rtree.Ident{Name: c.SrcName},
					Method:   "clone",
				},
			})
		}
	}
	paramNames := make([]string, len(n.Sig.ParamTypes))
	paramDecls := make([]string, len(n.Sig.ParamTypes))
	for i, pt := range n.Sig.ParamTypes {
		paramNames[i] = fmt.Sprintf("__arg%d", i)
		paramDecls[i] = paramNames[i] + ": " + rustTypeName(pt)
	}
	callArgs := make([]string, 0, len(n.Captures)+len(paramNames))
	for _, c := range n.Captures {
		if isOwnedType(c.VarType) {
			callArgs = append(callArgs, c.FieldName+".clone()")
		} else {
			callArgs = append(callArgs, c.FieldName)
		}
	}
	callArgs = append(callArgs, paramNames...)
	closureCode := fmt.Sprintf("move |%s| %s(%s)",
		strings.Join(paramDecls, ", "),
		n.FuncName,
		strings.Join(callArgs, ", "))
	closure := rtree.Expr(&rtree.RawExpr{Code: closureCode})
	if len(prelude) > 0 {
		closure = &rtree.BlockExpr{Stmts: prelude, Tail: closure}
	}
	return &rtree.CallExpr{Func: "Box::new", Args: []rtree.Expr{closure}}
}

// lowerFunCallExpr lowers a call to a fun-typed value. Direct identifier
// callees emit as `name(args)`; arbitrary expressions are parenthesised
// so the parser binds the call to the whole expression.
func (l *lowerer) lowerFunCallExpr(n *aotir.FunCallExpr) (rtree.Expr, error) {
	args := make([]rtree.Expr, 0, len(n.Args))
	for _, a := range n.Args {
		ea, err := l.lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args = append(args, ea)
	}
	if vr, ok := n.Callee.(*aotir.VarRef); ok {
		name := strings.TrimPrefix(vr.Name, "__e->")
		return &rtree.CallExpr{Func: name, Args: args}, nil
	}
	callee, err := l.lowerExpr(n.Callee)
	if err != nil {
		return nil, err
	}
	return &rtree.RawExpr{Code: fmt.Sprintf("(%s)(%s)", callee.RustExpr(), joinExprs(args))}, nil
}

func joinExprs(args []rtree.Expr) string {
	parts := make([]string, len(args))
	for i, a := range args {
		parts[i] = a.RustExpr()
	}
	return strings.Join(parts, ", ")
}

func (l *lowerer) lowerMapLit(m *aotir.MapLit) (rtree.Expr, error) {
	stmts := make([]rtree.Stmt, 0, len(m.Keys)+1)
	stmts = append(stmts, &rtree.LetStmt{
		Mutable: true,
		Name:    "__m",
		Value:   &rtree.RawExpr{Code: "std::collections::HashMap::new()"},
	})
	for i, k := range m.Keys {
		kk, err := l.lowerExpr(k)
		if err != nil {
			return nil, err
		}
		vv, err := l.lowerExpr(m.Values[i])
		if err != nil {
			return nil, err
		}
		stmts = append(stmts, &rtree.ExprStmt{Expr: &rtree.MethodCall{
			Receiver: &rtree.Ident{Name: "__m"},
			Method:   "insert",
			Args:     []rtree.Expr{kk, vv},
		}})
	}
	return &rtree.BlockExpr{Stmts: stmts, Tail: &rtree.Ident{Name: "__m"}}, nil
}

func (l *lowerer) lowerBinaryExpr(b *aotir.BinaryExpr) (rtree.Expr, error) {
	left, err := l.lowerExpr(b.Left)
	if err != nil {
		return nil, err
	}
	right, err := l.lowerExpr(b.Right)
	if err != nil {
		return nil, err
	}
	switch b.Op {
	case aotir.BinStrCat:
		return &rtree.CallExpr{
			Func: "mochi_runtime::strings::cat",
			Args: []rtree.Expr{left, right},
		}, nil
	case aotir.BinEqStr, aotir.BinNeStr:
		op := rtree.OpEq
		if b.Op == aotir.BinNeStr {
			op = rtree.OpNe
		}
		return &rtree.BinaryExpr{Op: op, Left: left, Right: right}, nil
	}
	return &rtree.BinaryExpr{Op: rustBinOp(b.Op), Left: left, Right: right}, nil
}

func rustBinOp(op aotir.BinOp) rtree.BinOp {
	switch op {
	case aotir.BinAddI64, aotir.BinAddF64:
		return rtree.OpAdd
	case aotir.BinSubI64, aotir.BinSubF64:
		return rtree.OpSub
	case aotir.BinMulI64, aotir.BinMulF64:
		return rtree.OpMul
	case aotir.BinDivI64, aotir.BinDivF64:
		return rtree.OpDiv
	case aotir.BinModI64:
		return rtree.OpMod
	case aotir.BinEqI64, aotir.BinEqF64, aotir.BinEqBool, aotir.BinEqStr,
		aotir.BinEqRec, aotir.BinEqList, aotir.BinEqMap:
		return rtree.OpEq
	case aotir.BinNeI64, aotir.BinNeF64, aotir.BinNeBool, aotir.BinNeStr,
		aotir.BinNeRec, aotir.BinNeList, aotir.BinNeMap:
		return rtree.OpNe
	case aotir.BinLtI64, aotir.BinLtF64:
		return rtree.OpLt
	case aotir.BinLeI64, aotir.BinLeF64:
		return rtree.OpLe
	case aotir.BinGtI64, aotir.BinGtF64:
		return rtree.OpGt
	case aotir.BinGeI64, aotir.BinGeF64:
		return rtree.OpGe
	case aotir.BinAndBool:
		return rtree.OpAnd
	case aotir.BinOrBool:
		return rtree.OpOr
	}
	return rtree.OpAdd
}

func (l *lowerer) lowerUnaryExpr(u *aotir.UnaryExpr) (rtree.Expr, error) {
	op, err := l.lowerExpr(u.Operand)
	if err != nil {
		return nil, err
	}
	switch u.Op {
	case aotir.UnNegI64, aotir.UnNegF64:
		return &rtree.UnaryExpr{Op: "-", Operand: op}, nil
	case aotir.UnNotBool:
		return &rtree.UnaryExpr{Op: "!", Operand: op}, nil
	}
	return nil, fmt.Errorf("rust lower: unknown unary op %v", u.Op)
}

func (l *lowerer) lowerCallExpr(c *aotir.CallExpr) (rtree.Expr, error) {
	if mapped, ok := builtinValueCall(c.Func); ok {
		args := make([]rtree.Expr, 0, len(c.Args))
		for _, a := range c.Args {
			ea, err := l.lowerExpr(a)
			if err != nil {
				return nil, err
			}
			args = append(args, ea)
		}
		return &rtree.CallExpr{Func: mapped, Args: args}, nil
	}
	args := make([]rtree.Expr, 0, len(c.Args))
	for _, a := range c.Args {
		ea, err := l.lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args = append(args, ea)
	}
	return &rtree.CallExpr{Func: c.Func, Args: args}, nil
}

func builtinValueCall(name string) (string, bool) {
	switch name {
	case "mochi_int_to_float", "mochi_float_to_int":
		// These are scalar casts: int_to_float -> f64 cast, float_to_int -> i64 cast.
		return "mochi_runtime::conv::" + strings.TrimPrefix(name, "mochi_"), true
	case "mochi_str_to_int":
		return "mochi_runtime::conv::str_to_int", true
	case "mochi_int_to_str":
		return "mochi_runtime::conv::int_to_str", true
	case "mochi_str_len":
		return "mochi_runtime::strings::len", true
	case "mochi_str_index":
		return "mochi_runtime::strings::index", true
	case "mochi_str_contains":
		return "mochi_runtime::strings::contains", true
	}
	return "", false
}

// isOwnedType reports whether values of t are owning (need .clone() on reads
// when bound to a variable). Copy primitives like i64/f64/bool return false.
// TypeFun is intentionally excluded because Box<dyn Fn> does not implement
// Clone; closure values are passed by-move and called through auto-deref.
func isOwnedType(t aotir.Type) bool {
	switch t {
	case aotir.TypeString, aotir.TypeRecord, aotir.TypeList, aotir.TypeMap,
		aotir.TypeSet, aotir.TypeUnion, aotir.TypeChan,
		aotir.TypeStream, aotir.TypeAgent:
		return true
	}
	return false
}

func rustTypeName(t aotir.Type) string {
	switch t {
	case aotir.TypeUnit:
		return "()"
	case aotir.TypeString:
		return "String"
	case aotir.TypeInt:
		return "i64"
	case aotir.TypeFloat:
		return "f64"
	case aotir.TypeBool:
		return "bool"
	}
	return "()"
}

func rustReturnType(t aotir.Type) string {
	if t == aotir.TypeUnit {
		return ""
	}
	return rustTypeName(t)
}

// rustCollectionTypeName builds a Rust type string for a (possibly
// nested) collection. It handles Vec<T>, Vec<Vec<T>>, Vec<HashMap<K,V>>,
// HashMap<K,V>, HashMap<K, Vec<V>>, HashSet<T>. Primitives/records/
// unions fall back to rustNamedTypeName.
func rustCollectionTypeName(t aotir.Type, recordName, unionName string,
	elemType aotir.Type, elemRecordName string, innerElemType aotir.Type,
	mapKey, mapValue aotir.Type,
	keyType, valueType aotir.Type, listValueElem aotir.Type,
	funSig *aotir.FunSig) string {
	switch t {
	case aotir.TypeList:
		switch elemType {
		case aotir.TypeList:
			inner := rustNamedTypeName(innerElemType, "", "")
			return "Vec<Vec<" + inner + ">>"
		case aotir.TypeMap:
			k := rustNamedTypeName(mapKey, "", "")
			v := rustNamedTypeName(mapValue, "", "")
			return "Vec<std::collections::HashMap<" + k + ", " + v + ">>"
		case aotir.TypeRecord:
			return "Vec<" + elemRecordName + ">"
		default:
			return "Vec<" + rustNamedTypeName(elemType, "", "") + ">"
		}
	case aotir.TypeMap:
		k := rustNamedTypeName(keyType, "", "")
		if valueType == aotir.TypeList {
			v := rustNamedTypeName(listValueElem, "", "")
			return "std::collections::HashMap<" + k + ", Vec<" + v + ">>"
		}
		v := rustNamedTypeName(valueType, "", "")
		return "std::collections::HashMap<" + k + ", " + v + ">"
	case aotir.TypeSet:
		return "std::collections::HashSet<" + rustNamedTypeName(elemType, "", "") + ">"
	case aotir.TypeFun:
		if funSig != nil {
			return rustFnType(funSig)
		}
	}
	return rustNamedTypeName(t, recordName, unionName)
}

func rustParamTypeFromParam(p aotir.Param) string {
	return rustCollectionTypeName(p.Type, p.RecordName, p.UnionName,
		p.ElemType, p.ElemRecordName, p.InnerElemType,
		p.MapElemKeyType, p.MapElemValueType,
		p.KeyType, p.ValueType, p.ListValueElemType,
		p.FunSig)
}

func rustReturnTypeFromFunction(fn *aotir.Function) string {
	return rustCollectionTypeName(fn.ReturnType, fn.ReturnRecordName, fn.ReturnUnionName,
		fn.ReturnElemType, fn.ReturnElemRecordName, fn.ReturnInnerElemType,
		fn.ReturnMapElemKeyType, fn.ReturnMapElemValueType,
		fn.ReturnKeyType, fn.ReturnValueType, fn.ReturnListValueElemType,
		fn.ReturnFunSig)
}

// lowerDatalogQueryExpr runs a compile-time semi-naive bottom-up Datalog
// evaluator over e.Prog and emits a Rust vec! literal of String values.
// The result is a flat list (free-variable values from each matching
// tuple concatenated), matching the C / BEAM backends.
func lowerDatalogQueryExpr(e *aotir.DatalogQueryExpr) rtree.Expr {
	if e.Prog == nil {
		return &rtree.RawExpr{Code: "Vec::<String>::new()"}
	}
	results := datalogEval(e)
	if len(results) == 0 {
		return &rtree.RawExpr{Code: "Vec::<String>::new()"}
	}
	args := make([]rtree.Expr, len(results))
	for i, s := range results {
		args[i] = &rtree.MethodCall{
			Receiver: &rtree.StringLit{Value: s},
			Method:   "to_string",
		}
	}
	return &rtree.MacroVecLit{Elems: args}
}

// datalogEval performs semi-naive bottom-up evaluation of e.Prog and
// returns the flat list of free-variable values from matching tuples.
func datalogEval(e *aotir.DatalogQueryExpr) []string {
	state := map[string][][]string{}
	for _, f := range e.Prog.Facts {
		args := make([]string, len(f.Args))
		copy(args, f.Args)
		state[f.Name] = append(state[f.Name], args)
	}
	for {
		changed := false
		for _, rule := range e.Prog.Rules {
			newTuples := datalogDeriveRule(rule, state)
			for _, t := range newTuples {
				if !datalogTupleInRelation(state[rule.HeadName], t) {
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

func datalogDeriveRule(rule aotir.DatalogRule, state map[string][][]string) [][]string {
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
						val := datalogResolveArg(arg, env)
						if val != t[i] {
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
				newEnv := datalogCopyEnv(env)
				ok := true
				for i, arg := range lit.Args {
					if datalogIsVariable(arg) {
						if existing, bound := newEnv[arg]; bound {
							if existing != t[i] {
								ok = false
								break
							}
						} else {
							newEnv[arg] = t[i]
						}
					} else {
						expected := datalogUnquote(arg)
						if t[i] != expected {
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
	var out [][]string
	for _, env := range results {
		head := make([]string, len(rule.HeadArgs))
		for i, ha := range rule.HeadArgs {
			if datalogIsVariable(ha) {
				head[i] = env[ha]
			} else {
				head[i] = datalogUnquote(ha)
			}
		}
		out = append(out, head)
	}
	return out
}

func datalogTupleInRelation(rel [][]string, t []string) bool {
	for _, r := range rel {
		if len(r) != len(t) {
			continue
		}
		eq := true
		for i := range r {
			if r[i] != t[i] {
				eq = false
				break
			}
		}
		if eq {
			return true
		}
	}
	return false
}

func datalogResolveArg(arg string, env map[string]string) string {
	if datalogIsVariable(arg) {
		return env[arg]
	}
	return datalogUnquote(arg)
}

func datalogIsVariable(s string) bool {
	return len(s) > 0 && s[0] != '"'
}

func datalogUnquote(s string) string {
	if len(s) >= 2 && s[0] == '"' && s[len(s)-1] == '"' {
		return s[1 : len(s)-1]
	}
	return s
}

func datalogCopyEnv(env map[string]string) map[string]string {
	out := make(map[string]string, len(env))
	for k, v := range env {
		out[k] = v
	}
	return out
}

// lowerListSortAscExpr emits a Rust block expression that clones the
// receiver list, sorts it in ascending order, and returns the new
// list. f64 elements use sort_by(partial_cmp) because f64 is not Ord.
func (l *lowerer) lowerListSortAscExpr(n *aotir.ListSortAscExpr) (rtree.Expr, error) {
	r, err := l.lowerExpr(n.Receiver)
	if err != nil {
		return nil, err
	}
	var sortStmt rtree.Stmt
	if n.ElemType == aotir.TypeFloat {
		sortStmt = &rtree.ExprStmt{Expr: &rtree.RawExpr{
			Code: "__t.sort_by(|a, b| a.partial_cmp(b).unwrap())",
		}}
	} else {
		sortStmt = &rtree.ExprStmt{Expr: &rtree.MethodCall{
			Receiver: &rtree.Ident{Name: "__t"},
			Method:   "sort",
		}}
	}
	return &rtree.BlockExpr{
		Stmts: []rtree.Stmt{
			&rtree.LetStmt{Mutable: true, Name: "__t", Value: &rtree.CloneExpr{Expr: r}},
			sortStmt,
		},
		Tail: &rtree.Ident{Name: "__t"},
	}, nil
}

// lowerListSliceExpr emits a Rust block expression that clamps Start
// and End to [0, len], slices the cloned receiver, and returns a fresh
// Vec. Mochi/C semantics: out-of-range bounds are clamped silently.
func (l *lowerer) lowerListSliceExpr(n *aotir.ListSliceExpr) (rtree.Expr, error) {
	r, err := l.lowerExpr(n.Receiver)
	if err != nil {
		return nil, err
	}
	start, err := l.lowerExpr(n.Start)
	if err != nil {
		return nil, err
	}
	end, err := l.lowerExpr(n.End)
	if err != nil {
		return nil, err
	}
	return &rtree.BlockExpr{
		Stmts: []rtree.Stmt{
			&rtree.LetStmt{Name: "__src", Value: &rtree.CloneExpr{Expr: r}},
			&rtree.LetStmt{Name: "__len", Value: &rtree.RawExpr{Code: "__src.len() as i64"}},
			&rtree.LetStmt{Name: "__s_raw", Value: start},
			&rtree.LetStmt{Name: "__e_raw", Value: end},
			&rtree.LetStmt{Name: "__s", Value: &rtree.RawExpr{
				Code: "if __s_raw < 0 { 0 } else if __s_raw > __len { __len as usize } else { __s_raw as usize }",
			}},
			&rtree.LetStmt{Name: "__e", Value: &rtree.RawExpr{
				Code: "if __e_raw < 0 { 0 } else if __e_raw > __len { __len as usize } else { __e_raw as usize }",
			}},
			&rtree.LetStmt{Name: "__e2", Value: &rtree.RawExpr{
				Code: "if __e < __s { __s } else { __e }",
			}},
		},
		Tail: &rtree.RawExpr{Code: "__src[__s..__e2].to_vec()"},
	}, nil
}
