package types

import "mochi/parser"

// parseDeclaredEffects converts the raw label list on FunStmt.Effects
// into a typed EffectSet. Each unknown label produces a T064
// diagnostic but the parse keeps going so the caller sees every bad
// label in one pass.
func parseDeclaredEffects(fn *parser.FunStmt) (EffectSet, []error) {
	var set EffectSet
	var errs []error
	for _, name := range fn.Effects {
		label, ok := ParseEffectLabel(name)
		if !ok {
			errs = append(errs, errUnknownEffectLabel(fn.Pos, name))
			continue
		}
		set = set.Add(label)
	}
	return set, errs
}

// inferFunctionEffects walks fn's body and returns the union of
// effect labels carried by each call and statement-level construct it
// reaches. MEP-15 Stage 2: replaces the boolean legacyEffectsFromPure
// bridge with real propagation.
//
// Forward references rely on the caller running this in a fixpoint:
// callees not yet registered contribute EmptyEffects on the first
// sweep; later sweeps pick up the missing labels. The lattice is
// finite (one bit per label) so the fixpoint terminates in at most
// effectMax rounds across any dependency chain.
func inferFunctionEffects(fn *parser.FunStmt, env *Env) EffectSet {
	if fn == nil {
		return EmptyEffects
	}
	child := NewEnv(env)
	for _, p := range fn.Params {
		if p.Type != nil {
			child.SetVar(p.Name, resolveTypeRef(p.Type, env), false)
		} else {
			child.SetVar(p.Name, AnyType{}, false)
		}
	}
	var effs EffectSet
	for _, stmt := range fn.Body {
		effs = effs.Union(stmtEffects(stmt, child))
	}
	return effs
}

// stmtEffects returns the effects produced by evaluating s in env. The
// helper mirrors the structure of statementHasImpureCall but tracks
// the typed set rather than a boolean. Nested control-flow bodies are
// walked so a `print` inside an `if` branch surfaces at the function
// boundary.
func stmtEffects(s *parser.Statement, env *Env) EffectSet {
	if s == nil {
		return EmptyEffects
	}
	var e EffectSet
	switch {
	case s.Let != nil:
		e = exprEffects(s.Let.Value, env)
	case s.Var != nil:
		e = exprEffects(s.Var.Value, env)
	case s.Assign != nil:
		e = exprEffects(s.Assign.Value, env)
		for _, idx := range s.Assign.Index {
			e = e.Union(exprEffects(idx.Start, env)).Union(exprEffects(idx.End, env))
		}
	case s.Return != nil:
		e = exprEffects(s.Return.Value, env)
	case s.Expr != nil:
		e = exprEffects(s.Expr.Expr, env)
	case s.If != nil:
		e = ifEffects(s.If, env)
	case s.While != nil:
		e = exprEffects(s.While.Cond, env)
		for _, b := range s.While.Body {
			e = e.Union(stmtEffects(b, env))
		}
	case s.For != nil:
		e = exprEffects(s.For.Source, env).Union(exprEffects(s.For.RangeEnd, env))
		for _, b := range s.For.Body {
			e = e.Union(stmtEffects(b, env))
		}
	case s.Update != nil:
		if s.Update.Where != nil {
			e = e.Union(exprEffects(s.Update.Where, env))
		}
		if s.Update.Set != nil {
			for _, it := range s.Update.Set.Items {
				e = e.Union(exprEffects(it.Value, env))
			}
		}
	case s.Fetch != nil:
		e = NewEffectSet(EffectNet)
		e = e.Union(exprEffects(s.Fetch.URL, env)).Union(exprEffects(s.Fetch.With, env))
	}
	return e
}

func ifEffects(i *parser.IfStmt, env *Env) EffectSet {
	if i == nil {
		return EmptyEffects
	}
	e := exprEffects(i.Cond, env)
	for _, b := range i.Then {
		e = e.Union(stmtEffects(b, env))
	}
	if i.ElseIf != nil {
		e = e.Union(ifEffects(i.ElseIf, env))
	}
	for _, b := range i.Else {
		e = e.Union(stmtEffects(b, env))
	}
	return e
}

func exprEffects(e *parser.Expr, env *Env) EffectSet {
	if e == nil || e.Binary == nil {
		return EmptyEffects
	}
	eff := unaryEffects(e.Binary.Left, env)
	for _, op := range e.Binary.Right {
		eff = eff.Union(unaryEffects(op.Right, env))
	}
	return eff
}

func unaryEffects(u *parser.Unary, env *Env) EffectSet {
	if u == nil {
		return EmptyEffects
	}
	return postfixEffects(u.Value, env)
}

func postfixEffects(p *parser.PostfixExpr, env *Env) EffectSet {
	if p == nil {
		return EmptyEffects
	}
	eff := primaryEffects(p.Target, env)
	for _, op := range p.Ops {
		if op.Call != nil {
			for _, a := range op.Call.Args {
				eff = eff.Union(exprEffects(a, env))
			}
			if p.Target != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
				if t, err := env.GetVar(p.Target.Selector.Root); err == nil {
					if ft, ok := t.(FuncType); ok {
						eff = eff.Union(ft.Effects)
					}
				}
			}
		}
		if op.Index != nil {
			eff = eff.Union(exprEffects(op.Index.Start, env)).Union(exprEffects(op.Index.End, env))
		}
	}
	return eff
}

func primaryEffects(p *parser.Primary, env *Env) EffectSet {
	if p == nil {
		return EmptyEffects
	}
	switch {
	case p.Call != nil:
		var eff EffectSet
		if t, err := env.GetVar(p.Call.Func); err == nil {
			if ft, ok := t.(FuncType); ok {
				eff = ft.Effects
			}
		}
		for _, a := range p.Call.Args {
			eff = eff.Union(exprEffects(a, env))
		}
		return eff
	case p.Group != nil:
		return exprEffects(p.Group, env)
	case p.Fetch != nil:
		return NewEffectSet(EffectNet)
	case p.Load != nil:
		return NewEffectSet(EffectFS)
	case p.Save != nil:
		return NewEffectSet(EffectFS)
	}
	return EmptyEffects
}
