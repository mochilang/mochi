package parser

import (
	"github.com/alecthomas/participle/v2/lexer"
	"mochi/diagnostic"
)

// Grammar-level diagnostic templates. The codes are stable: see MEP 2.
var (
	errEmptyIndexExpr = diagnostic.Template{
		Code:    "P060",
		Message: "index expression has no key, range, or step",
		Help:    "Use `xs[i]` for a subscript, or supply at least one of start/end/step in a slice like `xs[1:]`, `xs[:3]`, or `xs[::2]`.",
	}
	errMatchCaseMissingBody = diagnostic.Template{
		Code:    "P061",
		Message: "match case has no result expression or block",
		Help:    "Provide a result after `=>`, e.g. `0 => \"zero\"`, or a block: `0 => { ... }`.",
	}
	errPatternShape = diagnostic.Template{
		Code:    "P063",
		Message: "match case pattern is not a recognised pattern shape",
		Help:    "A pattern must be a literal (e.g. `1`, `\"foo\"`, `true`, `none`), an identifier (e.g. `x`, `_`, or a variant name like `Leaf`), or a variant constructor call (e.g. `Node(l, v, r)`). Compound expressions, indexing, field access, casts, and quoted constructors are not patterns.",
	}
	errUnknownImportLang = diagnostic.Template{
		Code:    "P064",
		Message: "unknown import language: %q",
		Help:    "The supported languages are `go`, `python`, `typescript`, `zig`, `lua`, `clj`, and `clojure`. Omit the language to use the Mochi default, or check the spelling.",
	}
	errUselessExprStmt = diagnostic.Template{
		Code:    "P065",
		Message: "expression statement has no observable effect",
		Help:    "An expression used as a statement must perform I/O or call a function. Bare values, arithmetic, and field/index access compute a result that nothing reads. Use `let _ = ...` if you need to evaluate for a side effect inside an argument, or remove the line.",
	}
)

// knownImportLangs is the set of host-language identifiers Mochi recognises
// in an `import <lang> "path" ...` statement. Backends consume `*ImportStmt`
// and dispatch on `Lang`; a value outside this set has no consumer and
// silently no-ops. Rejecting it at parse time turns a typo like
// `import pythn "math" as math` into a positioned diagnostic instead of a
// runtime "module not found" much later.
var knownImportLangs = map[string]struct{}{
	"go":         {},
	"python":     {},
	"typescript": {},
	"zig":        {},
	"lua":        {},
	"clj":        {},
	"clojure":    {},
}

// normalizeProgram performs the post-parse pass that turns the raw
// participle parse into a fully validated Mochi AST. It does three jobs:
//
//   - lift `TypeDecl.SingleVariant` into `Variants` so consumers see a
//     unified slice (the discriminator only exists to disambiguate the
//     grammar);
//   - reject silently-accepted shapes the grammar cannot express on its
//     own, like `xs[]` and a `match` case with no body;
//   - keep the pass deterministic so two runs over the same source always
//     produce the same diagnostic in source order.
func normalizeProgram(prog *Program) error {
	if prog == nil {
		return nil
	}
	for _, stmt := range prog.Statements {
		if err := normalizeStatement(stmt); err != nil {
			return err
		}
	}
	return nil
}

func normalizeStatement(s *Statement) error {
	if s == nil {
		return nil
	}
	switch {
	case s.Type != nil:
		if err := normalizeTypeDecl(s.Type); err != nil {
			return err
		}
	case s.Let != nil:
		if s.Let.Value != nil {
			if err := normalizeExpr(s.Let.Value); err != nil {
				return err
			}
		}
	case s.Var != nil:
		if s.Var.Value != nil {
			if err := normalizeExpr(s.Var.Value); err != nil {
				return err
			}
		}
	case s.Assign != nil:
		for _, idx := range s.Assign.Index {
			if err := validateIndexOp(idx); err != nil {
				return err
			}
			if err := normalizeIndexOp(idx); err != nil {
				return err
			}
		}
		if s.Assign.Value != nil {
			if err := normalizeExpr(s.Assign.Value); err != nil {
				return err
			}
		}
	case s.Return != nil:
		if s.Return.Value != nil {
			if err := normalizeExpr(s.Return.Value); err != nil {
				return err
			}
		}
	case s.Expect != nil:
		if s.Expect.Value != nil {
			if err := normalizeExpr(s.Expect.Value); err != nil {
				return err
			}
		}
	case s.If != nil:
		if err := normalizeIfStmt(s.If); err != nil {
			return err
		}
	case s.While != nil:
		if s.While.Cond != nil {
			if err := normalizeExpr(s.While.Cond); err != nil {
				return err
			}
		}
		for _, b := range s.While.Body {
			if err := normalizeStatement(b); err != nil {
				return err
			}
		}
	case s.For != nil:
		if s.For.Source != nil {
			if err := normalizeExpr(s.For.Source); err != nil {
				return err
			}
		}
		if s.For.RangeEnd != nil {
			if err := normalizeExpr(s.For.RangeEnd); err != nil {
				return err
			}
		}
		for _, b := range s.For.Body {
			if err := normalizeStatement(b); err != nil {
				return err
			}
		}
	case s.Fun != nil:
		for _, b := range s.Fun.Body {
			if err := normalizeStatement(b); err != nil {
				return err
			}
		}
	case s.Test != nil:
		for _, b := range s.Test.Body {
			if err := normalizeStatement(b); err != nil {
				return err
			}
		}
	case s.Bench != nil:
		for _, b := range s.Bench.Body {
			if err := normalizeStatement(b); err != nil {
				return err
			}
		}
	case s.Import != nil:
		if err := validateImportLang(s.Import); err != nil {
			return err
		}
	case s.Expr != nil:
		if s.Expr.Expr != nil {
			if err := normalizeExpr(s.Expr.Expr); err != nil {
				return err
			}
			if !exprHasObservableEffect(s.Expr.Expr) {
				return errUselessExprStmt.New(s.Expr.Pos)
			}
		}
	}
	return nil
}

// exprHasObservableEffect reports whether the expression, evaluated as a
// statement, can produce a side effect the user might care about. The rule
// is structural: a call, fetch, save, load, or generate anywhere in the
// reachable expression makes the statement observable. The traversal does
// not descend into function literal bodies, because a lambda is a value;
// the body only runs when the lambda is later invoked.
func exprHasObservableEffect(e *Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if unaryHasObservableEffect(e.Binary.Left) {
		return true
	}
	for _, op := range e.Binary.Right {
		if unaryHasObservableEffect(op.Right) {
			return true
		}
	}
	return false
}

func unaryHasObservableEffect(u *Unary) bool {
	if u == nil || u.Value == nil {
		return false
	}
	return postfixHasObservableEffect(u.Value)
}

func postfixHasObservableEffect(p *PostfixExpr) bool {
	if p == nil {
		return false
	}
	for _, op := range p.Ops {
		if op.Call != nil {
			return true
		}
		if op.Index != nil {
			if exprHasObservableEffect(op.Index.Start) ||
				exprHasObservableEffect(op.Index.End) ||
				exprHasObservableEffect(op.Index.Step) {
				return true
			}
		}
	}
	return primaryHasObservableEffect(p.Target)
}

func primaryHasObservableEffect(p *Primary) bool {
	if p == nil {
		return false
	}
	switch {
	case p.Call != nil, p.Fetch != nil, p.Save != nil, p.Load != nil, p.Generate != nil:
		return true
	case p.Group != nil:
		return exprHasObservableEffect(p.Group)
	case p.If != nil:
		return ifExprHasObservableEffect(p.If)
	case p.Match != nil:
		for _, c := range p.Match.Cases {
			if exprHasObservableEffect(c.Result) {
				return true
			}
			for _, s := range c.Block {
				if stmtHasObservableEffect(s) {
					return true
				}
			}
		}
		return exprHasObservableEffect(p.Match.Target)
	case p.List != nil:
		for _, el := range p.List.Elems {
			if exprHasObservableEffect(el) {
				return true
			}
		}
	case p.Map != nil:
		for _, it := range p.Map.Items {
			if exprHasObservableEffect(it.Key) || exprHasObservableEffect(it.Value) {
				return true
			}
		}
	case p.Struct != nil:
		for _, f := range p.Struct.Fields {
			if exprHasObservableEffect(f.Value) {
				return true
			}
		}
	}
	return false
}

func ifExprHasObservableEffect(e *IfExpr) bool {
	if e == nil {
		return false
	}
	if exprHasObservableEffect(e.Then) || exprHasObservableEffect(e.Else) {
		return true
	}
	return ifExprHasObservableEffect(e.ElseIf)
}

func stmtHasObservableEffect(s *Statement) bool {
	if s == nil {
		return false
	}
	if s.Expr != nil {
		return exprHasObservableEffect(s.Expr.Expr)
	}
	return true
}

func normalizeIfStmt(s *IfStmt) error {
	if s == nil {
		return nil
	}
	if s.Cond != nil {
		if err := normalizeExpr(s.Cond); err != nil {
			return err
		}
	}
	for _, b := range s.Then {
		if err := normalizeStatement(b); err != nil {
			return err
		}
	}
	if s.ElseIf != nil {
		if err := normalizeIfStmt(s.ElseIf); err != nil {
			return err
		}
	}
	for _, b := range s.Else {
		if err := normalizeStatement(b); err != nil {
			return err
		}
	}
	return nil
}

// normalizeTypeDecl lifts SingleVariant into the Variants slice so all
// downstream consumers see a uniform shape. SingleVariant only exists to
// disambiguate the grammar.
func normalizeTypeDecl(td *TypeDecl) error {
	if td == nil || td.SingleVariant == nil {
		return nil
	}
	v := &TypeVariant{
		Pos:    td.SingleVariant.Pos,
		Name:   td.SingleVariant.Name,
		Fields: td.SingleVariant.Fields,
	}
	td.Variants = append(td.Variants, v)
	td.SingleVariant = nil
	return nil
}

// validateImportLang rejects `import <lang> "..." as ...` where <lang> is
// not in the language's registered host set. The grammar admits any
// identifier in that position because Mochi imports are open to any
// backend; the runtime rejects unknown values with a "module not found"
// far later. Catching the typo at parse time keeps the diagnostic local.
func validateImportLang(im *ImportStmt) error {
	if im == nil || im.Lang == nil {
		return nil
	}
	if _, ok := knownImportLangs[*im.Lang]; ok {
		return nil
	}
	return errUnknownImportLang.New(im.Pos, *im.Lang)
}

// validateIndexOp rejects `xs[]`, `xs[:]`, and `xs[::]`. The grammar
// accepts those shapes because every component is independently optional.
// A valid index/slice must carry at least one of {start, end, step}.
func validateIndexOp(op *IndexOp) error {
	if op == nil {
		return nil
	}
	if op.Start == nil && op.End == nil && op.Step == nil {
		return errEmptyIndexExpr.New(op.Pos)
	}
	return nil
}

func normalizeIndexOp(op *IndexOp) error {
	if op == nil {
		return nil
	}
	if op.Start != nil {
		if err := normalizeExpr(op.Start); err != nil {
			return err
		}
	}
	if op.End != nil {
		if err := normalizeExpr(op.End); err != nil {
			return err
		}
	}
	if op.Step != nil {
		if err := normalizeExpr(op.Step); err != nil {
			return err
		}
	}
	return nil
}

// validateMatchCase rejects `pattern => ` with no body. The grammar
// allows it because both Result and Block are optional.
func validateMatchCase(c *MatchCase) error {
	if c == nil {
		return nil
	}
	if c.Result == nil && len(c.Block) == 0 {
		return errMatchCaseMissingBody.New(c.Pos)
	}
	return validatePatternShape(c.Pattern, c.Pos)
}

// validatePatternShape enforces the four pattern shapes spelled out in
// MEP 5 §"Inference for matches":
//
//   - literal: a `Primary.Lit`, with no prefix operators
//   - identifier: a `Primary.Selector` with one component and no postfix
//     ops (covers `_`, a bare bind, and a nullary variant name)
//   - variant call: a `Primary.Call` whose args are themselves patterns
//   - parenthesised pattern: `Primary.Group` wrapping a pattern
//
// Anything else (binary ops, indexing, field access, casts, list/map/struct
// literals, conditionals, lambdas, queries) is rejected with P063. The
// grammar admits these because pattern position reuses `*Expr`; the check
// is structural and runs during the normalisation pass.
func validatePatternShape(e *Expr, pos lexer.Position) error {
	if e == nil || e.Binary == nil {
		return nil
	}
	if len(e.Binary.Right) != 0 {
		return errPatternShape.New(pos)
	}
	u := e.Binary.Left
	if u == nil || u.Value == nil {
		return errPatternShape.New(pos)
	}
	if len(u.Ops) != 0 {
		return errPatternShape.New(pos)
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return errPatternShape.New(pos)
	}
	prim := p.Target
	if prim == nil {
		return errPatternShape.New(pos)
	}
	switch {
	case prim.Lit != nil:
		return nil
	case prim.Selector != nil:
		if len(prim.Selector.Tail) != 0 {
			return errPatternShape.New(pos)
		}
		return nil
	case prim.Call != nil:
		for _, a := range prim.Call.Args {
			if err := validatePatternShape(a, pos); err != nil {
				return err
			}
		}
		return nil
	case prim.Group != nil:
		return validatePatternShape(prim.Group, pos)
	default:
		return errPatternShape.New(pos)
	}
}

// normalizeExpr recursively descends into expressions to validate nested
// index ops and match cases. The traversal is conservative: it only walks
// the spine of the AST that can contain `IndexOp` or `MatchExpr` nodes.
func normalizeExpr(e *Expr) error {
	if e == nil || e.Binary == nil {
		return nil
	}
	if err := normalizeUnary(e.Binary.Left); err != nil {
		return err
	}
	for _, op := range e.Binary.Right {
		if err := normalizeUnary(op.Right); err != nil {
			return err
		}
	}
	return nil
}

func normalizeUnary(u *Unary) error {
	if u == nil {
		return nil
	}
	return normalizePostfix(u.Value)
}

func normalizePostfix(p *PostfixExpr) error {
	if p == nil {
		return nil
	}
	if err := normalizePrimary(p.Target); err != nil {
		return err
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			if err := validateIndexOp(op.Index); err != nil {
				return err
			}
			if err := normalizeIndexOp(op.Index); err != nil {
				return err
			}
		}
		if op.Call != nil {
			for _, a := range op.Call.Args {
				if err := normalizeExpr(a); err != nil {
					return err
				}
			}
		}
	}
	return nil
}

func normalizePrimary(p *Primary) error {
	if p == nil {
		return nil
	}
	switch {
	case p.Group != nil:
		return normalizeExpr(p.Group)
	case p.Match != nil:
		for _, c := range p.Match.Cases {
			if err := validateMatchCase(c); err != nil {
				return err
			}
			if c.Result != nil {
				if err := normalizeExpr(c.Result); err != nil {
					return err
				}
			}
			for _, st := range c.Block {
				if err := normalizeStatement(st); err != nil {
					return err
				}
			}
		}
		if p.Match.Target != nil {
			return normalizeExpr(p.Match.Target)
		}
	case p.If != nil:
		return normalizeIfExpr(p.If)
	case p.List != nil:
		for _, el := range p.List.Elems {
			if err := normalizeExpr(el); err != nil {
				return err
			}
		}
	case p.Map != nil:
		for _, it := range p.Map.Items {
			if it.Key != nil {
				if err := normalizeExpr(it.Key); err != nil {
					return err
				}
			}
			if it.Value != nil {
				if err := normalizeExpr(it.Value); err != nil {
					return err
				}
			}
		}
	case p.Call != nil:
		for _, a := range p.Call.Args {
			if err := normalizeExpr(a); err != nil {
				return err
			}
		}
	case p.Struct != nil:
		for _, f := range p.Struct.Fields {
			if f.Value != nil {
				if err := normalizeExpr(f.Value); err != nil {
					return err
				}
			}
		}
	case p.FunExpr != nil:
		for _, st := range p.FunExpr.BlockBody {
			if err := normalizeStatement(st); err != nil {
				return err
			}
		}
		if p.FunExpr.ExprBody != nil {
			return normalizeExpr(p.FunExpr.ExprBody)
		}
	case p.Query != nil:
		return normalizeQueryExpr(p.Query)
	case p.Load != nil:
		if err := validateLoadSavePath(p.Load, nil); err != nil {
			return err
		}
		if p.Load.With != nil {
			return normalizeExpr(p.Load.With)
		}
	case p.Save != nil:
		if err := validateLoadSavePath(nil, p.Save); err != nil {
			return err
		}
		if p.Save.Src != nil {
			if err := normalizeExpr(p.Save.Src); err != nil {
				return err
			}
		}
		if p.Save.With != nil {
			return normalizeExpr(p.Save.With)
		}
	}
	return nil
}

func normalizeIfExpr(e *IfExpr) error {
	if e == nil {
		return nil
	}
	if e.Cond != nil {
		if err := normalizeExpr(e.Cond); err != nil {
			return err
		}
	}
	if e.Then != nil {
		if err := normalizeExpr(e.Then); err != nil {
			return err
		}
	}
	if e.ElseIf != nil {
		if err := normalizeIfExpr(e.ElseIf); err != nil {
			return err
		}
	}
	if e.Else != nil {
		if err := normalizeExpr(e.Else); err != nil {
			return err
		}
	}
	return nil
}

func normalizeQueryExpr(q *QueryExpr) error {
	if q == nil {
		return nil
	}
	if q.Source != nil {
		if err := normalizeExpr(q.Source); err != nil {
			return err
		}
	}
	for _, f := range q.Froms {
		if f.Src != nil {
			if err := normalizeExpr(f.Src); err != nil {
				return err
			}
		}
	}
	for _, j := range q.Joins {
		if j.Src != nil {
			if err := normalizeExpr(j.Src); err != nil {
				return err
			}
		}
		if j.On != nil {
			if err := normalizeExpr(j.On); err != nil {
				return err
			}
		}
	}
	if q.Where != nil {
		if err := normalizeExpr(q.Where); err != nil {
			return err
		}
	}
	if q.Sort != nil {
		if err := normalizeExpr(q.Sort); err != nil {
			return err
		}
	}
	if q.Skip != nil {
		if err := normalizeExpr(q.Skip); err != nil {
			return err
		}
	}
	if q.Take != nil {
		if err := normalizeExpr(q.Take); err != nil {
			return err
		}
	}
	if q.Select != nil {
		if err := normalizeExpr(q.Select); err != nil {
			return err
		}
	}
	return nil
}
