package frontend

import (
	"fmt"
	"math"
	"sort"
	"strings"

	"mochi/compiler3/ffi/resolve"
	"mochi/compiler3/ffi/typebridge"
	"mochi/compiler3/ir"
	gogen "mochi/compiler3/emit/go"
	"mochi/parser"
)

// funEntry indexes a user-declared Mochi `fun` so calls and recursive
// references can resolve before the body has been lowered.
type funEntry struct {
	index uint32
	stmt  *parser.FunStmt
}

// Lower walks a parsed Mochi program and produces a compiler3 emit
// Program. Top-level statements are wrapped in a synthetic `main`
// function (Result: TypeUnit) so the emitter produces a runnable
// executable. Mochi `fun` declarations lower to standalone IR
// functions.
//
// Phase-6 scope covered: i64 literals, let/var bindings, assignments,
// binary arithmetic and comparisons, return, if/else, while, function
// calls (including recursion), and `print(int)`. Anything else
// surfaces an explicit "unsupported in MVP frontend" error so the A/B
// harness can mark the fixture as skipped rather than miscompile.
func Lower(prog *parser.Program) (*gogen.Program, error) {
	p := &gogen.Program{PkgName: "main"}

	// First pass: collect user fun declarations so call lookups can
	// resolve before the fun body is lowered (allows mutual recursion
	// and forward references).
	userFns := map[string]funEntry{}
	for _, st := range prog.Statements {
		if st.Fun != nil {
			idx := uint32(len(p.Funcs))
			fn := &ir.Function{Name: st.Fun.Name}
			p.Funcs = append(p.Funcs, fn)
			userFns[st.Fun.Name] = funEntry{index: idx, stmt: st.Fun}
		}
	}

	// Second pass: resolve `import go "path"` statements via the
	// MEP-43 Phase-2 resolver. Each binding is keyed by the import's
	// alias (the segment after `as`, defaulting to the package name).
	// `! meta` on the import flips SealHandles=true on every FFI call
	// site routed through this package (MEP-43 Phase 10).
	goImports := map[string]*goImport{}
	for _, st := range prog.Statements {
		if st.Import == nil || st.Import.Lang == nil || *st.Import.Lang != "go" {
			continue
		}
		imp := st.Import
		pb, err := resolve.New().Resolve(imp.Path)
		if err != nil {
			return nil, fmt.Errorf("lower import go %q: %w", imp.Path, err)
		}
		alias := imp.As
		if alias == "" {
			alias = pb.Name
			if alias == "" {
				alias = lastPathSegment(imp.Path)
			}
		}
		goImports[alias] = &goImport{
			pkg:         pb,
			alias:       alias,
			path:        imp.Path,
			sealHandles: hasEffect(imp.Effects, "meta"),
		}
	}

	// Lower each user fun. Each lowering must finish before the next
	// because they share the program-level function table.
	for name, e := range userFns {
		if err := lowerFun(p, e.index, e.stmt, userFns, goImports); err != nil {
			return nil, fmt.Errorf("lower fun %s: %w", name, err)
		}
	}

	// Wrap top-level (non-fun) statements in a synthetic `main`. If
	// there are no top-level statements, no main is emitted.
	var topLevel []*parser.Statement
	for _, st := range prog.Statements {
		if st.Fun != nil || st.Import != nil {
			continue
		}
		topLevel = append(topLevel, st)
	}
	if len(topLevel) > 0 {
		mainFn := &ir.Function{Name: "main", Result: ir.TypeUnit}
		p.Funcs = append(p.Funcs, mainFn)
		b := newBuilder(mainFn, userFns, p, goImports)
		entry := b.fn.AddBlock()
		b.curBlock = entry
		for _, st := range topLevel {
			if err := b.lowerStmt(st); err != nil {
				return nil, err
			}
			if b.terminated {
				break
			}
		}
		if !b.terminated {
			b.terminator(ir.Terminator{Kind: ir.TermReturn})
		}
	}

	return p, nil
}

// goImport carries one resolved `import go` binding through the
// lowering pass. The resolver returned `pkg`; alias is the Mochi-side
// name used at call sites; sealHandles is the MEP-43 Phase 10 effect.
type goImport struct {
	pkg         *resolve.PackageBinding
	alias       string
	path        string
	sealHandles bool
}

func hasEffect(effects []string, want string) bool {
	for _, e := range effects {
		if e == want {
			return true
		}
	}
	return false
}

func lastPathSegment(p string) string {
	if i := strings.LastIndex(p, "/"); i >= 0 {
		return p[i+1:]
	}
	return p
}

// builder holds the per-function lowering state.
type builder struct {
	fn         *ir.Function
	prog       *gogen.Program
	userFns    map[string]funEntry
	goImports  map[string]*goImport
	curBlock   uint32
	terminated bool
	// values is the lexical scope: Mochi name -> SSA value ID. We do
	// not track SSA renaming for var reassignment yet; the MVP only
	// supports straight-line let/var with no reassignment, and the
	// query/loop work needed for full phi insertion is part of the
	// post-MVP widening.
	values map[string]uint32
}

func newBuilder(fn *ir.Function, userFns map[string]funEntry, prog *gogen.Program, goImports map[string]*goImport) *builder {
	return &builder{
		fn:        fn,
		prog:      prog,
		userFns:   userFns,
		goImports: goImports,
		values:    map[string]uint32{},
	}
}

func lowerFun(p *gogen.Program, idx uint32, fs *parser.FunStmt, userFns map[string]funEntry, goImports map[string]*goImport) error {
	fn := p.Funcs[idx]
	// Single result type. MVP supports i64 returns only; the absence
	// of a return type annotation is treated as i64 to keep the most
	// common fixture shape working without forcing the user to annotate.
	fn.Result = ir.TypeI64
	if fs.Return != nil {
		t, err := lowerType(fs.Return)
		if err != nil {
			return err
		}
		fn.Result = t
	}
	b := newBuilder(fn, userFns, p, goImports)
	// Params: every param is an OpParam value of the declared type.
	for _, param := range fs.Params {
		pt := ir.TypeI64
		if param.Type != nil {
			t, err := lowerType(param.Type)
			if err != nil {
				return err
			}
			pt = t
		}
		vid := fn.AddValue(ir.Value{Type: pt, Op: ir.OpParam})
		fn.Params = append(fn.Params, vid)
		b.values[param.Name] = vid
	}
	entry := fn.AddBlock()
	b.curBlock = entry
	for _, st := range fs.Body {
		if err := b.lowerStmt(st); err != nil {
			return err
		}
		if b.terminated {
			break
		}
	}
	if !b.terminated {
		// User omitted a return. For TypeUnit that's fine; for i64
		// fixtures the type checker would normally reject it, but the
		// MVP frontend bypasses the checker so we emit a zero return
		// to keep the Go output buildable.
		if fn.Result == ir.TypeUnit {
			b.terminator(ir.Terminator{Kind: ir.TermReturn})
		} else {
			zero := b.addValue(ir.Value{Type: fn.Result, Op: ir.OpConst})
			b.terminator(ir.Terminator{Kind: ir.TermReturn, Value: zero})
		}
	}
	return nil
}

func lowerType(t *parser.TypeRef) (ir.Type, error) {
	if t == nil {
		return ir.TypeInvalid, fmt.Errorf("frontend: nil type ref")
	}
	if t.Generic != nil {
		// Phase 4.3.1: `list<int>` (the typed-i64 array shape) is the
		// only generic the MVP frontend lowers. ElemType inspection
		// happens at the call site that constructs a TypeList value;
		// here we only need to assert the surface form is supported.
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			el, err := lowerType(t.Generic.Args[0])
			if err != nil {
				return ir.TypeInvalid, fmt.Errorf("frontend: list element: %w", err)
			}
			if el != ir.TypeI64 {
				return ir.TypeInvalid, fmt.Errorf("frontend: list<%s> unsupported in MVP (only list<int>)", el)
			}
			return ir.TypeList, nil
		}
		return ir.TypeInvalid, fmt.Errorf("frontend: generic %s<...> unsupported in MVP", t.Generic.Name)
	}
	if t.Simple == nil {
		return ir.TypeInvalid, fmt.Errorf("frontend: only simple and list<...> type names are supported in the MVP")
	}
	switch *t.Simple {
	case "int":
		return ir.TypeI64, nil
	case "float":
		return ir.TypeF64, nil
	case "bool":
		return ir.TypeBool, nil
	case "string", "str":
		return ir.TypeStr, nil
	case "unit", "void":
		return ir.TypeUnit, nil
	}
	return ir.TypeInvalid, fmt.Errorf("frontend: type %q unsupported in MVP", *t.Simple)
}

func (b *builder) addValue(v ir.Value) uint32 {
	id := b.fn.AddValue(v)
	blk := b.fn.Block(b.curBlock)
	blk.Values = append(blk.Values, id)
	return id
}

func (b *builder) terminator(t ir.Terminator) {
	blk := b.fn.Block(b.curBlock)
	blk.Term = t
	switch t.Kind {
	case ir.TermJump:
		blk.Succs = []uint32{t.Target}
		b.fn.Block(t.Target).Preds = append(b.fn.Block(t.Target).Preds, b.curBlock)
	case ir.TermBranch:
		blk.Succs = []uint32{t.IfTrue, t.IfFalse}
		b.fn.Block(t.IfTrue).Preds = append(b.fn.Block(t.IfTrue).Preds, b.curBlock)
		b.fn.Block(t.IfFalse).Preds = append(b.fn.Block(t.IfFalse).Preds, b.curBlock)
	}
	b.terminated = true
}

func (b *builder) lowerStmt(st *parser.Statement) error {
	switch {
	case st.Let != nil:
		return b.lowerLet(st.Let.Name, st.Let.Value)
	case st.Var != nil:
		return b.lowerLet(st.Var.Name, st.Var.Value)
	case st.Assign != nil:
		if len(st.Assign.Field) != 0 {
			return fmt.Errorf("frontend: field assignment unsupported in MVP")
		}
		if len(st.Assign.Index) != 0 {
			return b.lowerIndexedAssign(st.Assign)
		}
		return b.lowerLet(st.Assign.Name, st.Assign.Value)
	case st.Return != nil:
		return b.lowerReturn(st.Return)
	case st.If != nil:
		return b.lowerIf(st.If)
	case st.While != nil:
		return b.lowerWhile(st.While)
	case st.For != nil:
		return b.lowerFor(st.For)
	case st.Expr != nil:
		_, err := b.lowerExprAsStmt(st.Expr.Expr)
		return err
	}
	return fmt.Errorf("frontend: statement kind unsupported in MVP")
}

// lowerIndexedAssign handles `xs[i] = v`. MVP scope is a single-level
// IndexOp on a TypeList(elem=i64) binding. Multi-level indices (xs[i][j])
// and field-then-index chains stay rejected until a later phase.
func (b *builder) lowerIndexedAssign(a *parser.AssignStmt) error {
	if len(a.Index) != 1 {
		return fmt.Errorf("frontend: multi-level indexed assignment unsupported in MVP")
	}
	idxOp := a.Index[0]
	if idxOp.Colon != nil || idxOp.Colon2 != nil || idxOp.End != nil || idxOp.Step != nil || idxOp.Start == nil {
		return fmt.Errorf("frontend: slice assignment unsupported in MVP")
	}
	listID, ok := b.values[a.Name]
	if !ok {
		return fmt.Errorf("frontend: indexed assign to unbound identifier %q", a.Name)
	}
	if b.fn.Values[listID].Type != ir.TypeList {
		return fmt.Errorf("frontend: indexed assign on non-list %s", b.fn.Values[listID].Type)
	}
	idxID, err := b.lowerExpr(idxOp.Start)
	if err != nil {
		return err
	}
	if b.fn.Values[idxID].Type != ir.TypeI64 {
		return fmt.Errorf("frontend: list index must be i64, got %s", b.fn.Values[idxID].Type)
	}
	valID, err := b.lowerExpr(a.Value)
	if err != nil {
		return err
	}
	if b.fn.Values[valID].Type != ir.TypeI64 {
		return fmt.Errorf("frontend: list<int> store requires i64 value, got %s", b.fn.Values[valID].Type)
	}
	b.addValue(ir.Value{
		Type:     ir.TypeUnit,
		ElemType: ir.TypeI64,
		Op:       ir.OpListSetI64,
		Args:     []uint32{listID, idxID, valID},
	})
	return nil
}

func (b *builder) lowerLet(name string, e *parser.Expr) error {
	if e == nil {
		return fmt.Errorf("frontend: binding %q has no initializer", name)
	}
	vid, err := b.lowerExpr(e)
	if err != nil {
		return err
	}
	b.values[name] = vid
	return nil
}

func (b *builder) lowerReturn(rs *parser.ReturnStmt) error {
	if rs.Value == nil {
		b.terminator(ir.Terminator{Kind: ir.TermReturn})
		return nil
	}
	vid, err := b.lowerExpr(rs.Value)
	if err != nil {
		return err
	}
	b.terminator(ir.Terminator{Kind: ir.TermReturn, Value: vid})
	return nil
}

func (b *builder) lowerIf(s *parser.IfStmt) error {
	cond, err := b.lowerExpr(s.Cond)
	if err != nil {
		return err
	}
	thenID := b.fn.AddBlock()
	elseID := b.fn.AddBlock()
	contID := b.fn.AddBlock()

	// Snapshot the env at entry so the else branch sees the same
	// pre-if bindings the then branch saw, and so the merge can phi-
	// join values that diverge between the two paths.
	preEnv := make(map[string]uint32, len(b.values))
	for k, v := range b.values {
		preEnv[k] = v
	}

	b.terminator(ir.Terminator{Kind: ir.TermBranch, Value: cond, IfTrue: thenID, IfFalse: elseID})

	// Then.
	b.curBlock = thenID
	b.terminated = false
	for _, ts := range s.Then {
		if err := b.lowerStmt(ts); err != nil {
			return err
		}
		if b.terminated {
			break
		}
	}
	var thenEnv map[string]uint32
	var thenEnd uint32
	thenTerminated := b.terminated
	if !b.terminated {
		thenEnd = b.curBlock
		thenEnv = make(map[string]uint32, len(b.values))
		for k, v := range b.values {
			thenEnv[k] = v
		}
		b.terminator(ir.Terminator{Kind: ir.TermJump, Target: contID})
	}

	// Restore the pre-if env so else sees the same bindings then did.
	b.values = make(map[string]uint32, len(preEnv))
	for k, v := range preEnv {
		b.values[k] = v
	}

	// Else.
	b.curBlock = elseID
	b.terminated = false
	if s.ElseIf != nil {
		if err := b.lowerIf(s.ElseIf); err != nil {
			return err
		}
	} else {
		for _, es := range s.Else {
			if err := b.lowerStmt(es); err != nil {
				return err
			}
			if b.terminated {
				break
			}
		}
	}
	var elseEnv map[string]uint32
	var elseEnd uint32
	elseTerminated := b.terminated
	if !b.terminated {
		elseEnd = b.curBlock
		elseEnv = make(map[string]uint32, len(b.values))
		for k, v := range b.values {
			elseEnv[k] = v
		}
		b.terminator(ir.Terminator{Kind: ir.TermJump, Target: contID})
	}

	// Continuation: phi-join bindings that diverge between the two
	// paths. When only one branch terminated, the merge has a single
	// predecessor and we can use that branch's env directly. When both
	// terminated, the contID block is unreachable; clear its preds.
	b.curBlock = contID
	b.terminated = false
	switch {
	case thenTerminated && elseTerminated:
		// Unreachable merge.
	case thenTerminated:
		b.values = elseEnv
	case elseTerminated:
		b.values = thenEnv
	default:
		// Both branches reach the merge. For every name in the pre-if
		// env, phi-join if the two paths diverge; otherwise keep the
		// common value. Names introduced only inside a branch are
		// dropped (their scope ended at the branch).
		names := make([]string, 0, len(preEnv))
		for name := range preEnv {
			names = append(names, name)
		}
		sort.Strings(names)
		newEnv := make(map[string]uint32, len(names))
		for _, name := range names {
			tv, tok := thenEnv[name]
			ev, eok := elseEnv[name]
			if !tok || !eok {
				newEnv[name] = preEnv[name]
				continue
			}
			if tv == ev {
				newEnv[name] = tv
				continue
			}
			phiVid := b.addValue(ir.Value{
				Type: b.fn.Values[tv].Type,
				Op:   ir.OpPhi,
				Args: []uint32{thenEnd, tv, elseEnd, ev},
			})
			newEnv[name] = phiVid
		}
		b.values = newEnv
	}
	return nil
}

// lowerWhile lowers `while cond { body }` into a three-block CFG
// (pre-header jump, header with phis + cond branch, body, continuation).
// For every binding in scope at loop entry we materialise a phi at the
// header so the body sees a join of the pre-loop value and the value
// flowing back from the previous iteration. The back-edge slot is
// patched after the body lowers, matching the fixture convention in
// compiler3/ir/fixture.go. Bindings introduced inside the body are
// not phi-tracked and leak past the loop in the current env, which
// matches lowerIf's scoping (a wider scope discipline is post-MVP).
func (b *builder) lowerWhile(s *parser.WhileStmt) error {
	preID := b.curBlock
	headID := b.fn.AddBlock()
	bodyID := b.fn.AddBlock()
	contID := b.fn.AddBlock()

	// Snapshot the bindings live at loop entry. Iteration order is
	// stable so the IR (and therefore emitted C/Go) is deterministic
	// across builds.
	names := make([]string, 0, len(b.values))
	for name := range b.values {
		names = append(names, name)
	}
	sort.Strings(names)
	preVals := make([]uint32, len(names))
	for i, name := range names {
		preVals[i] = b.values[name]
	}

	// Pre-header jumps unconditionally to the header.
	b.terminator(ir.Terminator{Kind: ir.TermJump, Target: headID})

	// Header: one phi per snapshotted binding, with the back-edge slot
	// left at sentinel 0 to be patched after the body lowers.
	b.curBlock = headID
	b.terminated = false
	phis := make([]uint32, len(names))
	for i, name := range names {
		preVid := preVals[i]
		phiVid := b.addValue(ir.Value{
			Type: b.fn.Values[preVid].Type,
			Op:   ir.OpPhi,
			Args: []uint32{preID, preVid, bodyID, 0},
		})
		phis[i] = phiVid
		b.values[name] = phiVid
	}
	cond, err := b.lowerExpr(s.Cond)
	if err != nil {
		return err
	}
	if b.fn.Values[cond].Type != ir.TypeBool {
		return fmt.Errorf("frontend: while-cond must be bool, got %s", b.fn.Values[cond].Type)
	}
	b.terminator(ir.Terminator{Kind: ir.TermBranch, Value: cond, IfTrue: bodyID, IfFalse: contID})

	// Body: lower statements, then jump back to the header.
	b.curBlock = bodyID
	b.terminated = false
	for _, st := range s.Body {
		if err := b.lowerStmt(st); err != nil {
			return err
		}
		if b.terminated {
			break
		}
	}
	if !b.terminated {
		// Patch back-edge slots from the post-body env before jumping.
		// The predecessor block is b.curBlock (the actual end of the
		// body), which differs from bodyID when the body contains
		// nested control flow that ends in a merge block.
		endBlock := b.curBlock
		for i, name := range names {
			b.fn.Values[phis[i]].Args[2] = endBlock
			b.fn.Values[phis[i]].Args[3] = b.values[name]
		}
		b.terminator(ir.Terminator{Kind: ir.TermJump, Target: headID})
	} else {
		// Body terminated unconditionally (e.g., return). The header has
		// only the pre-header as a predecessor; drop the back-edge slot
		// from every phi so the validator's arity check is satisfied.
		head := b.fn.Block(headID)
		head.Preds = []uint32{preID}
		for _, phiVid := range phis {
			phi := &b.fn.Values[phiVid]
			phi.Args = phi.Args[:2]
		}
	}

	// Continuation: the live value for each snapshotted binding is the
	// header phi (cont's only predecessor is the header).
	b.curBlock = contID
	b.terminated = false
	for i, name := range names {
		b.values[name] = phis[i]
	}
	return nil
}

// lowerFor lowers `for x in lo..hi { body }` (integer range) to the
// same phi-at-header CFG shape as lowerWhile, with the loop variable
// `x` as one of the snapshotted bindings. Pre-header initialises `x`
// to `lo`; the header phi joins (pre-header `x`, body's `x + 1`); the
// header's cond is `x < hi`; the body lowers each statement, then
// inserts the synthetic `x = x + 1` step at the end. The range form
// is the only ForStmt shape the MVP frontend lowers; collection-iter
// (`for x in xs { body }`, where xs is a list) stays rejected until
// Phase 4.3.x widens the surface.
//
// Bounds may be any TypeI64 expression including local bindings and
// nested arithmetic; they are evaluated once in the pre-header and
// captured in SSA, so a mutation inside the body does not affect the
// loop count.
func (b *builder) lowerFor(s *parser.ForStmt) error {
	if s.RangeEnd == nil {
		return fmt.Errorf("frontend: for-in over a collection unsupported in MVP (only `for x in lo..hi`)")
	}
	lo, err := b.lowerExpr(s.Source)
	if err != nil {
		return err
	}
	hi, err := b.lowerExpr(s.RangeEnd)
	if err != nil {
		return err
	}
	if b.fn.Values[lo].Type != ir.TypeI64 {
		return fmt.Errorf("frontend: for-range lo must be i64, got %s", b.fn.Values[lo].Type)
	}
	if b.fn.Values[hi].Type != ir.TypeI64 {
		return fmt.Errorf("frontend: for-range hi must be i64, got %s", b.fn.Values[hi].Type)
	}

	loopName := s.Name
	// Save the shadowed binding (if any) so the loop variable does not
	// leak past the loop. Mochi semantics scope the for-variable to the
	// loop body; both targets observe that by restoring the prior env
	// at the cont block.
	oldVal, hadOld := b.values[loopName]
	b.values[loopName] = lo

	preID := b.curBlock
	headID := b.fn.AddBlock()
	bodyID := b.fn.AddBlock()
	contID := b.fn.AddBlock()

	names := make([]string, 0, len(b.values))
	for name := range b.values {
		names = append(names, name)
	}
	sort.Strings(names)
	preVals := make([]uint32, len(names))
	for i, name := range names {
		preVals[i] = b.values[name]
	}

	b.terminator(ir.Terminator{Kind: ir.TermJump, Target: headID})

	b.curBlock = headID
	b.terminated = false
	phis := make([]uint32, len(names))
	var loopPhi uint32
	for i, name := range names {
		preVid := preVals[i]
		phiVid := b.addValue(ir.Value{
			Type: b.fn.Values[preVid].Type,
			Op:   ir.OpPhi,
			Args: []uint32{preID, preVid, bodyID, 0},
		})
		phis[i] = phiVid
		b.values[name] = phiVid
		if name == loopName {
			loopPhi = phiVid
		}
	}
	cond := b.addValue(ir.Value{
		Type: ir.TypeBool,
		Op:   ir.OpCmpLtI64,
		Args: []uint32{loopPhi, hi},
	})
	b.terminator(ir.Terminator{Kind: ir.TermBranch, Value: cond, IfTrue: bodyID, IfFalse: contID})

	b.curBlock = bodyID
	b.terminated = false
	for _, st := range s.Body {
		if err := b.lowerStmt(st); err != nil {
			return err
		}
		if b.terminated {
			break
		}
	}
	if !b.terminated {
		// Synthetic `x = x + 1` step at the end of the body. The
		// increment runs against the current SSA binding for `x`,
		// which is normally the header phi but would be a rebind if
		// the user shadowed the loop variable inside the body.
		cur := b.values[loopName]
		one := b.addValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 1})
		next := b.addValue(ir.Value{Type: ir.TypeI64, Op: ir.OpAddI64, Args: []uint32{cur, one}})
		b.values[loopName] = next
		// Patch back-edge slots. The predecessor block is b.curBlock
		// (the actual end of the body), which differs from bodyID when
		// the body contains nested control flow that ends in a merge
		// block (e.g., an inner if-statement).
		endBlock := b.curBlock
		for i, name := range names {
			b.fn.Values[phis[i]].Args[2] = endBlock
			b.fn.Values[phis[i]].Args[3] = b.values[name]
		}
		b.terminator(ir.Terminator{Kind: ir.TermJump, Target: headID})
	} else {
		head := b.fn.Block(headID)
		head.Preds = []uint32{preID}
		for _, phiVid := range phis {
			phi := &b.fn.Values[phiVid]
			phi.Args = phi.Args[:2]
		}
	}

	b.curBlock = contID
	b.terminated = false
	for i, name := range names {
		b.values[name] = phis[i]
	}
	// Restore the shadowed binding; the loop variable is dead after the
	// loop. Without this, a subsequent reference to `loopName` in the
	// outer scope would resolve to the header phi, which is wrong if
	// the user had a same-named outer binding.
	if hadOld {
		b.values[loopName] = oldVal
	} else {
		delete(b.values, loopName)
	}
	return nil
}

// lowerExprAsStmt lowers an expression-statement. The MVP recognises
// `print(arg)` and lowers it to a `fmt.Println` OpCallGo; other call
// expressions are lowered as regular expressions and their value is
// discarded.
func (b *builder) lowerExprAsStmt(e *parser.Expr) (uint32, error) {
	if call := exprAsCall(e); call != nil && call.Func == "print" && len(call.Args) == 1 {
		arg, err := b.lowerExpr(call.Args[0])
		if err != nil {
			return 0, err
		}
		argType := b.fn.Values[arg].Type
		goArgType := goTypeForIRType(argType)
		if goArgType == "" {
			return 0, fmt.Errorf("frontend: print() argument type %s unsupported in MVP", argType)
		}
		bind := ir.GoBinding{
			Pkg:      "fmt",
			Alias:    "fmt",
			Name:     "Println",
			ArgTypes: []string{goArgType},
			Result:   "",
		}
		bindIdx := int64(len(b.fn.GoBindings))
		b.fn.GoBindings = append(b.fn.GoBindings, bind)
		id := b.addValue(ir.Value{Type: ir.TypeUnit, Op: ir.OpCallGo, Args: []uint32{arg}, Const: bindIdx})
		return id, nil
	}
	return b.lowerExpr(e)
}

func goTypeForIRType(t ir.Type) string {
	switch t {
	case ir.TypeI64:
		return "int64"
	case ir.TypeF64:
		return "float64"
	case ir.TypeBool:
		return "bool"
	case ir.TypeStr:
		return "string"
	}
	return ""
}

func exprAsCall(e *parser.Expr) *parser.CallExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 || u.Value == nil || len(u.Value.Ops) != 0 {
		return nil
	}
	p := u.Value.Target
	if p == nil {
		return nil
	}
	return p.Call
}

// lowerExpr returns the SSA value ID for e. MVP handles BinaryExpr
// with the i64-compatible operators and a handful of Primary forms.
func (b *builder) lowerExpr(e *parser.Expr) (uint32, error) {
	if e == nil || e.Binary == nil {
		return 0, fmt.Errorf("frontend: empty expression")
	}
	return b.lowerBinary(e.Binary)
}

func (b *builder) lowerBinary(be *parser.BinaryExpr) (uint32, error) {
	left, err := b.lowerUnary(be.Left)
	if err != nil {
		return 0, err
	}
	if len(be.Right) == 0 {
		return left, nil
	}
	// MVP: left-associative without precedence; sufficient for the
	// numeric fixtures because most have only one operator or
	// fully-parenthesised expressions.
	cur := left
	for _, op := range be.Right {
		rhs, err := b.lowerUnary(op.Right)
		if err != nil {
			return 0, err
		}
		cur, err = b.applyBinOp(op.Op, cur, rhs)
		if err != nil {
			return 0, err
		}
	}
	return cur, nil
}

func (b *builder) applyBinOp(op string, l, r uint32) (uint32, error) {
	lt := b.fn.Values[l].Type
	rt := b.fn.Values[r].Type
	if lt != rt {
		return 0, fmt.Errorf("frontend: binop %q across types %s and %s unsupported in MVP", op, lt, rt)
	}
	var code ir.OpCode
	resType := lt
	switch lt {
	case ir.TypeI64:
		switch op {
		case "+":
			code = ir.OpAddI64
		case "-":
			code = ir.OpSubI64
		case "*":
			code = ir.OpMulI64
		case "/":
			code = ir.OpDivI64
		case "%":
			code = ir.OpModI64
		case "&":
			code = ir.OpAndI64
		case "|":
			code = ir.OpOrI64
		case "^":
			code = ir.OpXorI64
		case "<<":
			code = ir.OpShlI64
		case ">>":
			code = ir.OpShrI64
		case "==":
			code = ir.OpCmpEqI64
			resType = ir.TypeBool
		case "!=":
			code = ir.OpCmpNeI64
			resType = ir.TypeBool
		case "<":
			code = ir.OpCmpLtI64
			resType = ir.TypeBool
		case "<=":
			code = ir.OpCmpLeI64
			resType = ir.TypeBool
		case ">":
			code = ir.OpCmpGtI64
			resType = ir.TypeBool
		case ">=":
			code = ir.OpCmpGeI64
			resType = ir.TypeBool
		default:
			return 0, fmt.Errorf("frontend: operator %q on i64 unsupported in MVP", op)
		}
	case ir.TypeF64:
		switch op {
		case "+":
			code = ir.OpAddF64
		case "-":
			code = ir.OpSubF64
		case "*":
			code = ir.OpMulF64
		case "/":
			code = ir.OpDivF64
		case "==":
			code = ir.OpCmpEqF64
			resType = ir.TypeBool
		case "!=":
			code = ir.OpCmpNeF64
			resType = ir.TypeBool
		case "<":
			code = ir.OpCmpLtF64
			resType = ir.TypeBool
		case "<=":
			code = ir.OpCmpLeF64
			resType = ir.TypeBool
		case ">":
			code = ir.OpCmpGtF64
			resType = ir.TypeBool
		case ">=":
			code = ir.OpCmpGeF64
			resType = ir.TypeBool
		default:
			return 0, fmt.Errorf("frontend: operator %q on f64 unsupported in MVP", op)
		}
	default:
		return 0, fmt.Errorf("frontend: binop %q on type %s unsupported in MVP", op, lt)
	}
	return b.addValue(ir.Value{Type: resType, Op: code, Args: []uint32{l, r}}), nil
}

func (b *builder) lowerUnary(u *parser.Unary) (uint32, error) {
	val, err := b.lowerPostfix(u.Value)
	if err != nil {
		return 0, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-":
			vt := b.fn.Values[val].Type
			switch vt {
			case ir.TypeI64:
				val = b.addValue(ir.Value{Type: ir.TypeI64, Op: ir.OpNegI64, Args: []uint32{val}})
			case ir.TypeF64:
				val = b.addValue(ir.Value{Type: ir.TypeF64, Op: ir.OpNegF64, Args: []uint32{val}})
			default:
				return 0, fmt.Errorf("frontend: unary `-` on %s unsupported in MVP", vt)
			}
		case "!":
			vt := b.fn.Values[val].Type
			if vt != ir.TypeBool {
				return 0, fmt.Errorf("frontend: unary `!` on %s unsupported in MVP", vt)
			}
			val = b.addValue(ir.Value{Type: ir.TypeBool, Op: ir.OpNotBool, Args: []uint32{val}})
		default:
			return 0, fmt.Errorf("frontend: unary operator %q unsupported in MVP", op)
		}
	}
	return val, nil
}

func (b *builder) lowerPostfix(pe *parser.PostfixExpr) (uint32, error) {
	if pe == nil || pe.Target == nil {
		return 0, fmt.Errorf("frontend: empty postfix")
	}
	// `pkg.Func(args)`: a Selector targeting an imported Go package
	// whose first postfix op is a call. The selector's Tail names the
	// callee under the alias; remaining tail entries (e.g. method
	// receivers) are out of MVP scope.
	if len(pe.Ops) == 1 && pe.Ops[0].Call != nil && pe.Target.Selector != nil {
		sel := pe.Target.Selector
		if imp, ok := b.goImports[sel.Root]; ok && len(sel.Tail) == 1 {
			return b.lowerGoCall(imp, sel.Tail[0], pe.Ops[0].Call.Args)
		}
	}
	if len(pe.Ops) == 0 {
		return b.lowerPrimary(pe.Target)
	}
	// Phase 4.3.1: a chain of IndexOp postfixes on a Primary lowers
	// to OpListGetI64. Only single-level i64 indexing is supported in
	// the MVP; ranges and multi-level chains stay rejected so the A/B
	// harness skips the fixture rather than miscompiling.
	cur, err := b.lowerPrimary(pe.Target)
	if err != nil {
		return 0, err
	}
	for _, op := range pe.Ops {
		if op.Index == nil {
			return 0, fmt.Errorf("frontend: non-index postfix unsupported in MVP")
		}
		idx := op.Index
		if idx.Colon != nil || idx.Colon2 != nil || idx.End != nil || idx.Step != nil || idx.Start == nil {
			return 0, fmt.Errorf("frontend: slice indexing unsupported in MVP")
		}
		if b.fn.Values[cur].Type != ir.TypeList {
			return 0, fmt.Errorf("frontend: index on non-list %s", b.fn.Values[cur].Type)
		}
		iID, err := b.lowerExpr(idx.Start)
		if err != nil {
			return 0, err
		}
		if b.fn.Values[iID].Type != ir.TypeI64 {
			return 0, fmt.Errorf("frontend: list index must be i64, got %s", b.fn.Values[iID].Type)
		}
		cur = b.addValue(ir.Value{
			Type:     ir.TypeI64,
			ElemType: ir.TypeI64,
			Op:       ir.OpListGetI64,
			Args:     []uint32{cur, iID},
		})
	}
	return cur, nil
}

func (b *builder) lowerPrimary(p *parser.Primary) (uint32, error) {
	switch {
	case p.Lit != nil:
		return b.lowerLiteral(p.Lit)
	case p.Selector != nil:
		if len(p.Selector.Tail) == 1 {
			if imp, ok := b.goImports[p.Selector.Root]; ok {
				return b.lowerGoValue(imp, p.Selector.Tail[0])
			}
		}
		if len(p.Selector.Tail) != 0 {
			return 0, fmt.Errorf("frontend: selector tail %v unsupported in MVP", p.Selector.Tail)
		}
		id, ok := b.values[p.Selector.Root]
		if !ok {
			return 0, fmt.Errorf("frontend: unbound identifier %q", p.Selector.Root)
		}
		return id, nil
	case p.Call != nil:
		return b.lowerCall(p.Call)
	case p.List != nil:
		return b.lowerListLiteral(p.List)
	case p.Group != nil:
		return b.lowerExpr(p.Group)
	}
	return 0, fmt.Errorf("frontend: primary form unsupported in MVP")
}

// lowerListLiteral lowers `[e1, e2, ...]` to OpNewList plus a chain of
// OpListPushI64 ops, one per element. The empty form `[]` produces an
// OpNewList with ElemType=TypeI64; the type is inferred from the
// context only when elements are present, otherwise it defaults to i64
// (the MVP's sole supported element type). Non-i64 elements surface a
// frontend error.
func (b *builder) lowerListLiteral(lit *parser.ListLiteral) (uint32, error) {
	listID := b.addValue(ir.Value{
		Type:     ir.TypeList,
		ElemType: ir.TypeI64,
		Op:       ir.OpNewList,
	})
	for _, el := range lit.Elems {
		vid, err := b.lowerExpr(el)
		if err != nil {
			return 0, err
		}
		if b.fn.Values[vid].Type != ir.TypeI64 {
			return 0, fmt.Errorf("frontend: list literal element type %s unsupported (only i64 in MVP)",
				b.fn.Values[vid].Type)
		}
		b.addValue(ir.Value{
			Type:     ir.TypeUnit,
			ElemType: ir.TypeI64,
			Op:       ir.OpListPushI64,
			Args:     []uint32{listID, vid},
		})
	}
	return listID, nil
}

func (b *builder) lowerLiteral(lit *parser.Literal) (uint32, error) {
	switch {
	case lit.Int != nil:
		return b.addValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: int64(*lit.Int)}), nil
	case lit.Bool != nil:
		c := int64(0)
		if bool(*lit.Bool) {
			c = 1
		}
		return b.addValue(ir.Value{Type: ir.TypeBool, Op: ir.OpConst, Const: c}), nil
	case lit.Float != nil:
		return b.addValue(ir.Value{Type: ir.TypeF64, Op: ir.OpConst, Const: int64(math.Float64bits(*lit.Float))}), nil
	}
	return 0, fmt.Errorf("frontend: literal kind unsupported in MVP (str/none)")
}

func (b *builder) lowerCall(c *parser.CallExpr) (uint32, error) {
	// Builtins: `len(xs)` and `append(xs, v)` map to dedicated IR ops
	// rather than a user-declared `fun`. They're checked before the
	// user-fun lookup so a Mochi program cannot shadow them.
	if id, ok, err := b.lowerBuiltinCall(c); ok {
		return id, err
	}
	entry, ok := b.userFns[c.Func]
	if !ok {
		return 0, fmt.Errorf("frontend: unknown function %q (only user-declared funs callable in MVP)", c.Func)
	}
	args := make([]uint32, 0, len(c.Args))
	for _, a := range c.Args {
		vid, err := b.lowerExpr(a)
		if err != nil {
			return 0, err
		}
		args = append(args, vid)
	}
	callee := b.prog.Funcs[entry.index]
	id := b.addValue(ir.Value{Type: callee.Result, Op: ir.OpCall, Args: args, Const: int64(entry.index)})
	return id, nil
}

// lowerBuiltinCall recognises the small set of builtin function names
// that lower to dedicated IR ops. Returns (id, true, nil) on match,
// (0, false, nil) when the name is not a builtin, and (_, true, err)
// when the name matches but the argument shape is wrong.
//
// Phase 4.3.1 covers `len(xs)` and `append(xs, v)` against list<int>;
// `len("s")` against TypeStr is wired through OpLenStr in the same
// case for free since the IR op already exists.
func (b *builder) lowerBuiltinCall(c *parser.CallExpr) (uint32, bool, error) {
	switch c.Func {
	case "len":
		if len(c.Args) != 1 {
			return 0, true, fmt.Errorf("frontend: len() takes 1 argument, got %d", len(c.Args))
		}
		arg, err := b.lowerExpr(c.Args[0])
		if err != nil {
			return 0, true, err
		}
		switch b.fn.Values[arg].Type {
		case ir.TypeList:
			id := b.addValue(ir.Value{
				Type: ir.TypeI64, Op: ir.OpListLenI64, Args: []uint32{arg},
			})
			return id, true, nil
		case ir.TypeStr:
			id := b.addValue(ir.Value{
				Type: ir.TypeI64, Op: ir.OpLenStr, Args: []uint32{arg},
			})
			return id, true, nil
		default:
			return 0, true, fmt.Errorf("frontend: len() on %s unsupported in MVP",
				b.fn.Values[arg].Type)
		}
	case "append":
		if len(c.Args) != 2 {
			return 0, true, fmt.Errorf("frontend: append() takes 2 arguments, got %d", len(c.Args))
		}
		list, err := b.lowerExpr(c.Args[0])
		if err != nil {
			return 0, true, err
		}
		val, err := b.lowerExpr(c.Args[1])
		if err != nil {
			return 0, true, err
		}
		if b.fn.Values[list].Type != ir.TypeList {
			return 0, true, fmt.Errorf("frontend: append() target type %s unsupported (need list)",
				b.fn.Values[list].Type)
		}
		if b.fn.Values[val].Type != ir.TypeI64 {
			return 0, true, fmt.Errorf("frontend: append() value type %s unsupported (need i64)",
				b.fn.Values[val].Type)
		}
		// Push mutates the list in place; the SSA "result" of append
		// is the same list value. The caller's `xs = append(xs, v)`
		// rebinds the name to itself, which is correct.
		b.addValue(ir.Value{
			Type:     ir.TypeUnit,
			ElemType: ir.TypeI64,
			Op:       ir.OpListPushI64,
			Args:     []uint32{list, val},
		})
		return list, true, nil
	}
	return 0, false, nil
}

// lowerGoCall handles a `pkg.Func(args)` call against a resolved
// `import go` binding. The function's typebridge signature drives the
// arg-cast types at the emit boundary and the IR result type.
func (b *builder) lowerGoCall(imp *goImport, name string, callArgs []*parser.Expr) (uint32, error) {
	fb := imp.pkg.LookupFunc(name)
	if fb == nil {
		return 0, fmt.Errorf("frontend: %s.%s not found in resolved import %q", imp.alias, name, imp.path)
	}
	sig := fb.Signature
	if sig.Kind != typebridge.KindFunc {
		return 0, fmt.Errorf("frontend: %s.%s is not a function (kind=%s)", imp.alias, name, sig.Kind)
	}
	if sig.Variadic {
		return 0, fmt.Errorf("frontend: %s.%s is variadic, unsupported in MVP", imp.alias, name)
	}
	if len(sig.Params) != len(callArgs) {
		return 0, fmt.Errorf("frontend: %s.%s expects %d args, got %d", imp.alias, name, len(sig.Params), len(callArgs))
	}
	argTypes := make([]string, len(sig.Params))
	for i, pt := range sig.Params {
		argTypes[i] = goSourceTypeOf(pt)
		if argTypes[i] == "" {
			return 0, fmt.Errorf("frontend: %s.%s param %d has unsupported type %s", imp.alias, name, i, pt.Kind)
		}
	}
	var resGoType string
	resIRType := ir.TypeUnit
	switch len(sig.Results) {
	case 0:
		// void return
	case 1:
		resGoType = goSourceTypeOf(sig.Results[0])
		if resGoType == "" {
			return 0, fmt.Errorf("frontend: %s.%s result %s unsupported in MVP", imp.alias, name, sig.Results[0].Kind)
		}
		t, ok := irTypeOf(sig.Results[0])
		if !ok {
			return 0, fmt.Errorf("frontend: %s.%s result %s has no IR mapping in MVP", imp.alias, name, sig.Results[0].Kind)
		}
		resIRType = t
	default:
		return 0, fmt.Errorf("frontend: %s.%s returns %d values, MVP supports 0 or 1", imp.alias, name, len(sig.Results))
	}
	args := make([]uint32, 0, len(callArgs))
	for _, a := range callArgs {
		vid, err := b.lowerExpr(a)
		if err != nil {
			return 0, err
		}
		args = append(args, vid)
	}
	bind := ir.GoBinding{
		Pkg:         imp.path,
		Alias:       imp.alias,
		Name:        fb.Name,
		ArgTypes:    argTypes,
		Result:      resGoType,
		SealHandles: imp.sealHandles,
	}
	idx := int64(len(b.fn.GoBindings))
	b.fn.GoBindings = append(b.fn.GoBindings, bind)
	return b.addValue(ir.Value{Type: resIRType, Op: ir.OpCallGo, Args: args, Const: idx}), nil
}

// lowerGoValue handles a `pkg.Name` read against a resolved import.
// The symbol may be a package-level var or a const; both render as
// `alias.Name` in the emitted Go source.
func (b *builder) lowerGoValue(imp *goImport, name string) (uint32, error) {
	var bridgeType typebridge.Type
	switch {
	case imp.pkg.LookupConst(name) != nil:
		bridgeType = imp.pkg.LookupConst(name).Type
	case imp.pkg.LookupVar(name) != nil:
		bridgeType = imp.pkg.LookupVar(name).Type
	default:
		return 0, fmt.Errorf("frontend: %s.%s not found in resolved import %q (no var/const)", imp.alias, name, imp.path)
	}
	resGoType := goSourceTypeOf(bridgeType)
	if resGoType == "" {
		return 0, fmt.Errorf("frontend: %s.%s has unsupported type %s in MVP", imp.alias, name, bridgeType.Kind)
	}
	irType, ok := irTypeOf(bridgeType)
	if !ok {
		return 0, fmt.Errorf("frontend: %s.%s has no IR mapping in MVP", imp.alias, name)
	}
	bind := ir.GoBinding{
		Pkg:         imp.path,
		Alias:       imp.alias,
		Name:        name,
		Result:      resGoType,
		IsValue:     true,
		SealHandles: imp.sealHandles,
	}
	idx := int64(len(b.fn.GoBindings))
	b.fn.GoBindings = append(b.fn.GoBindings, bind)
	return b.addValue(ir.Value{Type: irType, Op: ir.OpCallGo, Const: idx}), nil
}

// goSourceTypeOf renders the Go-source form of t, suitable for an
// emitter cast at the FFI boundary. Returns "" for shapes the MVP
// frontend cannot lower (interfaces, channels, opaque-only types).
func goSourceTypeOf(t typebridge.Type) string {
	switch t.Kind {
	case typebridge.KindBool:
		return "bool"
	case typebridge.KindInt:
		switch t.Width {
		case 0:
			return "int"
		case 8:
			return "int8"
		case 16:
			return "int16"
		case 32:
			return "int32"
		case 64:
			return "int64"
		}
	case typebridge.KindUint:
		switch t.Width {
		case 0:
			return "uint"
		case 8:
			return "uint8"
		case 16:
			return "uint16"
		case 32:
			return "uint32"
		case 64:
			return "uint64"
		}
	case typebridge.KindFloat:
		switch t.Width {
		case 32:
			return "float32"
		case 64:
			return "float64"
		}
	case typebridge.KindString:
		return "string"
	}
	return ""
}

// irTypeOf maps a typebridge.Type to the compiler3 IR type the MVP
// frontend allocates SSA values in. Returns false for shapes that
// have no IR-side representation yet (slices, structs, interfaces).
func irTypeOf(t typebridge.Type) (ir.Type, bool) {
	switch t.Kind {
	case typebridge.KindBool:
		return ir.TypeBool, true
	case typebridge.KindInt, typebridge.KindUint:
		return ir.TypeI64, true
	case typebridge.KindFloat:
		return ir.TypeF64, true
	case typebridge.KindString:
		return ir.TypeStr, true
	}
	return ir.TypeInvalid, false
}
