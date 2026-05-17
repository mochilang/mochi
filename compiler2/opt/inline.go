package opt

import "mochi/compiler2/ir"

// LeafInline rewrites every OpCall whose callee is a "leaf" function
// (a function whose body contains no further OpCall / OpTailCall and
// whose value count is under the size threshold) by copying the
// callee's blocks into the caller. The original OpCall instruction is
// replaced by an OpBr to a renamed copy of the callee's entry; the
// callee's OpRet sites become OpBr to a fresh continuation block; if
// the callee returns a value, a phi at the continuation merges the
// per-Ret values.
//
// The pass does not recurse: each call site is inlined at most once,
// and a leaf cannot inline another leaf (because leaves have no
// calls). Callers run DCE afterwards to clean up dead constants /
// parameter-fed comparisons.
//
// Returns the number of call sites inlined across the module.
//
// MEP-38 §3.3. The merge gate (reverse_complement within 3x of Go) is
// measured in Appendix A.3; the pass is opt-in behind opt.LeafInline,
// parallel to ConstFold / DCE / TailCall.
func LeafInline(m *ir.Module) int {
	const leafSizeLimit = 64 // values, including OpParam/OpConst/terminators

	leaf := make([]bool, len(m.Funcs))
	for i, f := range m.Funcs {
		leaf[i] = isLeafFunction(f, leafSizeLimit)
	}

	n := 0
	for callerIdx, caller := range m.Funcs {
		_ = callerIdx
		// Re-scan after each inline: appended blocks shift indices, and a
		// new copy of the leaf may itself be a candidate (impossible, since
		// leaves have no calls, but we re-scan defensively).
		for {
			sites := callsToLeaf(caller, leaf)
			if len(sites) == 0 {
				break
			}
			// Inline the first site only, then re-scan: each inline
			// invalidates the block/value indices for subsequent sites.
			s := sites[0]
			inlineCallSite(caller, m.Funcs[s.calleeIdx], s.callVID, s.blockIdx, s.instIdx)
			n++
		}
	}
	return n
}

type inlineSite struct {
	calleeIdx int
	callVID   ir.ValueID
	blockIdx  int // index into caller.Blocks
	instIdx   int // index into block.Insts
}

// callsToLeaf finds every OpCall in caller whose target is a leaf
// function. Returns the sites in block / inst-index order.
func callsToLeaf(caller *ir.Function, leaf []bool) []inlineSite {
	var out []inlineSite
	for bi, blk := range caller.Blocks {
		for ii, vid := range blk.Insts {
			ins := caller.Values[vid]
			if ins.Op != ir.OpCall {
				continue
			}
			idx := int(ins.Aux)
			if idx < 0 || idx >= len(leaf) || !leaf[idx] {
				continue
			}
			out = append(out, inlineSite{
				calleeIdx: idx,
				callVID:   vid,
				blockIdx:  bi,
				instIdx:   ii,
			})
		}
	}
	return out
}

// isLeafFunction reports whether f has no OpCall / OpTailCall in its
// body and stays under the size limit.
func isLeafFunction(f *ir.Function, limit int) bool {
	if f == nil {
		return false
	}
	if len(f.Values) > limit {
		return false
	}
	for _, ins := range f.Values {
		switch ins.Op {
		case ir.OpCall, ir.OpTailCall:
			return false
		}
	}
	return true
}

// inlineCallSite copies callee into caller at the given OpCall site.
// The site's block is split immediately after the call: the suffix is
// moved to a fresh continuation block, the call is replaced by an
// OpBr to the renamed callee entry, each renamed OpRet becomes an
// OpBr to the continuation, and (for non-Unit returns) a phi at the
// head of the continuation merges the per-Ret values. All references
// to the original call's value get rewritten to the phi (or to the
// single forwarded value when only one Ret is reachable).
func inlineCallSite(caller, callee *ir.Function,
	callVID ir.ValueID, blockIdx, instIdx int) {
	callIns := caller.Values[callVID]
	siteBlock := caller.Blocks[blockIdx]

	// Build per-callee value map. OpParam values point at the caller's
	// call args; everything else gets a freshly allocated ValueID.
	valMap := make([]ir.ValueID, len(callee.Values))
	for i := range valMap {
		valMap[i] = -1
	}
	for vid, ins := range callee.Values {
		if ins.Op == ir.OpParam {
			idx := int(ins.Aux)
			if idx >= 0 && idx < len(callIns.Args) {
				valMap[vid] = callIns.Args[idx]
			}
		}
	}

	// Allocate one fresh block in caller for each callee block, plus
	// one continuation block.
	blockMap := make([]ir.BlockID, len(callee.Blocks))
	for i := range callee.Blocks {
		id := ir.BlockID(len(caller.Blocks))
		caller.Blocks = append(caller.Blocks, &ir.Block{ID: id})
		blockMap[i] = id
	}
	contID := ir.BlockID(len(caller.Blocks))
	caller.Blocks = append(caller.Blocks, &ir.Block{ID: contID})

	// Allocate fresh ValueIDs for every non-param callee inst. Same
	// order as callee's per-block traversal so the SSA "def before
	// use within block" invariant is preserved.
	for _, blk := range callee.Blocks {
		for _, vid := range blk.Insts {
			if callee.Values[vid].Op == ir.OpParam {
				continue
			}
			newVID := ir.ValueID(len(caller.Values))
			caller.Values = append(caller.Values, ir.Inst{})
			caller.ValueBlock = append(caller.ValueBlock,
				blockMap[callee.ValueBlock[vid]])
			valMap[vid] = newVID
		}
	}

	// Second pass: clone each non-param inst with translated operands.
	// OpRet becomes an OpBr to contID; record (predBlock, retValue) for
	// the merging phi.
	type retEdge struct {
		from ir.BlockID
		val  ir.ValueID
	}
	var retEdges []retEdge
	isUnit := callee.RetType == ir.TUnit

	for _, blk := range callee.Blocks {
		newBlockID := blockMap[blk.ID]
		for _, vid := range blk.Insts {
			ins := callee.Values[vid]
			if ins.Op == ir.OpParam {
				continue
			}
			newVID := valMap[vid]
			if ins.Op == ir.OpRet {
				if !isUnit && len(ins.Args) > 0 {
					retEdges = append(retEdges, retEdge{
						from: newBlockID,
						val:  valMap[ins.Args[0]],
					})
				}
				caller.Values[newVID] = ir.Inst{
					Op:        ir.OpBr,
					Type:      ir.TUnit,
					AuxBlocks: []ir.BlockID{contID},
				}
				caller.Blocks[newBlockID].Insts = append(
					caller.Blocks[newBlockID].Insts, newVID)
				continue
			}
			newArgs := make([]ir.ValueID, len(ins.Args))
			for i, a := range ins.Args {
				newArgs[i] = valMap[a]
			}
			newAux := make([]ir.BlockID, len(ins.AuxBlocks))
			for i, b := range ins.AuxBlocks {
				newAux[i] = blockMap[b]
			}
			caller.Values[newVID] = ir.Inst{
				Op:        ins.Op,
				Type:      ins.Type,
				Args:      newArgs,
				AuxBlocks: newAux,
				Aux:       ins.Aux,
			}
			caller.Blocks[newBlockID].Insts = append(
				caller.Blocks[newBlockID].Insts, newVID)
		}
	}

	// Split the site block. Everything after the call moves to the
	// continuation block; the call slot itself is dropped and replaced
	// with an OpBr to the renamed callee entry.
	suffix := append([]ir.ValueID(nil), siteBlock.Insts[instIdx+1:]...)
	siteBlock.Insts = siteBlock.Insts[:instIdx]
	caller.Blocks[contID].Insts = suffix
	for _, vid := range suffix {
		caller.ValueBlock[vid] = contID
	}

	brVID := ir.ValueID(len(caller.Values))
	caller.Values = append(caller.Values, ir.Inst{
		Op:        ir.OpBr,
		Type:      ir.TUnit,
		AuxBlocks: []ir.BlockID{blockMap[callee.Entry]},
	})
	caller.ValueBlock = append(caller.ValueBlock, siteBlock.ID)
	siteBlock.Insts = append(siteBlock.Insts, brVID)

	// If the callee returns a value, merge the per-Ret edges into a
	// phi at the continuation head, then rewrite uses of the original
	// call to the phi.
	var replacement ir.ValueID = -1
	switch {
	case isUnit:
		// Nothing to forward; the original call value is unused.
	case len(retEdges) == 1:
		replacement = retEdges[0].val
	case len(retEdges) > 1:
		phiVID := ir.ValueID(len(caller.Values))
		args := make([]ir.ValueID, len(retEdges))
		preds := make([]ir.BlockID, len(retEdges))
		for i, e := range retEdges {
			args[i] = e.val
			preds[i] = e.from
		}
		caller.Values = append(caller.Values, ir.Inst{
			Op:        ir.OpPhi,
			Type:      callee.RetType,
			Args:      args,
			AuxBlocks: preds,
		})
		caller.ValueBlock = append(caller.ValueBlock, contID)
		contBlk := caller.Blocks[contID]
		contBlk.Insts = append([]ir.ValueID{phiVID}, contBlk.Insts...)
		replacement = phiVID
	}

	caller.Values[callVID] = ir.Inst{Op: ir.OpInvalid}
	if replacement >= 0 {
		rewriteUses(caller, callVID, replacement)
	}
}

// rewriteUses replaces every Args reference to oldVID with newVID.
func rewriteUses(f *ir.Function, oldVID, newVID ir.ValueID) {
	for i := range f.Values {
		args := f.Values[i].Args
		for j, a := range args {
			if a == oldVID {
				args[j] = newVID
			}
		}
	}
}
