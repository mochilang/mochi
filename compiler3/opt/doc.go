// Package opt holds compiler3's type-preserving optimization passes.
//
// Pass pipeline (MEP-40 §7.3):
//
//  1. ConstFold     (preserves type, produces typed Const values)
//  2. DCE            (deletes unused SSA values)
//  3. BranchThread   (collapses trivial control flow)
//  4. LICM           (loop-invariant code motion, type-aware)
//  5. TailCall       (marks TCO candidates)
//
// Phase 0 ships only the scaffold; pass bodies arrive in Phase 2.
package opt

import "mochi/compiler3/ir"

// ConstFold folds constant operands of arithmetic and comparison ops.
// Phase 0 stub returns fn unchanged.
func ConstFold(fn *ir.Function) { _ = fn }

// DCE removes unused SSA values. Phase 0 stub.
func DCE(fn *ir.Function) { _ = fn }

// BranchThread collapses trivially-jump-only blocks. Phase 0 stub.
func BranchThread(fn *ir.Function) { _ = fn }

// LICM hoists loop-invariant values out of inner loops. Phase 0 stub.
func LICM(fn *ir.Function) { _ = fn }

// TailCall marks return-of-call patterns so emit can lower them to
// OpTailCall. Phase 0 stub.
func TailCall(fn *ir.Function) { _ = fn }
