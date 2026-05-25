package lower

// Package lower — match.go: Maranget decision-tree pass (Phase 4.1).
//
// For Mochi's current single-column pattern language (variant tag dispatch
// on sum types), the Maranget (2008) optimal decision tree is a plain
// switch(tag) with one case per variant. This pass:
//
//  1. Validates each MatchStmt has no duplicate variant tags.
//  2. Sorts MatchStmt arms by ascending tag for deterministic C output.
//  3. Recursively processes arm bodies and all other block-bearing statements.
//
// When Mochi grows multi-column patterns or guards, this pass becomes the
// natural home for the full Maranget algorithm; the emitter stays unchanged
// because it just traverses MatchStmt.Arms in order.

import (
	"fmt"
	"sort"

	"mochi/transpiler3/c/aotir"
)

// lowerMatchPass runs the Maranget canonicalization pass over every
// function in prog (including main). It returns an error if any MatchStmt
// contains duplicate tags, which would indicate a lowerer bug.
func lowerMatchPass(prog *aotir.Program) error {
	for i := range prog.Functions {
		if err := matchPassBlock(prog.Functions[i].Body); err != nil {
			return fmt.Errorf("function %q: %w", prog.Functions[i].Name, err)
		}
	}
	return nil
}

// matchPassBlock recurses into every statement in the block.
func matchPassBlock(block *aotir.Block) error {
	if block == nil {
		return nil
	}
	for _, stmt := range block.Statements {
		if err := matchPassStmt(stmt); err != nil {
			return err
		}
	}
	return nil
}

// matchPassStmt handles a single statement, recursing into nested blocks.
func matchPassStmt(stmt aotir.Stmt) error {
	if stmt == nil {
		return nil
	}
	switch s := stmt.(type) {
	case *aotir.MatchStmt:
		return canonicalizeMatchStmt(s)
	case *aotir.IfStmt:
		if err := matchPassBlock(s.Then); err != nil {
			return err
		}
		return matchPassBlock(s.Else)
	case *aotir.WhileStmt:
		return matchPassBlock(s.Body)
	case *aotir.ForRangeStmt:
		return matchPassBlock(s.Body)
	case *aotir.ForEachStmt:
		return matchPassBlock(s.Body)
	case *aotir.QueryScopeStmt:
		return matchPassBlock(s.Body)
	}
	return nil
}

// canonicalizeMatchStmt is the Maranget kernel for a single-column tag
// dispatch. For the current pattern language it:
//   - Detects duplicate tags (lowerer bug).
//   - Sorts arms by ascending tag (deterministic case ordering for cc).
//   - Recurses into every arm body and the default body.
func canonicalizeMatchStmt(s *aotir.MatchStmt) error {
	// Validate: every (non-default) arm must carry a distinct tag.
	seen := make(map[uint8]struct{}, len(s.Arms))
	for _, arm := range s.Arms {
		if _, dup := seen[arm.Tag]; dup {
			return fmt.Errorf("match on %q: duplicate tag %d", s.UnionName, arm.Tag)
		}
		seen[arm.Tag] = struct{}{}
	}

	// Sort arms by tag so the emitted switch cases are in ascending order
	// regardless of source order. This gives deterministic C output and
	// lets the C compiler and CPU branch predictor see a clean sequence.
	sort.Slice(s.Arms, func(i, j int) bool {
		return s.Arms[i].Tag < s.Arms[j].Tag
	})

	// Recurse into arm bodies.
	for i := range s.Arms {
		if err := matchPassBlock(s.Arms[i].Body); err != nil {
			return fmt.Errorf("arm %q: %w", s.Arms[i].VariantName, err)
		}
	}
	// Recurse into default body.
	if s.Default != nil {
		if err := matchPassBlock(s.Default.Body); err != nil {
			return fmt.Errorf("default arm: %w", err)
		}
	}
	return nil
}
