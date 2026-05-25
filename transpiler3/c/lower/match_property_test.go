package lower

// Phase 4.3: property test for the Maranget canonicalization pass.
//
// The test verifies that canonicalizeMatchStmt (tag-sort + dup-check) is
// semantically neutral: for every possible tag value, the arm selected by
// the sorted MatchStmt is the same as the arm selected by the naive
// first-match evaluation of the original arm order.
//
// Methodology (10 000 cases per run):
//  1. Pick a random union width N (2..8 arms) and a random subset of those
//     tag values to include in the match (1..N arms, each tag distinct).
//  2. Shuffle the arm list randomly.
//  3. Record the naive decision for each tag: the first arm whose tag equals
//     the tested tag (or "default" if none matches).
//  4. Run canonicalizeMatchStmt (sorts arms by tag, validates no dups).
//  5. For every tag in 0..N-1, verify that the sorted match picks the same
//     arm (by VariantName) as the naive decision from step 3.

import (
	"math/rand"
	"testing"

	"mochi/transpiler3/c/aotir"
)

const propertyTestCases = 10_000

func TestPhase4MatchProperty(t *testing.T) {
	rng := rand.New(rand.NewSource(42))

	for trial := range propertyTestCases {
		// Pick a union width (2–8 variants).
		nVariants := 2 + rng.Intn(7)

		// Build a random subset of tags to include in the match (1..nVariants).
		nArms := 1 + rng.Intn(nVariants)
		tagPool := rng.Perm(nVariants)[:nArms]

		// Build arms in random order.
		arms := make([]aotir.MatchArm, nArms)
		for i, tag := range tagPool {
			arms[i] = aotir.MatchArm{
				Tag:         uint8(tag),
				VariantName: variantName(tag),
				Body:        &aotir.Block{},
			}
		}
		// Shuffle arms.
		rng.Shuffle(len(arms), func(i, j int) { arms[i], arms[j] = arms[j], arms[i] })

		// Record the naive first-match decision for each tag in 0..nVariants-1.
		// naive[tag] == "" means no arm matched (would fall through to default).
		naive := make(map[uint8]string, nArms)
		for _, arm := range arms {
			if _, seen := naive[arm.Tag]; !seen {
				naive[arm.Tag] = arm.VariantName
			}
		}

		// Build the MatchStmt and run the canonicalization pass.
		stmt := &aotir.MatchStmt{
			UnionName: "TestUnion",
			Arms:      arms,
		}
		if err := canonicalizeMatchStmt(stmt); err != nil {
			t.Fatalf("trial %d: canonicalizeMatchStmt: %v", trial, err)
		}

		// Build a lookup from tag to sorted arm.
		sorted := make(map[uint8]string, len(stmt.Arms))
		for _, arm := range stmt.Arms {
			sorted[arm.Tag] = arm.VariantName
		}

		// Verify that for every tested tag the decision is identical.
		for tag, naiveChoice := range naive {
			if got := sorted[tag]; got != naiveChoice {
				t.Fatalf("trial %d: tag %d: naive chose %q, sorted chose %q",
					trial, tag, naiveChoice, got)
			}
		}

		// Verify the sorted order is strictly ascending by tag.
		for i := 1; i < len(stmt.Arms); i++ {
			if stmt.Arms[i].Tag <= stmt.Arms[i-1].Tag {
				t.Fatalf("trial %d: arms not sorted: tag[%d]=%d <= tag[%d]=%d",
					trial, i, stmt.Arms[i].Tag, i-1, stmt.Arms[i-1].Tag)
			}
		}
	}
}

// TestPhase4MatchDuplicateRejection verifies that canonicalizeMatchStmt
// returns an error for duplicate tags (would be a lowerer bug in practice).
func TestPhase4MatchDuplicateRejection(t *testing.T) {
	rng := rand.New(rand.NewSource(99))

	for trial := range 1000 {
		nArms := 2 + rng.Intn(5)
		// Force at least one duplicate by drawing nArms tags from a pool
		// smaller than nArms.
		poolSize := 1 + rng.Intn(nArms)
		arms := make([]aotir.MatchArm, nArms)
		for i := range nArms {
			tag := uint8(rng.Intn(poolSize))
			arms[i] = aotir.MatchArm{
				Tag:         tag,
				VariantName: variantName(int(tag)),
				Body:        &aotir.Block{},
			}
		}
		stmt := &aotir.MatchStmt{UnionName: "DupUnion", Arms: arms}
		if err := canonicalizeMatchStmt(stmt); err == nil {
			// Only fail if there actually were duplicates.
			seen := map[uint8]bool{}
			hasDup := false
			for _, arm := range arms {
				if seen[arm.Tag] {
					hasDup = true
					break
				}
				seen[arm.Tag] = true
			}
			if hasDup {
				t.Fatalf("trial %d: expected error for duplicate tags, got nil", trial)
			}
		}
	}
}

// variantName maps a tag index to a deterministic variant name string.
func variantName(tag int) string {
	names := []string{"A", "B", "C", "D", "E", "F", "G", "H"}
	if tag < len(names) {
		return names[tag]
	}
	return "V" + string(rune('0'+tag))
}
