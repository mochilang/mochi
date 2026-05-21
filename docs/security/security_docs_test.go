// Package security holds the Mochi memory-safety threat model
// (threat-model.md) and the public memory-safety statement skeleton
// (memory-safety.md). Both documents are referenced from MEP-41 §6.1
// and §10.8 respectively. The tests in this file enforce the
// MEP-spec-in-sync rule (MEP-41 §13): the documents must enumerate
// every boundary, invariant, and phase that MEP-41 names, and the
// MEP must reference these documents by name.
//
// The tests intentionally read the markdown as plain text rather
// than parsing it as a structured AST. The point is to detect
// drift (a rename in one place that didn't propagate to the other),
// not to validate markdown well-formedness.
package security

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// readSibling reads a file in the same directory as this test source.
func readSibling(t *testing.T, name string) string {
	t.Helper()
	wd, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	data, err := os.ReadFile(filepath.Join(wd, name))
	if err != nil {
		t.Fatalf("read %s: %v", name, err)
	}
	return string(data)
}

// readMEP loads the MEP-41 spec file relative to the repo root.
func readMEP(t *testing.T) string {
	t.Helper()
	wd, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	// docs/security -> docs -> repo root
	root := filepath.Clean(filepath.Join(wd, "..", ".."))
	data, err := os.ReadFile(filepath.Join(root, "website", "docs", "mep", "mep-0041.md"))
	if err != nil {
		t.Fatalf("read MEP-41: %v", err)
	}
	return string(data)
}

// TestThreatModelExists asserts that docs/security/threat-model.md is
// checked in. MEP-41 Phase 0 §8 lists this as a hard deliverable. The
// public memory-safety statement at memory-safety.md refers to this
// document as normative.
func TestThreatModelExists(t *testing.T) {
	got := readSibling(t, "threat-model.md")
	if len(got) < 1024 {
		t.Fatalf("threat-model.md too short (%d bytes); expected a real document, not a stub", len(got))
	}
}

// TestMemorySafetyStatementExists asserts the public memory-safety
// statement skeleton is checked in. MEP-41 Phase 0 §8 names this as a
// hard deliverable; the final wording is supplied in Phase 7 but the
// skeleton must be on disk from Phase 0 onward.
func TestMemorySafetyStatementExists(t *testing.T) {
	got := readSibling(t, "memory-safety.md")
	if len(got) < 1024 {
		t.Fatalf("memory-safety.md too short (%d bytes); expected a real skeleton, not a stub", len(got))
	}
}

// TestThreatModelEnumeratesAllBoundaries asserts the document spells
// out every trust boundary MEP-41 §6.1 names. The check is text-level:
// each boundary phrase must appear at least once. Renaming a boundary
// in MEP-41 must update this list to match.
func TestThreatModelEnumeratesAllBoundaries(t *testing.T) {
	got := readSibling(t, "threat-model.md")
	required := []string{
		// Five user-facing boundaries.
		"Untrusted source program",
		"Untrusted bytecode",
		"Untrusted external data",
		"Untrusted FFI",
		"Untrusted JIT input",
		// The verifier itself as the single point of policy.
		"verifier",
		// MEP-41 §6.1 names the boundary between trusted and untrusted as the verifier.
		"single point of memory-safety policy",
	}
	for _, want := range required {
		if !strings.Contains(got, want) {
			t.Errorf("threat-model.md is missing boundary phrase %q", want)
		}
	}
}

// TestThreatModelEnumeratesAllVerifierRuleClasses asserts the document
// mentions every verifier rule class A through E that MEP-41 §6.2
// defines. Each must be named, and the document must associate at
// least one invariant with each.
func TestThreatModelEnumeratesAllVerifierRuleClasses(t *testing.T) {
	got := readSibling(t, "threat-model.md")
	required := []string{
		"rule class A",
		"rule class B",
		"rule class C",
		"rule class D",
		"rule class E",
	}
	for _, want := range required {
		if !strings.Contains(got, want) {
			t.Errorf("threat-model.md does not mention %q (a MEP-41 §6.2 verifier rule class)", want)
		}
	}
}

// TestThreatModelInvariantsCoverMEP41 asserts the §3 invariants table
// covers every memory-safety invariant MEP-41 §6 surfaces to the
// public statement. If MEP-41 grows a new invariant, this list and the
// document table must grow with it.
func TestThreatModelInvariantsCoverMEP41(t *testing.T) {
	got := readSibling(t, "threat-model.md")
	required := []string{
		"No use-after-free",
		"No cross-type confusion",
		"No generation leak",
		"No null dereference",
		"No out-of-bounds container access",
		"No code injection in JIT",
		"No FFI escape",
	}
	for _, want := range required {
		if !strings.Contains(got, want) {
			t.Errorf("threat-model.md is missing invariant %q", want)
		}
	}
}

// TestThreatModelReferencesGoTargetSealing asserts the document
// cross-references MEP-43 Phase 10 sealing landing at the Go target.
// MEP-43 Phase 10 closed out 2026-05-21 and named runtime/mochi/ffi/seal.go.
func TestThreatModelReferencesGoTargetSealing(t *testing.T) {
	got := readSibling(t, "threat-model.md")
	want := []string{
		"runtime/mochi/ffi/seal.go",
		"MEP-43",
		"OpUnseal",
		"meta",
	}
	for _, w := range want {
		if !strings.Contains(got, w) {
			t.Errorf("threat-model.md should reference %q (MEP-43 Phase 10 sealing closeout)", w)
		}
	}
}

// TestMemorySafetyStatementListsAllPhases asserts the §9 status table
// has a row for every MEP-41 phase, so the gating discipline is
// visible from the public document.
func TestMemorySafetyStatementListsAllPhases(t *testing.T) {
	got := readSibling(t, "memory-safety.md")
	for i := 0; i <= 7; i++ {
		want := "Phase " + string(rune('0'+i))
		if !strings.Contains(got, want) {
			t.Errorf("memory-safety.md is missing a row for %q", want)
		}
	}
}

// TestMemorySafetyStatementMentionsCISA asserts §7 carries the
// pledge-template language; this is the single user-facing artifact
// downstream organizations cite.
func TestMemorySafetyStatementMentionsCISA(t *testing.T) {
	got := readSibling(t, "memory-safety.md")
	required := []string{
		"CISA Secure-by-Design Pledge",
		"January 1",
		"memory-safety roadmap",
	}
	for _, w := range required {
		if !strings.Contains(got, w) {
			t.Errorf("memory-safety.md is missing pledge phrase %q", w)
		}
	}
}

// TestMemorySafetyStatementMentionsGuarantees asserts every property
// the §1 table promises is named. If MEP-41 grows or renames a
// guarantee, this list and the table must move together.
func TestMemorySafetyStatementMentionsGuarantees(t *testing.T) {
	got := readSibling(t, "memory-safety.md")
	required := []string{
		"No use-after-free",
		"No cross-type confusion",
		"No null dereference",
		"No out-of-bounds container access",
		"No code injection in JIT",
		"No FFI escape",
		"Generation opacity",
	}
	for _, w := range required {
		if !strings.Contains(got, w) {
			t.Errorf("memory-safety.md §1 table is missing property %q", w)
		}
	}
}

// TestMemorySafetyStatementCitesProvenanceChain asserts the §3
// provenance-chain diagram is present and names every load-bearing
// MEP. The chain bottoms out at the Go runtime.
func TestMemorySafetyStatementCitesProvenanceChain(t *testing.T) {
	got := readSibling(t, "memory-safety.md")
	required := []string{
		"MEP-4",
		"MEP-5",
		"MEP-6",
		"MEP-16",
		"MEP-40",
		"MEP-41",
		"Go",
		"CISA-named memory-safe",
	}
	for _, w := range required {
		if !strings.Contains(got, w) {
			t.Errorf("memory-safety.md provenance chain missing %q", w)
		}
	}
}

// TestMemorySafetyStatementSkeletonStatus asserts the page is clearly
// marked as a Phase-0 skeleton (not yet authorized for citation).
// Phase 7 (week 22) replaces this status; before then, external citers
// should be redirected to MEP-41 §10.8.
func TestMemorySafetyStatementSkeletonStatus(t *testing.T) {
	got := readSibling(t, "memory-safety.md")
	for _, want := range []string{"skeleton", "Phase 7"} {
		if !strings.Contains(got, want) {
			t.Errorf("memory-safety.md should mark its status as a %q until Phase 7 closes", want)
		}
	}
}

// TestMEP41ReferencesSecurityDocs asserts the MEP-41 spec mentions
// both documents by relative path. This is the round-trip half of the
// spec-in-sync rule: renaming the docs without updating MEP-41
// should break this test.
func TestMEP41ReferencesSecurityDocs(t *testing.T) {
	mep := readMEP(t)
	for _, want := range []string{
		"docs/security/threat-model.md",
		"docs/security/memory-safety.md",
	} {
		if !strings.Contains(mep, want) {
			t.Errorf("MEP-41 should reference %q (spec-in-sync rule, MEP-41 §13)", want)
		}
	}
}

// TestMEP41Phase0Closeout asserts MEP-41's phase plan flags Phase 0 as
// landed once this PR merges. The closeout marker uses the project's
// standard timestamp format ("YYYY-MM-DD HH:MM (GMT+7)") per memory
// rule "MEP timestamps include hh:mm GMT+7".
func TestMEP41Phase0Closeout(t *testing.T) {
	mep := readMEP(t)
	// The §8 Phase 0 row should be annotated as landed with a 2026-05-21 timestamp in GMT+7.
	if !strings.Contains(mep, "Phase 0") {
		t.Fatal("MEP-41 §8 must keep a Phase 0 row")
	}
	if !strings.Contains(mep, "LANDED") && !strings.Contains(mep, "landed") {
		t.Errorf("MEP-41 §8 Phase 0 row should carry a LANDED marker once this PR merges")
	}
	if !strings.Contains(mep, "2026-05-21") {
		t.Errorf("MEP-41 §8 Phase 0 closeout should carry a 2026-05-21 timestamp")
	}
	if !strings.Contains(mep, "GMT+7") {
		t.Errorf("MEP-41 Phase 0 closeout timestamp should include the GMT+7 zone (project standard)")
	}
}

// TestNoEmDashes is the project-standard prose check: em-dashes are an
// AI tell; the user explicitly requested they not appear in docs,
// comments, or PR bodies. The check runs on both new documents.
func TestNoEmDashes(t *testing.T) {
	for _, name := range []string{"threat-model.md", "memory-safety.md"} {
		got := readSibling(t, name)
		if strings.Contains(got, "—") {
			t.Errorf("%s contains a U+2014 em-dash; project rule is to use comma/period/parens (see memory feedback_no_emdash)", name)
		}
		if strings.Contains(got, "–") {
			t.Errorf("%s contains a U+2013 en-dash; project rule is to use comma/period/parens", name)
		}
	}
}
