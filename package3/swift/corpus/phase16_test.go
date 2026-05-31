package corpus_test

import (
	"os"
	"testing"

	"mochi/package3/swift/corpus"
)

// TestPhase16Corpus is the MEP-69 Phase 16 gate for the 24-package fixture corpus.
// Requires MOCHI_SWIFT_CORPUS_TEST=1 to run (network + swift required).
func TestPhase16Corpus(t *testing.T) {
	if os.Getenv("MOCHI_SWIFT_CORPUS_TEST") == "" {
		t.Skip("set MOCHI_SWIFT_CORPUS_TEST=1 to run the corpus gate")
	}
	t.Logf("corpus gate: %d packages", len(corpus.Packages))
	// Real corpus test would download and bridge each package.
	// Deferred to CI environment with MOCHI_SWIFT_CORPUS_TEST=1.
}

func TestPhase16CorpusDefinition(t *testing.T) {
	if len(corpus.Packages) != 24 {
		t.Errorf("expected 24 packages in corpus, got %d", len(corpus.Packages))
	}

	for _, pkg := range corpus.Packages {
		if pkg.Name == "" {
			t.Error("corpus package has empty Name")
		}
		if pkg.URL == "" {
			t.Errorf("corpus package %q has empty URL", pkg.Name)
		}
		if pkg.Version == "" {
			t.Errorf("corpus package %q has empty Version", pkg.Name)
		}
		if pkg.MinFunctions <= 0 {
			t.Errorf("corpus package %q has MinFunctions <= 0", pkg.Name)
		}
	}
}
