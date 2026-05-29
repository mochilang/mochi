package corpus_test

import (
	"strings"
	"testing"

	"mochi/package3/php/asyncemit"
	"mochi/package3/php/corpus"
	"mochi/package3/php/externemit"
	"mochi/package3/php/glue"
	"mochi/package3/php/lock"
	bridgeerrors "mochi/package3/php/errors"
	"mochi/package3/php/typemap"
)

// TestCorpusSize verifies we have exactly 24 fixtures.
func TestCorpusSize(t *testing.T) {
	fixtures := corpus.All()
	if len(fixtures) != 24 {
		t.Errorf("expected 24 fixtures; got %d", len(fixtures))
	}
}

// TestCorpusNoDuplicateNames verifies all package names are unique.
func TestCorpusNoDuplicateNames(t *testing.T) {
	seen := map[string]bool{}
	for _, f := range corpus.All() {
		if seen[f.Name] {
			t.Errorf("duplicate fixture name: %s", f.Name)
		}
		seen[f.Name] = true
	}
}

// TestCorpusExternEmitDoesNotPanic runs Emit on all fixtures.
func TestCorpusExternEmitDoesNotPanic(t *testing.T) {
	for _, f := range corpus.All() {
		t.Run(f.Name, func(t *testing.T) {
			result, err := externemit.Emit(f.Surface)
			if err != nil {
				t.Fatalf("%s: Emit error: %v", f.Name, err)
			}
			if result == nil {
				t.Fatalf("%s: nil result", f.Name)
			}
		})
	}
}

// TestCorpusExternEmitProducesOutput verifies packages with methods produce output.
func TestCorpusExternEmitProducesOutput(t *testing.T) {
	for _, f := range corpus.All() {
		hasMethods := false
		for _, cls := range f.Surface.Classes {
			if len(cls.Methods) > 0 {
				hasMethods = true
				break
			}
		}
		for _, iface := range f.Surface.Interfaces {
			if len(iface.Methods) > 0 {
				hasMethods = true
				break
			}
		}
		if len(f.Surface.Functions) > 0 {
			hasMethods = true
		}
		if !hasMethods {
			continue
		}
		t.Run(f.Name, func(t *testing.T) {
			result, err := externemit.Emit(f.Surface)
			if err != nil {
				t.Fatalf("Emit error: %v", err)
			}
			if result.MochiSource == "" {
				t.Errorf("%s: expected non-empty MochiSource for surface with methods", f.Name)
			}
		})
	}
}

// TestCorpusExternEmitExternFnPresent verifies extern fn appears in output for
// packages that have mappable methods.
func TestCorpusExternEmitExternFnPresent(t *testing.T) {
	// Packages known to have at least one fully-mappable method.
	knownMappable := map[string]bool{
		"psr/log":                  true,
		"league/flysystem":         true,
		"phpmailer/phpmailer":      true,
		"ramsey/uuid":              true,
		"paragonie/random_compat":  true,
	}
	for _, f := range corpus.All() {
		if !knownMappable[f.Name] {
			continue
		}
		f := f
		t.Run(f.Name, func(t *testing.T) {
			result, err := externemit.Emit(f.Surface)
			if err != nil {
				t.Fatalf("Emit error: %v", err)
			}
			if !strings.Contains(result.MochiSource, "extern fn") {
				t.Errorf("%s: expected extern fn in output; got:\n%s", f.Name, result.MochiSource)
			}
		})
	}
}

// TestCorpusMixedTypeSkipped verifies methods with mixed params/returns produce SkipReports.
func TestCorpusMixedTypeSkipped(t *testing.T) {
	// guzzlehttp/guzzle has getConfig() -> mixed which should be skipped.
	// laravel/framework has make() -> mixed.
	skippedPackages := []string{"guzzlehttp/guzzle", "laravel/framework"}
	for _, f := range corpus.All() {
		for _, want := range skippedPackages {
			if f.Name != want {
				continue
			}
			result, err := externemit.Emit(f.Surface)
			if err != nil {
				t.Fatalf("%s: Emit error: %v", f.Name, err)
			}
			if len(result.Skips) == 0 {
				t.Errorf("%s: expected at least one skip report for mixed type; got none", f.Name)
			}
		}
	}
}

// TestCorpusGlueEmitDoesNotPanic runs glue.Emit on all fixtures.
func TestCorpusGlueEmitDoesNotPanic(t *testing.T) {
	for _, f := range corpus.All() {
		t.Run(f.Name, func(t *testing.T) {
			vendor := lock.Vendor(f.Name)
			pkg := lock.Pkg(f.Name)
			result, err := glue.Emit(f.Surface, vendor, pkg)
			if err != nil {
				t.Fatalf("%s: glue.Emit error: %v", f.Name, err)
			}
			if result == nil {
				t.Fatalf("%s: nil glue result", f.Name)
			}
		})
	}
}

// TestCorpusGlueNamespaceCorrect verifies the glue namespace format.
func TestCorpusGlueNamespaceCorrect(t *testing.T) {
	for _, f := range corpus.All() {
		vendor := lock.Vendor(f.Name)
		pkg := lock.Pkg(f.Name)
		result, err := glue.Emit(f.Surface, vendor, pkg)
		if err != nil {
			t.Fatalf("%s: glue.Emit error: %v", f.Name, err)
		}
		if !strings.HasPrefix(result.Namespace, "MochiGlue\\") {
			t.Errorf("%s: namespace should start with MochiGlue\\; got %q", f.Name, result.Namespace)
		}
	}
}

// TestCorpusLockRoundTrip verifies Format -> Check round-trip for all fixtures.
func TestCorpusLockRoundTrip(t *testing.T) {
	entries := make([]lock.PhpPackage, 0, len(corpus.All()))
	for _, f := range corpus.All() {
		entries = append(entries, lock.PhpPackage{
			Name:              f.Name,
			Version:           "1.0.0",
			DistSHA256:        "abc123",
			ReflectionSHA256:  "def456",
		})
	}
	formatted := lock.Format(entries)
	if formatted == "" {
		t.Error("expected non-empty lock format output")
	}
	// Verify all package names appear in the formatted output.
	for _, e := range entries {
		if !strings.Contains(formatted, e.Name) {
			t.Errorf("expected %q in formatted lock output", e.Name)
		}
	}
	// Round-trip: Check with matching hashes -> no drift.
	hashes := make(map[string]lock.OnDiskHashes)
	for _, e := range entries {
		hashes[e.Name] = lock.OnDiskHashes{
			DistSHA256:       e.DistSHA256,
			ReflectionSHA256: e.ReflectionSHA256,
		}
	}
	clean, drifts := lock.Check(entries, hashes)
	if !clean {
		t.Errorf("expected clean lock check; got drifts: %v", drifts)
	}
}

// TestCorpusLockDriftDetected verifies drift is detected when hashes differ.
func TestCorpusLockDriftDetected(t *testing.T) {
	entries := []lock.PhpPackage{
		{Name: "guzzlehttp/guzzle", Version: "7.8.0", DistSHA256: "expected-sha", ReflectionSHA256: "refl-sha"},
	}
	hashes := map[string]lock.OnDiskHashes{
		"guzzlehttp/guzzle": {DistSHA256: "DIFFERENT-sha", ReflectionSHA256: "refl-sha"},
	}
	clean, drifts := lock.Check(entries, hashes)
	if clean {
		t.Error("expected drift to be detected")
	}
	if len(drifts) == 0 {
		t.Error("expected at least one DriftEntry")
	}
}

// TestCorpusAsyncDetection verifies async methods are detected for async packages.
func TestCorpusAsyncDetection(t *testing.T) {
	for _, f := range corpus.All() {
		if f.Name != "guzzlehttp/guzzle" {
			continue
		}
		result := asyncemit.Emit(f.Surface, asyncemit.Config{})
		if result.AsyncMethodCount == 0 {
			t.Errorf("guzzlehttp/guzzle: expected async methods detected; got 0")
		}
		if !strings.Contains(result.MochiSource, "async extern fn") {
			t.Errorf("guzzlehttp/guzzle: expected async extern fn; got:\n%s", result.MochiSource)
		}
	}
}

// TestCorpusTypemapPrimitiveTypes exercises the type table for common primitive-returning methods.
func TestCorpusTypemapPrimitiveTypes(t *testing.T) {
	cases := []struct {
		phpType string
		wantOK  bool
	}{
		{"string", true},
		{"int", true},
		{"bool", true},
		{"float", true},
		{"void", true},
		{"mixed", false},
		{"object", false},
	}
	for _, tc := range cases {
		m, skip, _ := typemap.Map(tc.phpType, false, typemap.DirectionOut)
		if tc.wantOK && (m == nil || skip != bridgeerrors.SkipUnknown) {
			t.Errorf("typemap.Map(%q): expected mapping; got skip=%v", tc.phpType, skip)
		}
		if !tc.wantOK && skip == bridgeerrors.SkipUnknown {
			t.Errorf("typemap.Map(%q): expected skip; got mapping %v", tc.phpType, m)
		}
	}
}

// TestCorpusAllFixturesHaveSurface verifies no fixture has a nil surface.
func TestCorpusAllFixturesHaveSurface(t *testing.T) {
	for _, f := range corpus.All() {
		if f.Surface == nil {
			t.Errorf("%s: Surface is nil", f.Name)
		}
		if f.Surface.PackageName == "" {
			t.Errorf("%s: Surface.PackageName is empty", f.Name)
		}
	}
}
