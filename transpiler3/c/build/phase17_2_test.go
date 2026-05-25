package build

import (
	"path/filepath"
	"runtime"
	"testing"
)

// TestPhase17IROrdering is the MEP-45 Phase 17.2 gate. It verifies that
// the emitter produces byte-identical C source (and thus identical binaries)
// regardless of Go map iteration order by building complex fixtures that
// exercise every collect* function in the emitter:
//
//   - list_of_list: exercises collectListListInners (list<list<T>> inners)
//   - list_of_map: exercises collectListOfMapPairs (list<map<K,V>> pairs)
//   - map_of_list: exercises collectMapOfListInners (map<K,list<V>> inners)
//
// Each fixture is built twice with a fixed SOURCE_DATE_EPOCH; binary
// SHA-256 must be equal. The audit in the implementation doc confirms:
//
//   1. collectListListInners iterates a map[string]struct{} but its
//      caller emitListOfListHelpers sorts the result before iterating.
//   2. collectListRecordElems and collectListOfMapPairs follow the same
//      pattern: unsorted collect, sorted emit.
//   3. prog.Records and prog.Functions are accumulated via append in
//      source declaration order, never by map iteration.
//
// Together these properties guarantee that the emitted C is sorted and
// stable regardless of Go's runtime map randomisation.
func TestPhase17IROrdering(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("IR ordering gate skipped on Windows; Phase 11 wires Windows CI")
	}

	const fixedEpoch = "1748000000"
	t.Setenv("SOURCE_DATE_EPOCH", fixedEpoch)

	root := repoRoot(t)
	// Fixtures that trigger multiple distinct type instantiations, which
	// stress-test the collect* → sort → emit pipeline for IR ordering.
	fixtureRoots := []struct {
		suite string
		name  string
	}{
		// list<list<T>>: multiple list-list inners in one TU
		{"list_of_list", "lol_bool_basic"},
		// list<map<K,V>>: list<map<string,int>> in one TU
		{"list_of_map", "lom_i64_i64_basic"},
		// map<K,list<V>>: map<string,list<int>> in one TU
		{"map_of_list", "mol_int_list_int_for_keys"},
		// sum_types: multiple union arms + match - tests Records ordering
		{"sum_types", "sum_circle_square"},
	}

	for _, fx := range fixtureRoots {
		t.Run(fx.suite+"/"+fx.name, func(t *testing.T) {
			src := filepath.Join(root, "tests", "transpiler3", "c", "fixtures",
				fx.suite, fx.name, fx.name+".mochi")

			var hashes [2]string
			for i := range 2 {
				outBin := filepath.Join(t.TempDir(), fx.name)
				d := &Driver{CacheDir: t.TempDir(), NoCache: true}
				if err := d.Build(src, outBin, "", ""); err != nil {
					t.Fatalf("build %d: %v", i+1, err)
				}
				h, err := sha256File(outBin)
				if err != nil {
					t.Fatalf("sha256 build %d: %v", i+1, err)
				}
				hashes[i] = h
			}

			if hashes[0] != hashes[1] {
				t.Fatalf("binary not reproducible (%s/%s):\nbuild1: %s\nbuild2: %s",
					fx.suite, fx.name, hashes[0], hashes[1])
			}
			t.Logf("reproducible %s/%s: %s", fx.suite, fx.name, hashes[0])
		})
	}
}
