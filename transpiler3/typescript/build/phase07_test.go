package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase7QueryNode, TestPhase7QueryDeno, TestPhase7QueryBun run every
// fixture under tests/transpiler3/typescript/fixtures/phase07-query on each
// runtime and assert byte-equal stdout against the .out file.
//
// Phase 7 lowering contract:
//
//   - `from x in xs where pred select proj` lowers to a
//     local const array + for...of loop + conditional push (QueryScopeStmt).
//
//   - `order by f` emits an Array.prototype.sort call after the collection
//     loop (ascending by default; numerical keys use (a,b)=>a-b; string
//     keys use localeCompare or a TS sort comparator).
//
//   - `DatalogQueryExpr` is evaluated at compile time; the result is a
//     static string[] literal in the emitted TypeScript.
//
//   - Nested queries (query over the result of another query) compose by
//     re-using the existing ForEachStmt + IfStmt + push pattern for the
//     outer scope.
//
// Minimum fixture count: 12.
func TestPhase7QueryNode(t *testing.T) { runPhase7On(t, "node") }
func TestPhase7QueryDeno(t *testing.T) { runPhase7On(t, "deno") }
func TestPhase7QueryBun(t *testing.T)  { runPhase7On(t, "bun") }

func runPhase7On(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase07-query")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	mochiCount := 0
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		mochiCount++
		name := strings.TrimSuffix(e.Name(), ".mochi")
		t.Run(name, func(t *testing.T) {
			runTsFixture(t, runtime,
				filepath.Join(fixtureDir, e.Name()),
				filepath.Join(fixtureDir, name+".out"))
		})
	}
	if mochiCount < 12 {
		t.Fatalf("Phase 7 fixture corpus has %d .mochi files, expected at least 12", mochiCount)
	}
}

// TestPhase7EmitShape verifies load-bearing tokens in the query lowering.
// A QueryScopeStmt must produce a local array + for...of + push pattern;
// it must NOT produce a runtime query engine call or import.
func TestPhase7EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase07-query")
	cases := []struct {
		fixture  string
		wants    []string
		notWants []string
	}{
		{
			fixture: "query_filter.mochi",
			wants:   []string{".push(", "for (const"},
			notWants: []string{"require(", "import {"},
		},
		{
			fixture: "query_order_by.mochi",
			wants:   []string{".sort(", ".push("},
			notWants: []string{"import {"},
		},
	}
	for _, tc := range cases {
		t.Run(strings.TrimSuffix(tc.fixture, ".mochi"), func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(filepath.Join(fixtureDir, tc.fixture), outDir, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build %s: %v", tc.fixture, err)
			}
			src := readTrim(t, p)
			for _, want := range tc.wants {
				if !strings.Contains(src, want) {
					t.Errorf("%s emit missing %q\n---\n%s", tc.fixture, want, src)
				}
			}
			for _, notWant := range tc.notWants {
				if strings.Contains(src, notWant) {
					t.Errorf("%s emit contained forbidden token %q", tc.fixture, notWant)
				}
			}
		})
	}
}
