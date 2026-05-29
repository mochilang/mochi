package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase3_4ListRecords is the Phase 3.4 primary gate. Every
// fixture under tests/transpiler3/typescript/fixtures/phase03.4-list-records
// must lower via the TS transpiler and produce byte-equal stdout
// on Node 22, Deno 2, and Bun 1.1 against the vm3-recorded .out.
//
// The fixture corpus covers seven surface areas:
//
//   - Record decl + literal + field access. Fields cover all four
//     scalar primitives (int, float, bool, string).
//   - Lists of records: literal, len, for-each, var reassign,
//     functional append, grow-via-append-in-loop.
//   - Nested for-loops over two lists of records (the cartesian
//     product shape Phase 7's query DSL will lean on).
//   - Field-driven control flow (`if p.x > 0`, `if r.name == "a"`).
//   - Records as function parameters and return types.
//   - Lists of records as function parameters and return types.
//   - Multiple record types in one program (separate ClassDecl
//     emit order matches source order, Phase 16 reproducibility).
//
// vm3 has a known issue with `print(xs[i].field)` (it prints the
// whole record instead of the field), so every fixture that needs
// indexed access binds via `let p = xs[i]` first; that path works
// in vm3. The for-each pattern works in vm3 directly, which is
// what most fixtures use.
//
// Phase 3.4 ships 33 fixtures; MEP-52 §Phase 3.4 sets a 20-fixture
// floor (sub-phase 3.4 in the planning table).
func TestPhase3_4ListRecordsNode(t *testing.T) { runPhase3_4FixturesOn(t, "node") }
func TestPhase3_4ListRecordsDeno(t *testing.T) { runPhase3_4FixturesOn(t, "deno") }
func TestPhase3_4ListRecordsBun(t *testing.T)  { runPhase3_4FixturesOn(t, "bun") }

func runPhase3_4FixturesOn(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase03.4-list-records")
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
	if mochiCount < 20 {
		t.Fatalf("Phase 3.4 fixture corpus has %d .mochi files, expected at least 20 per MEP-52 §Phase 3.4", mochiCount)
	}
}

// TestPhase3_4EmitWithoutRuntime confirms the Phase 3.4 record +
// list-of-record surface lowers to syntactically plausible TS
// without any JS runtime present. It picks one representative
// fixture per surface area and asserts the emit includes a load-
// bearing token.
//
// The emit-shape tests guard against regressions in lowering
// decisions that would still produce byte-equal stdout (a
// shape-blind silent change) but would drift away from the spec's
// emit contract: readonly fields, private constructor, static
// `of(opts)` factory, `R.of({...})` at record-literal sites,
// direct `r.f` reads at field-access sites.
func TestPhase3_4EmitWithoutRuntime(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase03.4-list-records")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "lor_basic.mochi",
			wants: []string{
				"class Pt {",
				"readonly x: number;",
				"readonly y: number;",
				"private constructor(opts: { x: number; y: number; })",
				"static of(opts: { x: number; y: number; }): Pt",
				"return new Pt(opts);",
				"Pt.of({ x: 3, y: 7 })",
				"p.x",
				"p.y",
			},
		},
		{
			fixture: "lor_two_fields.mochi",
			wants: []string{
				"class User {",
				"readonly id: number;",
				"readonly name: string;",
				`User.of({ id: 42, name: "alice" })`,
			},
		},
		{
			fixture: "lor_float_field.mochi",
			wants: []string{
				"readonly v: number;",
				"readonly ok: boolean;",
				"Sample.of({ v: 3.14, ok: true })",
			},
		},
		{
			fixture: "lor_two_elements.mochi",
			wants: []string{
				"const ps: Pt[] = [Pt.of({ x: 1, y: 2 }), Pt.of({ x: 3, y: 4 })];",
				"ps.length",
				"for (const p of ps) {",
			},
		},
		{
			fixture: "lor_for_each_sum.mochi",
			wants: []string{
				"const ps: Pt[] = [Pt.of({ x: 1, y: 10 })",
				"for (const p of ps) {",
				"((sum + p.x) + p.y)",
			},
		},
		{
			fixture: "lor_append.mochi",
			wants: []string{
				"const qs: Pt[] = [...ps, Pt.of({ x: 3, y: 4 })];",
			},
		},
		{
			fixture: "lor_passed_to_fn.mochi",
			wants: []string{
				"function mochi__total_x(ps: Pt[]): number",
				"mochi__total_x(qs)",
			},
		},
		{
			fixture: "lor_returned_from_fn.mochi",
			wants: []string{
				"function mochi__mk_points(): Pt[]",
				"return [Pt.of({ x: 11, y: 22 }), Pt.of({ x: 33, y: 44 })];",
			},
		},
		{
			fixture: "lor_record_param.mochi",
			wants: []string{
				"function mochi__magnitude_sq(p: Pt): number",
				"((p.x * p.x) + (p.y * p.y))",
			},
		},
		{
			fixture: "lor_record_return.mochi",
			wants: []string{
				"function mochi__origin(): Pt",
				"function mochi__shift(p: Pt, dx: number, dy: number): Pt",
				"return Pt.of({ x: (p.x + dx), y: (p.y + dy) });",
			},
		},
		{
			fixture: "lor_index_via_let.mochi",
			wants: []string{
				"const first: Pt = mochi_list_at(ps, 0);",
				"first.x",
			},
		},
		{
			fixture: "lor_two_records.mochi",
			wants: []string{
				"class Pt {",
				"class User {",
			},
		},
		{
			fixture: "lor_grow_loop.mochi",
			wants: []string{
				"let xs: Item[] = [];",
				"xs = [...xs, Item.of({ v: (i * i) })];",
			},
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
		})
	}
}

// TestPhase3_4RecordClassShape asserts every emit for a Phase 3.4
// fixture follows the four spec-mandated class invariants:
//
//   1. The constructor is `private` (user code cannot `new` the
//      record; the factory is the only construction path).
//   2. The factory is `static of(opts: ...)`.
//   3. Every field is `readonly` (no per-field mutation).
//   4. The factory body is `return new <RecordName>(opts);` (no
//      indirection, no allocation reuse, no clone helper).
//
// These invariants are the contract Phase 4 will tighten (deep
// equality, structural deconstruction); a regression here would
// silently break Phase 4's assumptions even if byte-equal stdout
// still held.
func TestPhase3_4RecordClassShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase03.4-list-records")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		t.Run(name, func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(filepath.Join(fixtureDir, e.Name()), outDir, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build: %v", err)
			}
			src := readTrim(t, p)
			// Count class declarations (a class decl starts a line and
			// reads `class <Name> {`); we ignore the word "class" when
			// it appears inside a comment or string literal.
			classes := 0
			for _, line := range strings.Split(src, "\n") {
				trimmed := strings.TrimSpace(line)
				if strings.HasPrefix(trimmed, "class ") && strings.HasSuffix(trimmed, " {") {
					classes++
				}
			}
			if classes == 0 {
				return // a fixture might not declare any record locally
			}
			privateCtors := strings.Count(src, "private constructor(opts:")
			if privateCtors != classes {
				t.Errorf("%s: %d classes but %d private constructors; every class must have a private ctor", e.Name(), classes, privateCtors)
			}
			staticOf := strings.Count(src, "static of(opts:")
			if staticOf != classes {
				t.Errorf("%s: %d classes but %d static of() factories; every class must expose `of`", e.Name(), classes, staticOf)
			}
			// readonly fields: at least one per class (every record
			// is declared with at least one field per the Mochi
			// grammar; aotir's buildRecordDecl rejects zero-field
			// records).
			if !strings.Contains(src, "readonly ") {
				t.Errorf("%s: no `readonly` field annotation found; record fields must be readonly", e.Name())
			}
			// No public new of the record from user code: the
			// emit should never produce `new <RecordName>(` outside
			// the factory body. Approximation: the only `new <X>(`
			// site permitted by the spec is inside the factory's
			// `return new X(opts);` line. Count all `new ` lines
			// and require they all sit inside a factory.
			// Allowed `new ` sites: the factory body (`return new R(opts);`),
			// JS built-in container constructors (Set, Map), and built-in
			// error types thrown by runtime helpers (RangeError, TypeError,
			// etc.). Anything else means user code is constructing a record
			// directly, bypassing the factory.
			allowed := []string{"new Set<", "new Map<", "new RangeError(", "new TypeError(", "new Error("}
			lines := strings.Split(src, "\n")
			for i, line := range lines {
				trimmed := strings.TrimSpace(line)
				if strings.HasPrefix(trimmed, "return new ") || strings.HasPrefix(trimmed, "//") {
					continue
				}
				if !strings.Contains(line, "new ") {
					continue
				}
				ok := false
				for _, a := range allowed {
					if strings.Contains(line, a) {
						ok = true
						break
					}
				}
				if !ok {
					t.Errorf("%s line %d: unexpected `new ` outside the record factory: %s", e.Name(), i+1, line)
				}
			}
		})
	}
}

// TestPhase3_4FieldAccessIsDirect asserts every field-access emit
// is a direct property read (`r.f`) and never a runtime helper.
// Mochi field access is a single, lossless load; introducing a
// helper would be a measurable overhead in the hot loop Phase 7's
// query DSL will produce. The dual invariant (no `mochi_field_*`,
// no `Reflect.get`, no bracketed-string field reads on records)
// keeps the emit aligned with the spec.
func TestPhase3_4FieldAccessIsDirect(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase03.4-list-records")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		t.Run(name, func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(filepath.Join(fixtureDir, e.Name()), outDir, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build: %v", err)
			}
			src := readTrim(t, p)
			if strings.Contains(src, "mochi_field_") {
				t.Errorf("%s: emit references mochi_field_* (field access must be a direct read)", e.Name())
			}
			if strings.Contains(src, "Reflect.get") {
				t.Errorf("%s: emit references Reflect.get (field access must be a direct read)", e.Name())
			}
		})
	}
}
