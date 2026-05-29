package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase5Sums is the Phase 5 primary gate. Every fixture under
// tests/transpiler3/typescript/fixtures/phase05-sums must lower via
// the TS transpiler and produce byte-equal stdout on Node 22, Deno 2,
// and Bun 1.1 against the vm3-recorded .out.
//
// Phase 5 ships Mochi sum types (a.k.a. tagged unions / algebraic
// data types) and the `match` form that destructures them. The
// lowering contract is:
//
//   - A Mochi `type T = A(...) | B(...) | C` sum declaration lowers
//     to a TypeScript discriminated union over a literal `kind` tag:
//
//         type T =
//           | { readonly kind: "A"; readonly f1: ... }
//           | { readonly kind: "B"; readonly g1: ... }
//           | { readonly kind: "C" };
//
//     The literal-string tag is what gives tsc its discriminant for
//     control-flow narrowing across the switch in the match emit.
//     `readonly` on every field keeps Mochi's immutable-by-default
//     semantics surfaced in the type system; nothing in the emit ever
//     mutates a variant value in place.
//
//   - A variant constructor call like `Circle(5)` lowers to an
//     object literal with the right `kind` plus the field bindings,
//     widened back to the union type via a `as T` assertion:
//
//         { kind: "Circle", r: 5 } as Shape
//
//     The cast is load-bearing. Without it, tsc's const-aliasing
//     control-flow analysis narrows the initialiser's static type
//     down to the singleton literal `{kind:"Circle"; r:5}`, after
//     which the subsequent `case "Square"` arm fails TS2678
//     ("Type '"Square"' is not comparable to type '"Circle"'") even
//     though the runtime would have been correct. The `as T` widens
//     the slot back to the union so downstream match arms type-check.
//
//   - A `match` expression lowers to a fresh local capture of the
//     scrutinee plus a `switch` on `.kind`, wrapped in a block to
//     contain the temp so it doesn't leak:
//
//         let __matchN!: T;
//         {
//           const __mochi_match_N: U = scrutinee;
//           switch (__mochi_match_N.kind) {
//             case "A": {
//               const f1: ... = __mochi_match_N.f1;
//               __matchN = arm_a;
//               break;
//             }
//             ...
//             default: { mochiUnreachable(__mochi_match_N); break; }
//           }
//         }
//
//     The result var is declared with a TS definite-assignment
//     assertion (`!`) because the C lowerer pre-emits the slot with
//     no initialiser. tsc's CFA can't see across the switch into the
//     assignments under arms, so the `!` is the only way to keep the
//     emit strict-clean without sacrificing immutability elsewhere.
//
//   - Each arm's pattern bindings become `const` reads off the
//     captured scrutinee. They are scoped to the arm's block, so two
//     arms can introduce different field types under the same
//     binding name without colliding.
//
//   - The `default:` arm calls `mochiUnreachable(__mochi_match_N)`.
//     `mochiUnreachable(x: never): never` is the standard TS
//     exhaustiveness witness: if a future code change adds a variant
//     to the union without adding a case, tsc rejects the argument
//     (type widens past `never`) at the call site, surfacing the
//     missed arm at compile time.
//
//   - A wildcard arm (`_ =>`) replaces the `default: mochiUnreachable`
//     line with `default: { __matchN = arm_wildcard; break; }`. The
//     witness call is dropped because exhaustiveness is now
//     dynamically satisfied; the cost is that adding a variant won't
//     trip tsc, but that's the user's explicit opt-out.
//
//   - A unit variant (no fields, e.g. `North`) lowers to a singleton
//     `{ kind: "North" }` object literal cast to the union. No field
//     bindings, no extra cost.
//
// Why this gate matters. Match is Mochi's primary destructuring
// form. Phase 6 (closures) wants match in higher-order callbacks;
// Phase 7 (query DSL) wants match inside `where` predicates that
// dispatch on row tag types; Phase 8 (datalog) wants match over
// fact-shape unions in stratified rules. Every later phase reads
// from this surface, so it has to land tsc-strict-clean across all
// three runtimes before the rest of the stack can compose.
//
// The fixture corpus covers thirteen surface areas:
//
//   - Sum with payload fields (Circle(r) / Square(side)).
//   - Sum with int / string / float / bool / mixed payload field
//     types (the discriminant survives each).
//   - Sum with unit-only variants (Status, Dir, Day, Level, Color).
//   - Sum used as a function parameter and a function return type.
//   - Match inside a function body (statement-position) vs match as
//     an expression (RHS of `let`).
//   - Two match expressions in the same function.
//   - Nested match (a match in the arm of another match).
//   - Two distinct sum types in one program (helper / type emit must
//     stay byte-stable in source order).
//   - Wildcard arm only.
//   - Wildcard mixed with constructor arms.
//   - Match in a `for` loop body.
//   - Match where the scrutinee is itself a let-bound local.
//   - Two consecutive matches sharing the same scrutinee (each gets
//     its own `__mochi_match_<n>` capture).
//
// Phase 5 ships 24 fixtures; the floor is 20.
func TestPhase5SumsNode(t *testing.T) { runPhase5FixturesOn(t, "node") }
func TestPhase5SumsDeno(t *testing.T) { runPhase5FixturesOn(t, "deno") }
func TestPhase5SumsBun(t *testing.T)  { runPhase5FixturesOn(t, "bun") }

func runPhase5FixturesOn(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase05-sums")
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
		t.Fatalf("Phase 5 fixture corpus has %d .mochi files, expected at least 20", mochiCount)
	}
}

// TestPhase5EmitShape lowers a representative fixture and asserts
// the load-bearing tokens are present in the emit. These tokens
// distinguish the shipped lowering from a byte-equal but
// type-incorrect alternative (e.g. a class-based encoding that
// happens to print the same numbers but doesn't survive tsc-strict
// or doesn't compose with Phase 7's `where` narrowing).
//
// The invariants checked here are:
//
//   - `type Shape =` — the sum decl lowers to a TS `type` alias, not
//     an interface or a class hierarchy.
//   - `readonly kind: "Circle"` — the discriminant is a literal
//     string tag, `readonly`, scoped to the variant.
//   - `as Shape` — variant constructor emits widen the literal back
//     to the union (the const-aliasing fix).
//   - `switch (__mochi_match_` — match lowers to `switch` (not an
//     if/else cascade), keying on the captured-scrutinee variable.
//   - `mochiUnreachable(` — exhaustive match emits the witness call
//     in its `default:` arm.
//   - `function mochiUnreachable(x: never): never` — the runtime
//     helper is emitted exactly once per program that uses match
//     exhaustively, and its signature is the standard never-witness
//     form so tsc can flag added variants.
func TestPhase5EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase05-sums")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "req_circle_square.mochi",
			wants: []string{
				"type Shape =",
				`readonly kind: "Circle"`,
				`readonly kind: "Square"`,
				"readonly r: number",
				"readonly side: number",
				"as Shape",
				"switch (__mochi_match_",
				"mochiUnreachable(",
				"function mochiUnreachable(x: never): never",
			},
		},
		{
			fixture: "req_unit_enum.mochi",
			wants: []string{
				"type Status =",
				`readonly kind: "Active"`,
				`readonly kind: "Inactive"`,
				`readonly kind: "Pending"`,
				"as Status",
				"switch (__mochi_match_",
			},
		},
		{
			fixture: "req_function_param.mochi",
			wants: []string{
				"function mochi__area(s: Shape): number",
				"switch (__mochi_match_",
				"mochiUnreachable(",
			},
		},
		{
			fixture: "req_two_unions.mochi",
			wants: []string{
				"type Shape =",
				"type Color =",
				"as Shape",
				"as Color",
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

// TestPhase5WildcardSkipsUnreachable asserts that a match with a
// wildcard arm does NOT emit a `mochiUnreachable(` call in its
// `default:` branch. The wildcard is the user's explicit opt-out
// from exhaustiveness; the witness call would be unreachable (the
// wildcard already absorbs the missing variants) and would also
// fail tsc if the scrutinee variable's type isn't `never` at that
// point (which it won't be when arms are missing).
//
// Conversely, an exhaustive match (every variant named, no `_`) is
// required to emit `mochiUnreachable(` so tsc rejects future
// non-exhaustive code at compile time.
func TestPhase5WildcardSkipsUnreachable(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase05-sums")
	outDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	p, err := d.Build(filepath.Join(fixtureDir, "req_wildcard.mochi"), outDir, TargetTypeScriptSource)
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	src := readTrim(t, p)
	// The match in req_wildcard.mochi has only North / South / _ arms
	// out of {North,South,East,West}. Because the user supplied `_`,
	// the lowerer must trust the user and skip the never-witness.
	if strings.Contains(src, "mochiUnreachable(__mochi_match_") {
		t.Errorf("wildcard match emitted mochiUnreachable witness in default arm; expected user's _ arm to suppress it\n---\n%s", src)
	}
}

// TestPhase5UnreachableEmittedOnce asserts the mochiUnreachable
// runtime helper is emitted exactly once per program, no matter how
// many exhaustive match sites reference it. A regression to
// per-call-site emission would inflate the bundle linearly with
// match-site count and break Phase 16 byte-equal reproducibility.
func TestPhase5UnreachableEmittedOnce(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase05-sums")
	cases := []string{
		"req_multi_match.mochi", // two exhaustive matches in one program
		"req_two_unions.mochi",  // two exhaustive matches, two distinct sum types
		"req_nested_match.mochi",
		"req_local_double_match.mochi",
	}
	for _, fx := range cases {
		t.Run(strings.TrimSuffix(fx, ".mochi"), func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(filepath.Join(fixtureDir, fx), outDir, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build: %v", err)
			}
			src := readTrim(t, p)
			helpers := strings.Count(src, "function mochiUnreachable(")
			if helpers != 1 {
				t.Errorf("%s: expected exactly one mochiUnreachable declaration, found %d\n---\n%s", fx, helpers, src)
			}
		})
	}
}

// TestPhase5MatchTempIsLocal asserts every match emit captures its
// scrutinee in a fresh `__mochi_match_<n>` local inside a block, so
// the temp can't leak to surrounding scope. Two matches in one
// function must use distinct counter values; the counter resets per
// function so byte-equal regenerated emits stay stable.
func TestPhase5MatchTempIsLocal(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase05-sums")
	outDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	p, err := d.Build(filepath.Join(fixtureDir, "req_multi_match.mochi"), outDir, TargetTypeScriptSource)
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	src := readTrim(t, p)
	// Two distinct captures, both inside switch headers.
	if !strings.Contains(src, "switch (__mochi_match_1.kind)") {
		t.Errorf("expected first match to capture into __mochi_match_1\n---\n%s", src)
	}
	if !strings.Contains(src, "switch (__mochi_match_2.kind)") {
		t.Errorf("expected second match to capture into __mochi_match_2\n---\n%s", src)
	}
}
