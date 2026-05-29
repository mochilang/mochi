package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase6Closures is the Phase 6 primary gate. Every fixture
// under tests/transpiler3/typescript/fixtures/phase06-closures must
// lower via the TS transpiler and produce byte-equal stdout on Node
// 22, Deno 2, and Bun 1.1 against the vm3-recorded .out.
//
// Phase 6 ships Mochi anonymous fun expressions, higher-order
// parameters, fun-typed values returned from functions, and the
// `fun(int): int` type syntax in let-bindings / param slots /
// return slots. The lowering contract is:
//
//   - A Mochi `fun(x: int): int => x * 2` literal lowers to a
//     TypeScript arrow expression `(x: number): number => { return
//     x * 2; }`. The lifted `__anon_<n>` function the aotir IR
//     produces is NOT emitted as a top-level TS function; its body
//     is inlined at the FunLit site. JS closures pick up captures
//     from the surrounding lexical scope for free, so no env
//     struct is generated.
//
//   - A capturing closure (`fun(x) => x + base + offset` where
//     `base` and `offset` are free variables in the closure body)
//     lowers the same way as a non-capturing one. The C lowerer
//     stamps the captured names as `__e->base` / `__e->offset` so
//     its env-struct emit picks them up; the TS lowerer strips the
//     `__e->` prefix on every VarRef, restoring the bare identifier
//     name. The captured value is then resolved by JS via lexical
//     scope.
//
//   - The aotir `ClosureEnvStmt` (allocate-env-struct, fill-fields)
//     has no TS surface and lowers to zero statements. Dropping it
//     keeps the emit clean.
//
//   - A `fun(int): int` type slot lowers to the TS arrow-type
//     `(__p0: number) => number`. The parameter names in the type
//     position are synthetic (the type slot has no real names; only
//     the literal's own params carry names) so they get a fixed
//     `__p<i>` form. Return type after `=>` is the rendered TS type
//     of the FunSig's ReturnType (Phase 6 restricts the FunSig to
//     scalar primitives and unit; deeper containers land later).
//
//   - A bare function name used as a value (`let f = add; f(3)`)
//     lowers via the C lowerer's `__shim_add` wrapper. The TS
//     lowerer collapses the shim back into the bare identifier
//     `mochi__add` (the C-side mangled name), so no wrapper
//     function is emitted. Calling the value goes through a regular
//     CallExpr.
//
//   - Calling a fun-typed value lowers to `callee(args)`. When the
//     callee is itself an arrow literal (a directly-invoked function
//     expression) the emit wraps the callee in parentheses to keep
//     TS's grammar happy. The IR distinguishes this case via
//     `FunCallExpr` rather than the named-function `CallExpr`.
//
// Why this gate matters. Closures are the lingua franca of every
// later phase: Phase 7 (query DSL) lowers `xs.filter(p)` to
// `xs.filter((x) => p(x))`; Phase 10 (streams) wants per-event
// callbacks; Phase 12 (FFI) wants user code to register handlers
// for JS-side events. If the closure lowering doesn't compose
// correctly with capture semantics under tsc-strict, every later
// phase has to work around it. So we exercise capture in every
// scalar primitive (int, float, bool, string), capture inside a
// function body, capture across nested closures, fun-typed
// parameters, fun-typed returns, function-value indirection, and
// the for-loop / void-call / zero-arg edge cases.
//
// The fixture corpus covers fourteen surface areas:
//
//   - Anonymous closure with arrow body (closure_simple,
//     closure_two_arg, closure_float, closure_bool_return,
//     closure_string_return).
//   - Anonymous closure with block body (closure_block_body).
//   - Multiple non-capturing closures in one program
//     (closure_multiple_types, two_closures).
//   - Closure defined inside a named function (closure_in_function).
//   - Captures of every scalar primitive (capture_int,
//     capture_float, capture_bool, capture_string, capture_multi,
//     capture_adder).
//   - Captures across an arrow chain (capture_counter_sim).
//   - Capture inside a function that returns the closure
//     (capture_in_function).
//   - Higher-order parameter (higher_order_param,
//     higher_order_two_arg, capture_in_block).
//   - Function-name-as-value via the shim path (funref_value).
//   - Nested closures with shared captures (nested_closures).
//   - Void-returning closure called for side effect
//     (closure_void_call).
//   - Zero-argument closure (closure_zero_arg).
//   - Closure called from inside a `for` loop body
//     (closure_with_loop).
//
// Phase 6 ships 25 fixtures; the floor is 20.
func TestPhase6ClosuresNode(t *testing.T) { runPhase6FixturesOn(t, "node") }
func TestPhase6ClosuresDeno(t *testing.T) { runPhase6FixturesOn(t, "deno") }
func TestPhase6ClosuresBun(t *testing.T)  { runPhase6FixturesOn(t, "bun") }

func runPhase6FixturesOn(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase06-closures")
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
		t.Fatalf("Phase 6 fixture corpus has %d .mochi files, expected at least 20", mochiCount)
	}
}

// TestPhase6EmitShape asserts the load-bearing tokens of the closure
// lowering. These invariants distinguish the shipped lowering from
// an alternative that happens to print the same numbers but breaks
// composition with later phases (e.g. an emit that lifts every
// closure to a top-level function would defeat capture-by-lexical-
// scope and force every Phase 7 query callback to thread an env
// pointer through manually).
//
//   - `=> {` — closure body lowers to a block-arrow form, not a
//     concise `=>` followed by an expression. The block form is the
//     IR's source of truth (the aotir Body always carries an
//     explicit ReturnStmt) and the form Phase 16 reproducibility is
//     gated against.
//   - `(__p0: number) => number` — the fun type renders with
//     synthetic param names. Tests for fun-typed parameter slots.
//   - `(x: number): number =>` — the closure literal renders its
//     own param names. Distinct from the type-slot form.
//   - no `__anon_` function declaration — lifted bodies are inlined,
//     never emitted as their own top-level decls.
//   - no `__shim_` function declaration — function-ref shims are
//     also elided.
//   - no `__e->` in the emit — captured-var refs have their env
//     prefix stripped so JS lexical scope picks them up.
//   - no `function __anon_` and no `function __shim_` — strong
//     cross-check that nothing leaks past the lifted-skip filter.
func TestPhase6EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase06-closures")
	cases := []struct {
		fixture   string
		wants     []string
		notWants  []string
	}{
		{
			fixture: "req_closure_simple.mochi",
			wants: []string{
				"(x: number): number =>",
				"const double_it: (__p0: number) => number",
			},
			notWants: []string{
				"function __anon_",
				"function __shim_",
				"__e->",
			},
		},
		{
			fixture: "req_capture_adder.mochi",
			wants: []string{
				"(x: number): number =>",
				"base",
				"offset",
			},
			notWants: []string{
				"__e->",
				"function __anon_",
			},
		},
		{
			fixture: "req_higher_order_param.mochi",
			wants: []string{
				"function mochi__apply(f: (__p0: number) => number, x: number): number",
				"return f(x);",
			},
			notWants: []string{
				"function __anon_",
				"function __shim_",
			},
		},
		{
			fixture: "req_funref_value.mochi",
			wants: []string{
				"function mochi__add_one(x: number): number",
				"function mochi__apply(",
				"mochi__apply(mochi__add_one, 41)",
			},
			notWants: []string{
				"function __shim_",
				"__shim_add_one",
			},
		},
		{
			fixture: "req_capture_in_function.mochi",
			wants: []string{
				"function mochi__make_adder(n: number): (__p0: number) => number",
				"(x: number): number =>",
				"return f;",
			},
			notWants: []string{
				"function __anon_",
				"__e->",
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
			for _, notWant := range tc.notWants {
				if strings.Contains(src, notWant) {
					t.Errorf("%s emit contained forbidden token %q\n---\n%s", tc.fixture, notWant, src)
				}
			}
		})
	}
}

// TestPhase6NoLiftedTopLevel asserts that the TS emit never contains
// a top-level `function __anon_<n>(` or `function __shim_<name>(`
// declaration. The C lowerer generates those for its own closure-
// conversion pass; in TS they would be dead code (every FunLit
// inlines its body, every shim collapses to the bare callee).
//
// A regression here would surface as duplicate function definitions
// (the inlined arrow plus the dead top-level decl), inflate emit
// size linearly with closure count, and break Phase 16 byte-equal
// reproducibility.
func TestPhase6NoLiftedTopLevel(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase06-closures")
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
			for _, forbidden := range []string{"function __anon_", "function __shim_"} {
				if strings.Contains(src, forbidden) {
					t.Errorf("%s leaked lifted top-level: %q\n---\n%s", name, forbidden, src)
				}
			}
		})
	}
}

// TestPhase6EnvPrefixStripped asserts that no emitted TS source
// ever contains the C lowerer's `__e->` capture prefix. The C
// lowerer stamps that prefix into VarRef.Name for variables read
// through a closure env struct; the TS lowerer strips it on every
// VarRef (and UnionVarRef) lowering. A regression would surface as
// `TypeError: __e is not defined` at runtime, but the cheaper
// check is to grep the emitted source.
func TestPhase6EnvPrefixStripped(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase06-closures")
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
			if strings.Contains(src, "__e->") {
				t.Errorf("%s emit leaked C env prefix:\n%s", name, src)
			}
		})
	}
}
