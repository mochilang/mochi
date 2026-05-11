package parser_test

import (
	"strings"
	"testing"

	"mochi/parser"
)

// TestGrammar_TypeDeclShapes locks in the four allowed top-level shapes of
// `type` declarations. Each input is small enough that a regression points
// at exactly one production. The cases were chosen from the audit that
// motivated MEP 2 and from the failure modes MEP 2 promises to fix.
func TestGrammar_TypeDeclShapes(t *testing.T) {
	cases := []struct {
		name string
		src  string
		// expected outcome on the first (and only) type decl:
		isStruct  bool
		isAlias   bool
		variants  []string // expected variant names if not alias/struct
	}{
		{
			name:     "bare identifier is an alias, not a single variant",
			src:      `type Id = int`,
			isAlias:  true,
		},
		{
			name:     "generic identifier is an alias",
			src:      `type IdList = list<int>`,
			isAlias:  true,
		},
		{
			name:     "map type is an alias",
			src:      `type M = map<string, int>`,
			isAlias:  true,
		},
		{
			name:     "function type is an alias",
			src:      `type Fn = fun(int): string`,
			isAlias:  true,
		},
		{
			name:     "struct body with leading equals",
			src:      `type Pt = { x: int, y: int }`,
			isStruct: true,
		},
		{
			name:     "struct body without leading equals",
			src:      `type Pt { x: int, y: int }`,
			isStruct: true,
		},
		{
			name:     "multi variant without fields",
			src:      `type Color = Red | Green | Blue`,
			variants: []string{"Red", "Green", "Blue"},
		},
		{
			name:     "multi variant with fields",
			src:      `type Shape = Circle(r: float) | Square(s: float)`,
			variants: []string{"Circle", "Square"},
		},
		{
			name:     "single variant with fields",
			src:      `type Pair = P(a: int, b: int)`,
			variants: []string{"P"},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			prog, err := parser.ParseString(tc.src)
			if err != nil {
				t.Fatalf("ParseString(%q): %v", tc.src, err)
			}
			if len(prog.Statements) != 1 || prog.Statements[0].Type == nil {
				t.Fatalf("expected a single TypeDecl statement, got %#v", prog.Statements)
			}
			td := prog.Statements[0].Type
			if td.SingleVariant != nil {
				t.Fatalf("normalize did not lift SingleVariant: %#v", td.SingleVariant)
			}
			switch {
			case tc.isStruct:
				if len(td.Members) == 0 {
					t.Fatalf("expected Members to be set; got td=%+v", td)
				}
				if td.Alias != nil || len(td.Variants) != 0 {
					t.Fatalf("expected pure struct, got Alias=%v Variants=%v", td.Alias, td.Variants)
				}
			case tc.isAlias:
				if td.Alias == nil {
					t.Fatalf("expected Alias to be set; td=%+v", td)
				}
				if len(td.Members) != 0 || len(td.Variants) != 0 {
					t.Fatalf("expected pure alias, got Members=%v Variants=%v", td.Members, td.Variants)
				}
			default:
				if len(td.Variants) != len(tc.variants) {
					t.Fatalf("variant count mismatch: got %d, want %d (variants=%+v)", len(td.Variants), len(tc.variants), td.Variants)
				}
				for i, want := range tc.variants {
					if td.Variants[i].Name != want {
						t.Errorf("variants[%d].Name = %q, want %q", i, td.Variants[i].Name, want)
					}
				}
				if td.Alias != nil {
					t.Errorf("expected Alias=nil for variant decl, got %+v", td.Alias)
				}
			}
		})
	}
}

// TestGrammar_IndexAndSliceShapes is the conformance suite for the
// `xs[...]` postfix operator. Subscript and slice forms must succeed; the
// silent-empty forms exposed by the audit must produce P060.
func TestGrammar_IndexAndSliceShapes(t *testing.T) {
	good := []string{
		`let r = xs[0]`,
		`let r = xs[i+1]`,
		`let r = xs[1:]`,
		`let r = xs[:3]`,
		`let r = xs[1:3]`,
		`let r = xs[::2]`,
		`let r = xs[1::2]`,
		`let r = xs[:3:2]`,
		`let r = xs[1:3:2]`,
		`let r = m["k"]`,
		`let r = a[i][j]`,
	}
	for _, src := range good {
		t.Run("ok:"+src, func(t *testing.T) {
			if _, err := parser.ParseString(src); err != nil {
				t.Fatalf("expected ok, got %v", err)
			}
		})
	}
	bad := []struct {
		name string
		src  string
	}{
		{"empty subscript", `let r = xs[]`},
		{"empty slice", `let r = xs[:]`},
		{"double colon empty", `let r = xs[::]`},
		{"assign with empty index", `var a = [1]
a[] = 2`},
	}
	for _, tc := range bad {
		t.Run("err:"+tc.name, func(t *testing.T) {
			_, err := parser.ParseString(tc.src)
			if err == nil {
				t.Fatalf("expected P060 error, got success")
			}
			if !strings.Contains(err.Error(), "P060") {
				t.Errorf("expected error code P060, got %v", err)
			}
		})
	}
}

// TestGrammar_MatchCaseBody locks in that every `match` arm must carry
// either a result expression or a block body. Never both empty.
func TestGrammar_MatchCaseBody(t *testing.T) {
	good := []string{
		`let v = 0
let r = match v {
  0 => "zero"
  _ => "other"
}`,
		`let v = 0
let r = match v { 0 => { 1 } }`,
		`let v = 0
let r = match v {
  0 => 1
  1 => 2
}`,
	}
	for _, src := range good {
		t.Run("ok", func(t *testing.T) {
			if _, err := parser.ParseString(src); err != nil {
				t.Fatalf("expected ok for %q, got %v", src, err)
			}
		})
	}
	bad := []string{
		`let v = 0
let r = match v { 0 => }`,
		`let v = 0
let r = match v {
  0 =>
  1 => 1
}`,
	}
	for _, src := range bad {
		t.Run("err", func(t *testing.T) {
			_, err := parser.ParseString(src)
			if err == nil {
				t.Fatalf("expected error for %q, got success", src)
			}
		})
	}
}

// TestGrammar_StatementShapes is a smoke test for every top-level
// statement form. If any of these stop parsing we want to know loudly.
func TestGrammar_StatementShapes(t *testing.T) {
	cases := []struct {
		name string
		src  string
	}{
		{"let with type and value", `let x: int = 1`},
		{"let without value", `let x: int`},
		{"var with type and value", `var x: int = 1`},
		{"assign field", `var a = {b: 0}
a.b = 1`},
		{"assign index", `var a = [0]
a[0] = 1`},
		{"if-then-else", `if true { } else { }`},
		{"while", `while false { }`},
		{"for-in", `for i in [1, 2] { }`},
		{"for-range", `for i in 0..10 { }`},
		{"return empty", `fun f() { return }`},
		{"return value", `fun f(): int { return 1 }`},
		{"fun with type params", `fun id<T>(x: T): T { return x }`},
		{"break/continue", `for i in 0..1 { break
continue }`},
		{"test block", `test "t" { expect 1 == 1 }`},
		{"bench block", `bench "b" { let x = 1 }`},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if _, err := parser.ParseString(tc.src); err != nil {
				t.Fatalf("ParseString(%q): %v", tc.src, err)
			}
		})
	}
}

// TestGrammar_LiteralShapes covers literal and primary expression forms
// to catch regressions in container syntax and trailing-comma handling.
func TestGrammar_LiteralShapes(t *testing.T) {
	cases := []string{
		`let xs = [1, 2, 3]`,
		`let xs = [1, 2, 3,]`,
		`let xs = []`,
		`let m = {a: 1, b: 2}`,
		`let m = {a: 1,}`,
		`let m = {}`,
		`let s = "hello"`,
		`let n = 0xff`,
		`let n = 0b1010`,
		`let n = 0o17`,
		`let f = 1.5`,
		`let f = 1e10`,
		`let b = true`,
		`let z = null`,
	}
	for _, src := range cases {
		t.Run(src, func(t *testing.T) {
			if _, err := parser.ParseString(src); err != nil {
				t.Fatalf("ParseString(%q): %v", src, err)
			}
		})
	}
}

// TestGrammar_UnaryOnBinaryRHS locks in the symmetry between the left and
// right operands of binary operators. The textbook layered precedence
// grammar (Aho-Sethi-Ullman, Crafting Interpreters) requires every binary
// operand to be a unary-expression. Mochi flattens the binary tower into
// a list and climbs precedence post-parse, but the invariant must still
// hold: any unary prefix accepted on the LHS must also be accepted on the
// RHS. Before MEP 2 the RHS was a PostfixExpr, so `a + -b` failed to parse.
// This test guarantees the asymmetry never returns.
func TestGrammar_UnaryOnBinaryRHS(t *testing.T) {
	cases := []struct {
		name string
		src  string
		// rhsOps is the expected unary prefix sequence on the RHS of the
		// first BinaryOp. Nil means "no BinaryOp expected".
		rhsOps []string
	}{
		{"minus on RHS of +", `let r = a + -b`, []string{"-"}},
		{"minus on RHS of -", `let r = a - -b`, []string{"-"}},
		{"minus on RHS of *", `let r = a * -b`, []string{"-"}},
		{"minus on RHS of /", `let r = a / -b`, []string{"-"}},
		{"minus on RHS of %", `let r = a % -b`, []string{"-"}},
		{"minus on RHS of <", `let r = a < -b`, []string{"-"}},
		{"minus on RHS of ==", `let r = a == -b`, []string{"-"}},
		{"not on RHS of &&", `let r = a && !b`, []string{"!"}},
		{"not on RHS of ||", `let r = a || !b`, []string{"!"}},
		{"double not on RHS", `let r = a || !!b`, []string{"!", "!"}},
		{"minus then not on RHS", `let r = a + -!b`, []string{"-", "!"}},
		{"no prefix on RHS still works", `let r = a + b`, nil},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			prog, err := parser.ParseString(tc.src)
			if err != nil {
				t.Fatalf("ParseString(%q): %v", tc.src, err)
			}
			if len(prog.Statements) != 1 || prog.Statements[0].Let == nil {
				t.Fatalf("expected a single let statement, got %#v", prog.Statements)
			}
			expr := prog.Statements[0].Let.Value
			if expr == nil || expr.Binary == nil {
				t.Fatalf("expected a binary expression value")
			}
			if len(expr.Binary.Right) != 1 {
				t.Fatalf("expected exactly one BinaryOp, got %d", len(expr.Binary.Right))
			}
			rhs := expr.Binary.Right[0].Right
			if rhs == nil {
				t.Fatalf("BinaryOp.Right is nil")
			}
			if len(rhs.Ops) != len(tc.rhsOps) {
				t.Fatalf("rhs ops = %v, want %v", rhs.Ops, tc.rhsOps)
			}
			for i, op := range tc.rhsOps {
				if rhs.Ops[i] != op {
					t.Fatalf("rhs ops[%d] = %q, want %q", i, rhs.Ops[i], op)
				}
			}
		})
	}
}

// TestGrammar_BinaryOperandsAreSymmetric verifies the invariant that any
// expression accepted as `LHS op RHS` is also accepted as `RHS op LHS`
// when the operator is commutative and unary prefixes are allowed. This
// is the structural test for the symmetry restored by MEP 2.
func TestGrammar_BinaryOperandsAreSymmetric(t *testing.T) {
	primaries := []string{
		"b",
		"-b",
		"!b",
		"--b",
		"-!b",
	}
	for _, p1 := range primaries {
		for _, p2 := range primaries {
			src := "let r = " + p1 + " + " + p2
			t.Run(src, func(t *testing.T) {
				if _, err := parser.ParseString(src); err != nil {
					t.Fatalf("ParseString(%q): %v", src, err)
				}
			})
		}
	}
}

// TestGrammar_UnaryRHSPrecedence verifies that unary minus on the RHS of
// a binary operator binds tighter than the binary operator itself, so
// `a + -b * c` parses as `a + ((-b) * c)`, not `(a + -b) * c` or other
// shapes. This is the standard layered-grammar treatment of unary.
func TestGrammar_UnaryRHSPrecedence(t *testing.T) {
	// We parse `a + -b * c` and check that the RHS of `+` is a single
	// Unary whose Value is the postfix `b`, and that the BinaryOp list
	// contains both `+` and `*` so the post-parse precedence pass binds
	// `-b * c` tighter than `a + ...`.
	prog, err := parser.ParseString(`let r = a + -b * c`)
	if err != nil {
		t.Fatalf("ParseString: %v", err)
	}
	be := prog.Statements[0].Let.Value.Binary
	if len(be.Right) != 2 {
		t.Fatalf("expected 2 BinaryOps, got %d", len(be.Right))
	}
	if be.Right[0].Op != "+" || be.Right[1].Op != "*" {
		t.Fatalf("ops = %q, %q; want +, *", be.Right[0].Op, be.Right[1].Op)
	}
	if got := strings.Join(be.Right[0].Right.Ops, ""); got != "-" {
		t.Fatalf("first RHS unary ops = %q, want -", got)
	}
	if len(be.Right[1].Right.Ops) != 0 {
		t.Fatalf("second RHS should have no unary prefix")
	}
}
