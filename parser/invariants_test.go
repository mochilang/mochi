package parser

import (
	"strings"
	"testing"
)

// TestInvariants_StatementMultiArm hand-builds a Statement with two arms set
// and asserts the validator rejects it. The exported parser never produces
// this shape; the test exists so a future refactor that breaks the
// tagged-union encoding fails loudly here instead of corrupting downstream
// consumers.
func TestInvariants_StatementMultiArm(t *testing.T) {
	prog := &Program{
		Statements: []*Statement{{
			Let: &LetStmt{Name: "x"},
			Var: &VarStmt{Name: "y"},
		}},
	}
	err := assertProgramInvariants(prog)
	if err == nil {
		t.Fatalf("expected invariant violation, got nil")
	}
	if !strings.Contains(err.Error(), "P070") {
		t.Fatalf("expected error code P070, got %q", err.Error())
	}
	if !strings.Contains(err.Error(), "2 arms") {
		t.Fatalf("expected message mentioning 2 arms, got %q", err.Error())
	}
}

// TestInvariants_StatementNoArm covers the dual case: a Statement with all
// arms nil. Normalise should never produce one, but if it ever did, the
// downstream type-checker would silently treat it as a no-op.
func TestInvariants_StatementNoArm(t *testing.T) {
	prog := &Program{Statements: []*Statement{{}}}
	err := assertProgramInvariants(prog)
	if err == nil {
		t.Fatalf("expected invariant violation, got nil")
	}
	if !strings.Contains(err.Error(), "P070") {
		t.Fatalf("expected P070, got %q", err.Error())
	}
	if !strings.Contains(err.Error(), "0 arms") {
		t.Fatalf("expected message mentioning 0 arms, got %q", err.Error())
	}
}

// TestInvariants_TypeDeclSingleVariantNotLifted asserts the validator catches
// a TypeDecl that still carries SingleVariant. Normalise lifts it into
// Variants; the assertion guarantees no consumer ever sees the raw form.
func TestInvariants_TypeDeclSingleVariantNotLifted(t *testing.T) {
	prog := &Program{
		Statements: []*Statement{{
			Type: &TypeDecl{
				Name:          "T",
				SingleVariant: &TypeVariantHead{Name: "V"},
			},
		}},
	}
	err := assertProgramInvariants(prog)
	if err == nil {
		t.Fatalf("expected invariant violation, got nil")
	}
	if !strings.Contains(err.Error(), "SingleVariant") {
		t.Fatalf("expected SingleVariant in message, got %q", err.Error())
	}
}

// TestInvariants_TypeRefMultiArm guards the four-way tagged union on
// TypeRef. The grammar admits each arm only through a distinct production;
// a malformed normaliser could in principle set two simultaneously.
func TestInvariants_TypeRefMultiArm(t *testing.T) {
	name := "int"
	prog := &Program{
		Statements: []*Statement{{
			Let: &LetStmt{
				Name: "x",
				Type: &TypeRef{
					Simple:  &name,
					Generic: &GenericType{Name: "list"},
				},
			},
		}},
	}
	err := assertProgramInvariants(prog)
	if err == nil {
		t.Fatalf("expected invariant violation, got nil")
	}
	if !strings.Contains(err.Error(), "type reference has 2 arms") {
		t.Fatalf("expected type-reference arm error, got %q", err.Error())
	}
}

// TestInvariants_ValidProgramsPass round-trips a representative corpus
// through ParseString. The post-parse validator now runs on every program,
// so any false positive in the assertions surfaces here.
func TestInvariants_ValidProgramsPass(t *testing.T) {
	srcs := []string{
		`let x: int = 1`,
		`var y = 2`,
		`fun id<T>(x: T): T { return x }`,
		`type Color = Red | Green | Blue`,
		`type Pt = { x: int, y: int }`,
		`type IdList = list<int>`,
		`if true { } else if false { }`,
		`for i in 0..10 { let z = i }`,
		`let xs = [1, 2, 3]`,
		`let m = {a: 1, b: 2}`,
		`let r = a + -b`,
		`let s = match x { 0 => "zero" 1 => "one" }`,
	}
	for _, src := range srcs {
		t.Run(src, func(t *testing.T) {
			if _, err := ParseString(src); err != nil {
				t.Fatalf("ParseString(%q): %v", src, err)
			}
		})
	}
}
