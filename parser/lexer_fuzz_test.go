package parser_test

import (
	"testing"

	"mochi/parser"
)

// FuzzLexer exercises Tokenize with arbitrary input. The lexer must never
// panic and must terminate on every byte sequence shorter than the
// implicit length limit applied by go test's fuzz engine.
//
// Seed corpus is deliberately small but covers every token class plus
// the historical paper cuts (block comment edge cases, adjacent number+
// ident, base prefix without digits, BOM).
func FuzzLexer(f *testing.F) {
	seeds := []string{
		"",
		"let x = 1",
		"// comment\n# also a comment\n/* block */",
		"/***/", "/*a**/", "/* * */",
		"\"a\\nb\\t\\x41\"",
		"0xFF 0b1010 0o7 42 3.14 1e10",
		"== != <= >= && || => :- ..",
		"true false null all",
		"from where by select sort order distinct join",
		"λ αβγ 🦀",
		";;;;",
		"\xef\xbb\xbflet x = 1",
		"package foo\nlet y = 2",
	}
	for _, s := range seeds {
		f.Add(s)
	}
	f.Fuzz(func(t *testing.T, src string) {
		// Tokenize must either succeed or return an error. It must not
		// panic, hang, or produce nil-with-nil-error.
		toks, err := parser.Tokenize("fuzz", src)
		if err == nil && toks == nil && src != "" {
			t.Fatalf("Tokenize returned (nil, nil) for %q", src)
		}
	})
}

// FuzzParseString covers the full parse pipeline including post-lex
// participle behaviour. The seed corpus is built from MEP 2's grammar
// audit: every shape the audit exercised (both the silent-failure cases
// that are now diagnostics, and the previously rejected cases that are
// now accepted) is represented so a regression in any production shows
// up here first.
//
// The constraint is the same as FuzzLexer: do not panic and do not return
// (nil, nil).
func FuzzParseString(f *testing.F) {
	seeds := []string{
		// Basic statements
		"let x = 1",
		"var x: int = 1",
		"fun f(x: int): int { return x + 1 }",
		"if true { } else { }",
		"while false { }",
		"for i in 0..10 { }",
		"return",
		"break",
		"continue",

		// Type declarations: every shape MEP 2 recognises
		"type Id = int",
		"type IdList = list<int>",
		"type M = map<string, int>",
		"type Fn = fun(int): string",
		"type Point { x: int, y: int }",
		"type Pt = { x: int, y: int }",
		"type Color = Red | Green | Blue",
		"type Pair = P(a: int, b: int)",
		"type Shape = Circle(r: float) | Square(s: float)",

		// Postfix ops: interleavings the audit exposed
		"let r = xs[0]",
		"let r = xs[1:3]",
		"let r = xs[::2]",
		"let r = a.b[0]",
		"let r = a[0].b",
		"let r = f(1)(2)",

		// Match
		"let r = match v { 0 => \"zero\" _ => \"other\" }",
		"let r = match v { 0 => { let x = 1\nx } }",

		// Queries
		"from x in xs select x",
		"from x in xs where x > 0 select x",
		"from x in xs join y in ys on x == y select x",
		"from x in xs group by x into g select g",

		// Literals and containers
		"let xs = [1, 2, 3,]",
		"let m = {a: 1, b: 2}",
		"let s = \"hello \\n world\"",
		"let n = 0xff",
		"let f = 1.5e-2",

		// Edge cases from the audit
		"let r = xs[]",
		"let r = xs[:]",
		"let r = match v { 0 => }",
	}
	for _, s := range seeds {
		f.Add(s)
	}
	f.Fuzz(func(t *testing.T, src string) {
		prog, err := parser.ParseString(src)
		if err == nil && prog == nil {
			t.Fatalf("ParseString returned (nil, nil) for %q", src)
		}
	})
}
