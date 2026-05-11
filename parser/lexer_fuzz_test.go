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
// participle behaviour. The constraint is the same as FuzzLexer: do not
// panic and do not return (nil, nil).
func FuzzParseString(f *testing.F) {
	f.Add("let x = 1")
	f.Add("fun f(x: int): int { return x + 1 }")
	f.Add("type Point { x: int, y: int }")
	f.Add("from x in xs select x")
	f.Add("match v { 0 => 1 1 => 2 _ => 0 }")
	f.Fuzz(func(t *testing.T, src string) {
		prog, err := parser.ParseString(src)
		if err == nil && prog == nil {
			t.Fatalf("ParseString returned (nil, nil) for %q", src)
		}
	})
}
