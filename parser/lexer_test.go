package parser_test

import (
	"strings"
	"testing"

	"mochi/parser"
)

// tok is a compact representation of a token for table-driven assertions.
// It elides the source filename and offset (they are checked separately by
// dedicated position tests).
type tok struct {
	kind   string
	value  string
	line   int
	column int
}

func tokenize(t *testing.T, src string) []parser.Token {
	t.Helper()
	got, err := parser.Tokenize("", src)
	if err != nil {
		t.Fatalf("Tokenize(%q): unexpected error: %v", src, err)
	}
	return got
}

func assertTokens(t *testing.T, src string, want []tok) {
	t.Helper()
	got := tokenize(t, src)
	if len(got) != len(want) {
		t.Fatalf("Tokenize(%q): got %d tokens, want %d\n  got:  %#v\n  want: %#v", src, len(got), len(want), got, want)
	}
	for i, g := range got {
		w := want[i]
		if g.Kind != w.kind || g.Value != w.value || g.Pos.Line != w.line || g.Pos.Column != w.column {
			t.Errorf("Tokenize(%q)[%d] = %s %q @ %d:%d; want %s %q @ %d:%d",
				src, i, g.Kind, g.Value, g.Pos.Line, g.Pos.Column,
				w.kind, w.value, w.line, w.column)
		}
	}
}

// --- Token classes ---

func TestLexer_BooleanLiteral(t *testing.T) {
	assertTokens(t, `true false`, []tok{
		{"Bool", "true", 1, 1},
		{"Whitespace", " ", 1, 5},
		{"Bool", "false", 1, 6},
	})
}

func TestLexer_HardKeywords(t *testing.T) {
	// Every keyword listed in MEP 1 must lex as Keyword, not Ident.
	keywords := []string{
		"test", "expect", "agent", "intent", "on", "stream", "emit",
		"type", "fun", "extern", "import", "return", "break", "continue",
		"let", "var", "if", "else", "then", "for", "while", "in",
		"generate", "match", "fetch", "load", "save", "package",
		"export", "fact", "rule", "all", "null",
	}
	for _, kw := range keywords {
		got := tokenize(t, kw)
		if len(got) != 1 || got[0].Kind != "Keyword" || got[0].Value != kw {
			t.Errorf("%q did not lex as a single Keyword token: %#v", kw, got)
		}
	}
}

func TestLexer_SoftKeywordsAreIdentifiers(t *testing.T) {
	// MEP 1 declares these as soft keywords: outside their production
	// they must lex as ordinary identifiers.
	soft := []string{
		"bench", "model", "update",
		"from", "where", "select", "group", "by", "into", "having",
		"sort", "order", "skip", "take", "distinct", "join",
		"left", "right", "outer",
		"as", "to", "with",
		"union", "except", "intersect",
	}
	for _, w := range soft {
		got := tokenize(t, w)
		if len(got) != 1 || got[0].Kind != "Ident" || got[0].Value != w {
			t.Errorf("soft keyword %q must lex as Ident, got %#v", w, got)
		}
	}
}

func TestLexer_IdentifierShapes(t *testing.T) {
	cases := []struct {
		src  string
		want string
	}{
		{"x", "x"},
		{"_", "_"},
		{"_abc", "_abc"},
		{"abc_123", "abc_123"},
		{"λ", "λ"},
		{"αβγ", "αβγ"},
		{"🦀", "🦀"},
	}
	for _, c := range cases {
		got := tokenize(t, c.src)
		if len(got) != 1 || got[0].Kind != "Ident" || got[0].Value != c.want {
			t.Errorf("ident %q lexed unexpectedly: %#v", c.src, got)
		}
	}
}

func TestLexer_IntegerLiterals(t *testing.T) {
	cases := []struct{ src, want string }{
		{"0", "0"},
		{"7", "7"},
		{"42", "42"},
		{"007", "007"}, // documented: leading zeros allowed, no octal semantics.
		{"0x0", "0x0"},
		{"0xFF", "0xFF"},
		{"0Xff", "0Xff"},
		{"0b0", "0b0"},
		{"0b1010", "0b1010"},
		{"0B11", "0B11"},
		{"0o7", "0o7"},
		{"0o777", "0o777"},
		{"0O17", "0O17"},
	}
	for _, c := range cases {
		got := tokenize(t, c.src)
		if len(got) != 1 || got[0].Kind != "Int" || got[0].Value != c.want {
			t.Errorf("int %q lexed unexpectedly: %#v", c.src, got)
		}
	}
}

func TestLexer_FloatLiterals(t *testing.T) {
	cases := []string{
		"1.0", "0.5", "3.14", "1e10", "1E10", "1e+10", "1e-10",
		"1.5e2", "1.5E+2", "1.5e-2",
	}
	for _, c := range cases {
		got := tokenize(t, c)
		if len(got) != 1 || got[0].Kind != "Float" || got[0].Value != c {
			t.Errorf("float %q lexed unexpectedly: %#v", c, got)
		}
	}
}

func TestLexer_StringLiterals(t *testing.T) {
	cases := []string{
		`""`,
		`"abc"`,
		`"a\"b"`,
		`"\\"`,
		`"\n\r\t"`,
		`"\x41"`,
		`"¶"`,
	}
	for _, c := range cases {
		got := tokenize(t, c)
		if len(got) != 1 || got[0].Kind != "String" || got[0].Value != c {
			t.Errorf("string %q lexed unexpectedly: %#v", c, got)
		}
	}
}

func TestLexer_Punctuation(t *testing.T) {
	cases := []struct{ src, kind string }{
		{"==", "=="}, {"!=", "!="}, {"<=", "<="}, {">=", ">="},
		{"&&", "&&"}, {"||", "||"}, {"=>", "=>"}, {":-", ":-"},
		{"..", ".."},
		{"+", "+"}, {"-", "-"}, {"*", "*"}, {"/", "/"}, {"%", "%"},
		{"=", "="}, {"<", "<"}, {">", ">"}, {"!", "!"}, {"|", "|"},
		{"{", "{"}, {"}", "}"}, {"[", "["}, {"]", "]"},
		{"(", "("}, {")", ")"}, {",", ","}, {".", "."}, {":", ":"},
	}
	for _, c := range cases {
		got := tokenize(t, c.src)
		if len(got) != 1 || got[0].Kind != "Punct" || got[0].Value != c.kind {
			t.Errorf("punct %q lexed unexpectedly: %#v", c.src, got)
		}
	}
}

// --- Comment forms ---

func TestLexer_LineComments(t *testing.T) {
	got := tokenize(t, "// hi there\n# also hi\nx")
	wantKinds := []string{"Comment", "Whitespace", "Comment", "Whitespace", "Ident"}
	if len(got) != len(wantKinds) {
		t.Fatalf("expected %d tokens, got %d (%#v)", len(wantKinds), len(got), got)
	}
	for i, g := range got {
		if g.Kind != wantKinds[i] {
			t.Errorf("token %d: kind %s, want %s", i, g.Kind, wantKinds[i])
		}
	}
}

func TestLexer_BlockComments(t *testing.T) {
	// Each input must lex as exactly one Comment token. /***/ and /*a**/
	// regressed previously because the rule was /\*([^*]|\*+[^*/])*\*/.
	for _, src := range []string{
		`/**/`,
		`/* hi */`,
		`/***/`,
		`/*a**/`,
		`/* * */`,
		`/* / */`,
		`/*\n*/`,
	} {
		got := tokenize(t, src)
		if len(got) != 1 || got[0].Kind != "Comment" || got[0].Value != src {
			t.Errorf("block comment %q did not lex as single Comment token: %#v", src, got)
		}
	}
}

func TestLexer_BlockCommentDoesNotNest(t *testing.T) {
	// Documented behaviour: the first `*/` terminates the comment.
	got := tokenize(t, `/* outer /* inner */`)
	if len(got) == 0 || got[0].Kind != "Comment" {
		t.Fatalf("expected leading Comment token, got %#v", got)
	}
	if got[0].Value != `/* outer /* inner */` {
		t.Errorf("expected outermost first-close behaviour, got %q", got[0].Value)
	}
}

// --- Position fidelity ---

func TestLexer_PositionsAcrossLines(t *testing.T) {
	src := "let x = 1\nlet y = 2"
	got := tokenize(t, src)
	// Find the `2` token and check its position.
	var two *parser.Token
	for i := range got {
		if got[i].Kind == "Int" && got[i].Value == "2" {
			two = &got[i]
			break
		}
	}
	if two == nil {
		t.Fatal("expected to find token `2`")
	}
	if two.Pos.Line != 2 || two.Pos.Column != 9 {
		t.Errorf("expected `2` at 2:9, got %d:%d", two.Pos.Line, two.Pos.Column)
	}
}

func TestLexer_TabIsOneColumn(t *testing.T) {
	// Mochi columns are byte counts from the start of the line; a tab
	// counts as one column.
	got := tokenize(t, "\tlet x = 1")
	first := got[0]
	if first.Kind != "Whitespace" || first.Pos.Column != 1 {
		t.Errorf("expected leading tab at column 1, got %#v", first)
	}
	// `let` should be at column 2.
	for _, g := range got {
		if g.Kind == "Keyword" && g.Value == "let" {
			if g.Pos.Column != 2 {
				t.Errorf("expected `let` at column 2 after tab, got column %d", g.Pos.Column)
			}
			return
		}
	}
	t.Fatal("did not find `let` keyword")
}

// --- BOM handling ---

func TestLexer_StripsLeadingBOM(t *testing.T) {
	src := "\xef\xbb\xbflet x = 1"
	if _, err := parser.ParseString(src); err != nil {
		t.Fatalf("BOM-prefixed source failed to parse: %v", err)
	}
}

func TestLexer_BOMOnlySourceIsValid(t *testing.T) {
	if _, err := parser.ParseString("\xef\xbb\xbf"); err != nil {
		t.Fatalf("BOM-only source failed to parse: %v", err)
	}
}

// --- Error paths ---

func mustFail(t *testing.T, label, src, codeSubstr string) {
	t.Helper()
	_, err := parser.ParseString(src)
	if err == nil {
		t.Fatalf("%s: expected error, got nil", label)
	}
	if !strings.Contains(err.Error(), codeSubstr) {
		t.Fatalf("%s: error %q does not mention %q", label, err.Error(), codeSubstr)
	}
}

func TestLexer_UnterminatedBlockComment(t *testing.T) {
	mustFail(t, "unterminated /*", "/* foo bar", "P042")
	mustFail(t, "unterminated /* at EOF after lines", "/* foo\nbar", "P042")
}

func TestLexer_UnterminatedString(t *testing.T) {
	mustFail(t, "unterminated string", `let s = "abc`, "P040")
	mustFail(t, "trailing backslash", `let s = "abc\`, "P040")
}

func TestLexer_NumberAdjacentIdent(t *testing.T) {
	for _, src := range []string{
		`let n = 1e`,
		`let n = 1_000`,
		`let n = 3.14abc`,
		`let n = 0xFFG`,
		`let n = 123foo`,
	} {
		mustFail(t, src, src, "P043")
	}
}

func TestLexer_IncompleteBasePrefix(t *testing.T) {
	for _, src := range []string{
		`let n = 0x`,
		`let n = 0X`,
		`let n = 0b`,
		`let n = 0o`,
		`let n = 0xy`, // 0x with no hex digits but a trailing identifier.
	} {
		mustFail(t, src, src, "P046")
	}
}

func TestLexer_IntegerOverflow(t *testing.T) {
	mustFail(t, "decimal overflow", `let n = 9999999999999999999`, "P045")
	mustFail(t, "hex overflow", `let n = 0xFFFFFFFFFFFFFFFFFF`, "P045")
}

func TestLexer_InvalidEscapeSequence(t *testing.T) {
	// Mochi's escape set is the Go strconv set. \q is not in it.
	mustFail(t, "bad escape", `let s = "a\qb"`, "P041")
}

// --- Soft keyword as identifier in real programs ---

func TestLexer_SoftKeywordsAcceptedInBindings(t *testing.T) {
	// Snapshot regression for the bench/from/by/union family. None of
	// these should be reserved at the global level.
	for _, name := range []string{"from", "where", "by", "union", "bench", "model", "update"} {
		src := "let " + name + " = 1"
		if _, err := parser.ParseString(src); err != nil {
			t.Errorf("soft keyword %q cannot be bound by let: %v", name, err)
		}
	}
}

// --- Semicolons as whitespace ---

func TestLexer_SemicolonIsWhitespace(t *testing.T) {
	got := tokenize(t, ";;;")
	if len(got) != 1 || got[0].Kind != "Whitespace" || got[0].Value != ";;;" {
		t.Errorf("expected `;;;` to lex as a single Whitespace token, got %#v", got)
	}
}

// --- Pinned regressions ---

func TestLexer_IndexExpressionParsesAsSubtraction(t *testing.T) {
	// xs[len(xs)-1] must parse cleanly: the lexer must split `1` and `-`
	// so the parser sees subtraction inside the index.
	src := `fun last(xs: list<int>): int { return xs[len(xs)-1] }`
	if _, err := parser.ParseString(src); err != nil {
		t.Fatalf("len(xs)-1 should parse as subtraction: %v", err)
	}
}
