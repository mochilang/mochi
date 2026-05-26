package parser

import (
	"fmt"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/alecthomas/participle/v2"
	"github.com/alecthomas/participle/v2/lexer"
)

type boolLit bool

func (b *boolLit) Capture(values []string) error {
	v, err := strconv.ParseBool(values[0])
	if err != nil {
		return err
	}
	*b = boolLit(v)
	return nil
}

type IntLit int

func (i *IntLit) Capture(values []string) error {
	s := values[0]
	base := 10
	if strings.HasPrefix(s, "0x") || strings.HasPrefix(s, "0X") {
		base = 16
		s = s[2:]
	} else if strings.HasPrefix(s, "0b") || strings.HasPrefix(s, "0B") {
		base = 2
		s = s[2:]
	} else if strings.HasPrefix(s, "0o") || strings.HasPrefix(s, "0O") {
		base = 8
		s = s[2:]
	}
	v, err := strconv.ParseInt(s, base, 64)
	if err != nil {
		return err
	}
	*i = IntLit(v)
	return nil
}

// --- Mochi Lexer ---
//
// The rules below are the canonical lexical grammar of Mochi, mirrored in
// MEP 1. Rule order is significant: every rule is tried at the current
// input position and the first one that matches wins. `Bool` precedes
// `Keyword`, `Keyword` precedes `Ident`, otherwise reserved words would
// lex as identifiers.
//
// Numeric literals do not include a leading `-`. Unary minus is a
// separate operator so that `len(list)-1` parses as subtraction even
// inside an index expression like `xs[len(xs)-1]`.
//
// The block comment regex matches the longest run ending at the first
// `*/` and accepts pathological forms such as `/***/` and `/*a**/`. It
// intentionally does not nest; the pre-lex scanner catches unterminated
// block comments with a precise diagnostic.
var mochiLexer = lexer.MustSimple([]lexer.SimpleRule{
	{Name: "Comment", Pattern: `//[^\n]*|#[^\n]*|/\*[^*]*\*+(?:[^/*][^*]*\*+)*/`},
	{Name: "Bool", Pattern: `\b(true|false)\b`},
	{Name: "Keyword", Pattern: `\b(test|expect|agent|intent|on|close|stream|emit|type|fun|extern|import|return|break|continue|let|var|if|else|then|for|while|in|generate|match|when|fetch|load|save|package|export|fact|rule|all|none|try|catch|not|set|omap|async|await|spawn)\b`},
	{Name: "Ident", Pattern: `[\p{L}\p{So}_][\p{L}\p{So}\p{N}_]*`},
	{Name: "Float", Pattern: `\d+\.\d+(?:[eE][+-]?\d+)?|\d+[eE][+-]?\d+`},
	{Name: "Int", Pattern: `0[xX][0-9a-fA-F]+|0[bB][01]+|0[oO][0-7]+|\d+`},
	{Name: "String", Pattern: `"(?:\\.|[^"\\])*"`},
	{Name: "Punct", Pattern: `==|!=|<=|>=|&&|\|\||=>|:-|\.\.|\?\?|[-+*/%=<>!|{}\[\](),.:?]`},
	{Name: "Whitespace", Pattern: `[ \t\n\r;]+`},
})

// --- Parser Instance ---

var Parser = participle.MustBuild[Program](
	participle.Lexer(mochiLexer),
	participle.Elide("Whitespace", "Comment"),
	participle.Unquote("String"),
	participle.UseLookahead(999),
)

func ParseString(src string) (*Program, error) {
	text := trimBOM(src)
	if err := preLexScan("", text); err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	prog, err := Parser.ParseString("", text)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", wrapParseError("", err))
	}
	if err := normalizeProgram(prog); err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	if err := assertProgramInvariants(prog); err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	attachDocs(text, prog)
	return prog, nil
}

// AliasFromPath derives an import alias from a module path.
func AliasFromPath(path string) string {
	base := filepath.Base(strings.Trim(path, "\""))
	ext := filepath.Ext(base)
	return strings.TrimSuffix(base, ext)
}
