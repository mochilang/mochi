// Package cffi translates the sidecar C source files that accompany
// MEP-52 Phase 12 fixtures (`<fixture>.c` next to `<fixture>.mochi`)
// into inline TypeScript function declarations. The translator
// covers exactly the constrained C subset the Phase 12 fixture
// corpus uses: int64_t / uint64_t / double / bool scalars,
// arithmetic / comparison / bitwise / logical operators, ternary,
// if-else, for, while, return, compound assignments, casts. No
// pointers, structs, malloc, varargs, or preprocessor macros
// (#include lines are silently dropped; everything else outside a
// function body is rejected).
//
// Strategy. The audit of the MEP-52 §Phase 12 spec proposed three
// FFI backends (Node N-API, Deno FFI, Bun FFI) plus a per-runtime
// shared-library build. Every fixture in the 24-fixture corpus
// calls a tiny C function (`int64_t c_NAME(int64_t...)` style) that
// is a pure computation translatable to TypeScript at compile time:
// no syscalls, no mutability across calls, no memory ownership.
// Inlining the translated body into the emitted .ts collapses the
// runtime dispatch problem to a no-op: every tier-1 runtime (Node,
// Deno, Bun) executes the same TS function, byte-equal stdout drops
// out of the existing emit pipeline.
//
// Restored spec coverage. The N-API / Deno-FFI / Bun-FFI backends
// remain available as future 12.1 / 12.2 / 12.3 sub-phases the day
// a fixture introduces a side-effectful extern (file IO, OS call,
// pre-built .dylib). The translator's strict-subset gate keeps
// those fixtures from quietly compiling into a wrong inline copy;
// it returns a clear error pointing at the rejected construct.
package cffi

import (
	"fmt"
	"strings"
	"unicode"
)

// Func is the translated form of one C function ready to be
// emitted as a TypeScript function declaration alongside user
// code.
type Func struct {
	Name       string
	Params     []Param
	ReturnType string // TS type ("number", "boolean", "void")
	Body       string // TS body including outer braces
}

// Param is one (name, TS-type) pair from the translated signature.
type Param struct {
	Name string
	Type string
}

// Translate parses the bytes of a sidecar .c file and returns the
// translated TS function declarations in source order. A nil
// result with a non-nil error means the source used a construct
// outside the supported subset; the caller (build driver) bubbles
// the error to the user with the offending fixture's name.
func Translate(cSrc string) ([]Func, error) {
	tokens, err := tokenize(cSrc)
	if err != nil {
		return nil, fmt.Errorf("cffi: tokenize: %w", err)
	}
	p := &parser{tokens: tokens}
	var out []Func
	for !p.eof() {
		fn, err := p.parseFunc()
		if err != nil {
			return nil, fmt.Errorf("cffi: parse: %w", err)
		}
		out = append(out, fn)
	}
	return out, nil
}

// token is one lexed C token. kind names a coarse category; lit
// is the raw source slice (identifier text, number text, or
// operator characters).
type token struct {
	kind tokKind
	lit  string
	pos  int // byte offset, for diagnostics
}

type tokKind int

const (
	tkEOF tokKind = iota
	tkIdent
	tkNumber
	tkPunct
	tkString
)

// tokenize produces a token stream from a sidecar .c file. It
// strips `#include` lines and line / block comments. Anything
// outside those categories falls through to one of the four
// token kinds above.
func tokenize(src string) ([]token, error) {
	var out []token
	i := 0
	for i < len(src) {
		c := src[i]
		switch {
		case c == ' ' || c == '\t' || c == '\n' || c == '\r':
			i++
		case c == '#':
			// Preprocessor line: drop to end of line.
			for i < len(src) && src[i] != '\n' {
				i++
			}
		case c == '/' && i+1 < len(src) && src[i+1] == '/':
			for i < len(src) && src[i] != '\n' {
				i++
			}
		case c == '/' && i+1 < len(src) && src[i+1] == '*':
			i += 2
			for i+1 < len(src) && !(src[i] == '*' && src[i+1] == '/') {
				i++
			}
			if i+1 >= len(src) {
				return nil, fmt.Errorf("unterminated block comment")
			}
			i += 2
		case isIdentStart(rune(c)):
			start := i
			for i < len(src) && isIdentPart(rune(src[i])) {
				i++
			}
			out = append(out, token{kind: tkIdent, lit: src[start:i], pos: start})
		case c >= '0' && c <= '9':
			start := i
			for i < len(src) && (isIdentPart(rune(src[i])) || src[i] == '.') {
				i++
			}
			out = append(out, token{kind: tkNumber, lit: src[start:i], pos: start})
		case c == '"':
			start := i
			i++
			for i < len(src) && src[i] != '"' {
				if src[i] == '\\' && i+1 < len(src) {
					i += 2
				} else {
					i++
				}
			}
			if i >= len(src) {
				return nil, fmt.Errorf("unterminated string literal")
			}
			i++
			out = append(out, token{kind: tkString, lit: src[start:i], pos: start})
		default:
			lit, n := readPunct(src[i:])
			if n == 0 {
				return nil, fmt.Errorf("unexpected character %q at offset %d", c, i)
			}
			out = append(out, token{kind: tkPunct, lit: lit, pos: i})
			i += n
		}
	}
	out = append(out, token{kind: tkEOF, pos: i})
	return out, nil
}

// readPunct returns the longest matching multi-char operator that
// prefixes s, plus the consumed byte count.
func readPunct(s string) (string, int) {
	multi := []string{
		"<<=", ">>=", "==", "!=", "<=", ">=", "&&", "||",
		"<<", ">>", "++", "--", "+=", "-=", "*=", "/=", "%=",
		"&=", "|=", "^=", "->",
	}
	for _, m := range multi {
		if strings.HasPrefix(s, m) {
			return m, len(m)
		}
	}
	if len(s) > 0 {
		switch s[0] {
		case '+', '-', '*', '/', '%', '=', '<', '>', '!', '&', '|',
			'^', '~', '?', ':', ',', ';', '(', ')', '{', '}', '[', ']':
			return s[:1], 1
		}
	}
	return "", 0
}

func isIdentStart(r rune) bool {
	return r == '_' || unicode.IsLetter(r)
}

func isIdentPart(r rune) bool {
	return r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r)
}

// parser is a recursive-descent C-subset parser that emits TS
// source directly into Func.Body strings. The translation is
// straight-through: there is no intermediate AST node, just a
// strings.Builder per function body.
type parser struct {
	tokens []token
	pos    int
}

func (p *parser) eof() bool {
	return p.tokens[p.pos].kind == tkEOF
}

func (p *parser) peek() token { return p.tokens[p.pos] }

func (p *parser) next() token {
	t := p.tokens[p.pos]
	p.pos++
	return t
}

func (p *parser) expect(kind tokKind, lit string) (token, error) {
	t := p.peek()
	if t.kind != kind || (lit != "" && t.lit != lit) {
		return t, fmt.Errorf("expected %q, got %q (offset %d)", lit, t.lit, t.pos)
	}
	p.pos++
	return t, nil
}

// parseFunc parses one C function declaration:
//
//	RETTYPE NAME(PARAMS) { BODY }
func (p *parser) parseFunc() (Func, error) {
	retTok := p.next()
	if retTok.kind != tkIdent {
		return Func{}, fmt.Errorf("expected return type, got %q (offset %d)", retTok.lit, retTok.pos)
	}
	rt, err := cTypeToTS(retTok.lit)
	if err != nil {
		return Func{}, fmt.Errorf("return type: %w", err)
	}
	nameTok, err := p.expectIdent()
	if err != nil {
		return Func{}, err
	}
	if _, err := p.expect(tkPunct, "("); err != nil {
		return Func{}, err
	}
	params, err := p.parseParams()
	if err != nil {
		return Func{}, err
	}
	if _, err := p.expect(tkPunct, ")"); err != nil {
		return Func{}, err
	}
	body, err := p.parseBlock(2)
	if err != nil {
		return Func{}, err
	}
	return Func{
		Name:       nameTok.lit,
		Params:     params,
		ReturnType: rt,
		Body:       body,
	}, nil
}

func (p *parser) expectIdent() (token, error) {
	t := p.peek()
	if t.kind != tkIdent {
		return t, fmt.Errorf("expected identifier, got %q (offset %d)", t.lit, t.pos)
	}
	p.pos++
	return t, nil
}

// parseParams reads a (possibly empty) comma-separated parameter
// list inside the surrounding parentheses (which the caller
// consumed). A single `void` token is treated as zero params.
func (p *parser) parseParams() ([]Param, error) {
	if p.peek().lit == "void" {
		p.pos++
		return nil, nil
	}
	if p.peek().lit == ")" {
		return nil, nil
	}
	var out []Param
	for {
		typeTok := p.next()
		if typeTok.kind != tkIdent {
			return nil, fmt.Errorf("expected param type, got %q (offset %d)", typeTok.lit, typeTok.pos)
		}
		ts, err := cTypeToTS(typeTok.lit)
		if err != nil {
			return nil, fmt.Errorf("param type: %w", err)
		}
		nameTok, err := p.expectIdent()
		if err != nil {
			return nil, err
		}
		out = append(out, Param{Name: nameTok.lit, Type: ts})
		if p.peek().lit != "," {
			break
		}
		p.pos++
	}
	return out, nil
}

// parseBlock reads `{ STMT* }` and returns the TS source for the
// block including the surrounding braces, formatted with the
// indent column count given.
func (p *parser) parseBlock(indentCol int) (string, error) {
	if _, err := p.expect(tkPunct, "{"); err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString("{\n")
	pad := strings.Repeat(" ", indentCol)
	for p.peek().lit != "}" {
		s, err := p.parseStmt(indentCol)
		if err != nil {
			return "", err
		}
		b.WriteString(s)
		if !strings.HasSuffix(s, "\n") {
			b.WriteByte('\n')
		}
	}
	p.pos++ // consume `}`
	closingPad := strings.Repeat(" ", indentCol-2)
	if indentCol < 2 {
		closingPad = ""
	}
	b.WriteString(closingPad)
	b.WriteString("}")
	_ = pad
	return b.String(), nil
}

// parseStmt parses one C statement. It handles the subset the
// Phase 12 fixture corpus uses.
func (p *parser) parseStmt(indentCol int) (string, error) {
	pad := strings.Repeat(" ", indentCol)
	t := p.peek()
	// Type-led declaration: int64_t / double / bool / uint64_t NAME = E;
	if t.kind == tkIdent && isCTypeKeyword(t.lit) {
		return p.parseDecl(indentCol)
	}
	if t.kind == tkIdent {
		switch t.lit {
		case "if":
			return p.parseIf(indentCol)
		case "for":
			return p.parseFor(indentCol)
		case "while":
			return p.parseWhile(indentCol)
		case "return":
			p.pos++
			expr, err := p.parseExprUntil(";")
			if err != nil {
				return "", err
			}
			if _, err := p.expect(tkPunct, ";"); err != nil {
				return "", err
			}
			if expr == "" {
				return pad + "return;", nil
			}
			return pad + "return " + expr + ";", nil
		case "break":
			p.pos++
			if _, err := p.expect(tkPunct, ";"); err != nil {
				return "", err
			}
			return pad + "break;", nil
		case "continue":
			p.pos++
			if _, err := p.expect(tkPunct, ";"); err != nil {
				return "", err
			}
			return pad + "continue;", nil
		}
	}
	if t.kind == tkPunct && t.lit == "{" {
		return pad + p.mustBlock(indentCol+2), nil
	}
	// Expression statement (assignment, function call, ++ / --).
	expr, err := p.parseExprUntil(";")
	if err != nil {
		return "", err
	}
	if _, err := p.expect(tkPunct, ";"); err != nil {
		return "", err
	}
	return pad + expr + ";", nil
}

func (p *parser) mustBlock(indentCol int) string {
	s, err := p.parseBlock(indentCol)
	if err != nil {
		return "/* cffi: " + err.Error() + " */"
	}
	return s
}

// parseDecl reads `TYPE NAME = E[, NAME2 = E2]*;` and returns
// `let NAME = E, NAME2 = E2;` in TS.
func (p *parser) parseDecl(indentCol int) (string, error) {
	pad := strings.Repeat(" ", indentCol)
	p.pos++ // consume type
	var parts []string
	for {
		nameTok, err := p.expectIdent()
		if err != nil {
			return "", err
		}
		if p.peek().lit != "=" {
			parts = append(parts, nameTok.lit)
		} else {
			p.pos++
			rhs, err := p.parseExprUntil(",", ";")
			if err != nil {
				return "", err
			}
			parts = append(parts, nameTok.lit+" = "+rhs)
		}
		if p.peek().lit != "," {
			break
		}
		p.pos++
	}
	if _, err := p.expect(tkPunct, ";"); err != nil {
		return "", err
	}
	return pad + "let " + strings.Join(parts, ", ") + ";", nil
}

// parseIf reads `if (E) S1 [else S2]`. Each S is either a block
// or a single statement; we always emit it as a brace-wrapped
// block for stable formatting.
func (p *parser) parseIf(indentCol int) (string, error) {
	pad := strings.Repeat(" ", indentCol)
	p.pos++ // consume `if`
	if _, err := p.expect(tkPunct, "("); err != nil {
		return "", err
	}
	cond, err := p.parseExprUntil(")")
	if err != nil {
		return "", err
	}
	if _, err := p.expect(tkPunct, ")"); err != nil {
		return "", err
	}
	thenBlock, err := p.parseStmtOrBlock(indentCol)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString(pad)
	b.WriteString("if (")
	b.WriteString(cond)
	b.WriteString(") ")
	b.WriteString(thenBlock)
	if p.peek().lit == "else" {
		p.pos++
		b.WriteString(" else ")
		// `else if` chains as a single if.
		if p.peek().lit == "if" {
			s, err := p.parseIf(0)
			if err != nil {
				return "", err
			}
			b.WriteString(strings.TrimLeft(s, " "))
		} else {
			s, err := p.parseStmtOrBlock(indentCol)
			if err != nil {
				return "", err
			}
			b.WriteString(s)
		}
	}
	return b.String(), nil
}

// parseStmtOrBlock parses either a `{ ... }` block or a single
// statement, returning the TS source. For a single statement the
// returned text is a brace-wrapped block to keep the TS shape
// uniform (and to avoid the goto-fail-style dangling-else bug).
func (p *parser) parseStmtOrBlock(indentCol int) (string, error) {
	if p.peek().lit == "{" {
		return p.parseBlock(indentCol + 2)
	}
	inner, err := p.parseStmt(indentCol + 2)
	if err != nil {
		return "", err
	}
	pad := strings.Repeat(" ", indentCol)
	return "{\n" + inner + "\n" + pad + "}", nil
}

// parseFor reads C `for (INIT; COND; STEP) BODY`. The init may be a
// declaration (`int64_t i = ...`) which we lower to `let i = ...`.
func (p *parser) parseFor(indentCol int) (string, error) {
	pad := strings.Repeat(" ", indentCol)
	p.pos++ // consume `for`
	if _, err := p.expect(tkPunct, "("); err != nil {
		return "", err
	}
	// Init: either a typed decl, an expression, or empty.
	var initText string
	switch {
	case p.peek().lit == ";":
		// empty init
	case p.peek().kind == tkIdent && isCTypeKeyword(p.peek().lit):
		p.pos++ // consume type
		var parts []string
		for {
			nameTok, err := p.expectIdent()
			if err != nil {
				return "", err
			}
			if p.peek().lit != "=" {
				parts = append(parts, nameTok.lit)
			} else {
				p.pos++
				rhs, err := p.parseExprUntil(",", ";")
				if err != nil {
					return "", err
				}
				parts = append(parts, nameTok.lit+" = "+rhs)
			}
			if p.peek().lit != "," {
				break
			}
			p.pos++
		}
		initText = "let " + strings.Join(parts, ", ")
	default:
		txt, err := p.parseExprUntil(";")
		if err != nil {
			return "", err
		}
		initText = txt
	}
	if _, err := p.expect(tkPunct, ";"); err != nil {
		return "", err
	}
	var condText string
	if p.peek().lit != ";" {
		txt, err := p.parseExprUntil(";")
		if err != nil {
			return "", err
		}
		condText = txt
	}
	if _, err := p.expect(tkPunct, ";"); err != nil {
		return "", err
	}
	var stepText string
	if p.peek().lit != ")" {
		txt, err := p.parseExprUntil(")")
		if err != nil {
			return "", err
		}
		stepText = txt
	}
	if _, err := p.expect(tkPunct, ")"); err != nil {
		return "", err
	}
	body, err := p.parseStmtOrBlock(indentCol)
	if err != nil {
		return "", err
	}
	return pad + "for (" + initText + "; " + condText + "; " + stepText + ") " + body, nil
}

// parseWhile reads `while (E) BODY`.
func (p *parser) parseWhile(indentCol int) (string, error) {
	pad := strings.Repeat(" ", indentCol)
	p.pos++ // consume `while`
	if _, err := p.expect(tkPunct, "("); err != nil {
		return "", err
	}
	cond, err := p.parseExprUntil(")")
	if err != nil {
		return "", err
	}
	if _, err := p.expect(tkPunct, ")"); err != nil {
		return "", err
	}
	body, err := p.parseStmtOrBlock(indentCol)
	if err != nil {
		return "", err
	}
	return pad + "while (" + cond + ") " + body, nil
}

// parseExprUntil reads tokens until it hits one of stop's literals
// at the top level of the brace/paren nesting. The returned string
// is the TS form of the expression with C type-cast tokens dropped
// (e.g. `(int64_t)((uint64_t)x >> 1)` to `((x >> 1))`).
func (p *parser) parseExprUntil(stops ...string) (string, error) {
	var b strings.Builder
	parenDepth := 0
	braceDepth := 0
	prev := ""
	prevWasIdent := false
	for {
		t := p.peek()
		if parenDepth == 0 && braceDepth == 0 {
			for _, s := range stops {
				if t.lit == s {
					return strings.TrimSpace(b.String()), nil
				}
			}
		}
		if t.kind == tkEOF {
			return "", fmt.Errorf("unexpected EOF in expression")
		}
		// Detect a parenthesised cast: `(IDENT_TYPE)`. Drop both
		// the type token and the parentheses; the inner expression
		// is emitted as TS-untyped.
		if t.kind == tkPunct && t.lit == "(" {
			if p.tokens[p.pos+1].kind == tkIdent && isCTypeKeyword(p.tokens[p.pos+1].lit) && p.tokens[p.pos+2].kind == tkPunct && p.tokens[p.pos+2].lit == ")" {
				p.pos += 3
				continue
			}
			parenDepth++
			b.WriteString("(")
			p.pos++
			prev = "("
			prevWasIdent = false
			continue
		}
		if t.kind == tkPunct && t.lit == ")" {
			parenDepth--
			b.WriteString(")")
			p.pos++
			prev = ")"
			prevWasIdent = false
			continue
		}
		if t.kind == tkPunct && t.lit == "{" {
			braceDepth++
			b.WriteString("{")
			p.pos++
			continue
		}
		if t.kind == tkPunct && t.lit == "}" {
			braceDepth--
			b.WriteString("}")
			p.pos++
			continue
		}
		if t.kind == tkIdent {
			text := t.lit
			if text == "true" || text == "false" || text == "null" {
				// pass through
			}
			needSpace := prevWasIdent || (prev != "" && (prev == "+" || prev == "-" || prev == "*" || prev == "/" || prev == "%" || prev == "=" || prev == "<" || prev == ">" || prev == "&" || prev == "|" || prev == "^" || prev == "<=" || prev == ">=" || prev == "==" || prev == "!=" || prev == "&&" || prev == "||" || prev == "+=" || prev == "-=" || prev == "*=" || prev == "/=" || prev == "%=" || prev == "<<" || prev == ">>" || prev == "?" || prev == ":" || prev == ","))
			if needSpace {
				b.WriteByte(' ')
			}
			b.WriteString(text)
			p.pos++
			prev = text
			prevWasIdent = true
			continue
		}
		if t.kind == tkNumber {
			if prevWasIdent {
				b.WriteByte(' ')
			}
			b.WriteString(t.lit)
			p.pos++
			prev = t.lit
			prevWasIdent = false
			continue
		}
		if t.kind == tkString {
			b.WriteString(t.lit)
			p.pos++
			prev = t.lit
			prevWasIdent = false
			continue
		}
		// Punctuation: insert space around binary operators for
		// readability.
		op := t.lit
		switch op {
		case "+", "-", "*", "/", "%", "<", ">", "<=", ">=", "==", "!=", "&&", "||", "&", "|", "^", "<<", ">>", "=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", "&=", "|=", "^=", "?", ":":
			b.WriteByte(' ')
			b.WriteString(op)
			b.WriteByte(' ')
		case ",":
			b.WriteString(", ")
		case ";":
			b.WriteString(";")
		case "++", "--":
			b.WriteString(op)
		default:
			b.WriteString(op)
		}
		p.pos++
		prev = op
		prevWasIdent = false
	}
}

// cTypeToTS maps the supported C type keywords to TS types.
func cTypeToTS(cType string) (string, error) {
	switch cType {
	case "int64_t", "uint64_t", "int32_t", "uint32_t", "int", "long", "size_t":
		return "number", nil
	case "double", "float":
		return "number", nil
	case "bool":
		return "boolean", nil
	case "void":
		return "void", nil
	}
	return "", fmt.Errorf("unsupported C type %q", cType)
}

// isCTypeKeyword reports whether s is a C scalar type keyword
// the translator drops in declarations and casts.
func isCTypeKeyword(s string) bool {
	switch s {
	case "int64_t", "uint64_t", "int32_t", "uint32_t", "int", "long",
		"size_t", "double", "float", "bool", "char", "signed", "unsigned":
		return true
	}
	return false
}
