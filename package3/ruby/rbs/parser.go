package rbs

import (
	"archive/tar"
	"compress/gzip"
	"io"
	"io/fs"
	"path/filepath"
	"strings"
	"unicode"
)

// ---- Tokenizer ---------------------------------------------------------------

// tokKind is the kind of a lexical token in an RBS source file.
type tokKind int

const (
	tokIdent    tokKind = iota // identifier or keyword
	tokColon                   // :
	tokArrow                   // ->
	tokLParen                  // (
	tokRParen                  // )
	tokLBracket                // [
	tokRBracket                // ]
	tokComma                   // ,
	tokPipe                    // |
	tokQuestion                // ?
	tokStar                    // *
	tokCaret                   // ^
	tokBang                    // !
	tokDot                     // .
	tokNewline                 // \n
	tokEOF
)

type token struct {
	kind tokKind
	text string
}

// tokenize converts an RBS source string into a flat token list.
// Comments (#…) are stripped. The last token is always tokEOF.
func tokenize(src string) []token {
	var tokens []token
	i := 0
	for i < len(src) {
		c := src[i]
		switch {
		case c == '#':
			// Strip comment to end of line.
			for i < len(src) && src[i] != '\n' {
				i++
			}
		case c == '\n':
			tokens = append(tokens, token{kind: tokNewline, text: "\n"})
			i++
		case c == ' ' || c == '\t' || c == '\r':
			i++
		case c == '(':
			tokens = append(tokens, token{kind: tokLParen, text: "("})
			i++
		case c == ')':
			tokens = append(tokens, token{kind: tokRParen, text: ")"})
			i++
		case c == '[':
			tokens = append(tokens, token{kind: tokLBracket, text: "["})
			i++
		case c == ']':
			tokens = append(tokens, token{kind: tokRBracket, text: "]"})
			i++
		case c == ',':
			tokens = append(tokens, token{kind: tokComma, text: ","})
			i++
		case c == '|':
			tokens = append(tokens, token{kind: tokPipe, text: "|"})
			i++
		case c == '?':
			tokens = append(tokens, token{kind: tokQuestion, text: "?"})
			i++
		case c == '*':
			tokens = append(tokens, token{kind: tokStar, text: "*"})
			i++
		case c == '^':
			tokens = append(tokens, token{kind: tokCaret, text: "^"})
			i++
		case c == '!':
			tokens = append(tokens, token{kind: tokBang, text: "!"})
			i++
		case c == '.':
			tokens = append(tokens, token{kind: tokDot, text: "."})
			i++
		case c == '-' && i+1 < len(src) && src[i+1] == '>':
			tokens = append(tokens, token{kind: tokArrow, text: "->"})
			i += 2
		case c == ':' && i+1 < len(src) && src[i+1] == ':':
			// :: treated as part of an identifier; scan ahead to collect qualified name
			// (this case falls through to the ident scanner below)
			j := i
			for j < len(src) {
				if src[j] == ':' && j+1 < len(src) && src[j+1] == ':' {
					j += 2
					for j < len(src) && (isIdentChar(rune(src[j]))) {
						j++
					}
				} else {
					break
				}
			}
			tokens = append(tokens, token{kind: tokIdent, text: src[i:j]})
			i = j
		case c == ':':
			tokens = append(tokens, token{kind: tokColon, text: ":"})
			i++
		case unicode.IsLetter(rune(c)) || c == '_':
			j := i
			for j < len(src) && isIdentChar(rune(src[j])) {
				j++
			}
			// Absorb :: continuations for qualified names.
			for j+1 < len(src) && src[j] == ':' && src[j+1] == ':' {
				j += 2
				for j < len(src) && isIdentChar(rune(src[j])) {
					j++
				}
			}
			tokens = append(tokens, token{kind: tokIdent, text: src[i:j]})
			i = j
		default:
			// Skip unrecognised characters.
			i++
		}
	}
	tokens = append(tokens, token{kind: tokEOF, text: ""})
	return tokens
}

func isIdentChar(r rune) bool {
	return unicode.IsLetter(r) || unicode.IsDigit(r) || r == '_'
}

// ---- Parser ------------------------------------------------------------------

type rbsParser struct {
	tokens []token
	pos    int
}

func (p *rbsParser) peek() token {
	for p.pos < len(p.tokens) && p.tokens[p.pos].kind == tokNewline {
		p.pos++
	}
	if p.pos >= len(p.tokens) {
		return token{kind: tokEOF}
	}
	return p.tokens[p.pos]
}

func (p *rbsParser) peekRaw() token {
	if p.pos >= len(p.tokens) {
		return token{kind: tokEOF}
	}
	return p.tokens[p.pos]
}

func (p *rbsParser) consume() token {
	t := p.peek()
	p.pos++
	// skip over newlines that peek skipped
	for p.pos < len(p.tokens) && p.tokens[p.pos-1].kind == tokNewline {
		break
	}
	return t
}

func (p *rbsParser) consumeRaw() token {
	if p.pos >= len(p.tokens) {
		return token{kind: tokEOF}
	}
	t := p.tokens[p.pos]
	p.pos++
	return t
}

func (p *rbsParser) skipNewlines() {
	for p.pos < len(p.tokens) && p.tokens[p.pos].kind == tokNewline {
		p.pos++
	}
}

func (p *rbsParser) skipToNewline() {
	for p.pos < len(p.tokens) && p.tokens[p.pos].kind != tokNewline && p.tokens[p.pos].kind != tokEOF {
		p.pos++
	}
}

// parseFile parses a full RBS file, collecting all class/module declarations.
func (p *rbsParser) parseFile(parentPrefix string) []ClassDecl {
	var classes []ClassDecl
	for {
		p.skipNewlines()
		t := p.peek()
		if t.kind == tokEOF {
			break
		}
		if t.kind != tokIdent {
			p.skipToNewline()
			continue
		}
		switch t.text {
		case "class", "module", "interface":
			p.consume() // eat class/module/interface
			cd, nested := p.parseClassDecl(parentPrefix)
			classes = append(classes, cd)
			classes = append(classes, nested...)
		default:
			p.skipToNewline()
		}
	}
	return classes
}

// parseClassDecl parses everything after the `class`/`module` keyword up to matching `end`.
func (p *rbsParser) parseClassDecl(parentPrefix string) (ClassDecl, []ClassDecl) {
	// Read qualified class name
	nameToken := p.peek()
	var name string
	if nameToken.kind == tokIdent {
		p.consume()
		name = nameToken.text
	}
	if parentPrefix != "" {
		name = parentPrefix + "::" + name
	}

	// Skip superclass / generic params etc. on the same logical line
	p.skipInheritanceLine()

	var cd ClassDecl
	cd.Name = name
	var nested []ClassDecl

	for {
		p.skipNewlines()
		t := p.peek()
		if t.kind == tokEOF {
			break
		}
		if t.kind == tokIdent && t.text == "end" {
			p.consume()
			break
		}
		if t.kind != tokIdent {
			p.skipToNewline()
			continue
		}
		switch t.text {
		case "def":
			p.consume()
			m, ok := p.parseMethodDecl()
			if ok {
				cd.Methods = append(cd.Methods, m)
			}
		case "attr_reader", "attr_accessor", "attr_writer":
			p.consume()
			a, ok := p.parseAttrDecl()
			if ok {
				cd.Attrs = append(cd.Attrs, a)
			}
		case "class", "module", "interface":
			p.consume()
			inner, innerNested := p.parseClassDecl(name)
			nested = append(nested, inner)
			nested = append(nested, innerNested...)
		default:
			p.skipToNewline()
		}
	}
	return cd, nested
}

// skipInheritanceLine skips tokens on the class declaration header line
// (superclass, generic params, etc.) until the next newline.
func (p *rbsParser) skipInheritanceLine() {
	for p.pos < len(p.tokens) {
		t := p.tokens[p.pos]
		if t.kind == tokNewline || t.kind == tokEOF {
			break
		}
		p.pos++
	}
}

// parseMethodDecl parses a method declaration after the `def` keyword.
// Returns the MethodDecl and true on success, or false to skip.
func (p *rbsParser) parseMethodDecl() (MethodDecl, bool) {
	var md MethodDecl

	// Check for self. prefix
	t := p.peek()
	if t.kind == tokIdent && t.text == "self" {
		p.consume()
		dot := p.peek()
		if dot.kind == tokDot {
			p.consume()
		} else {
			// "self" as method name or unexpected syntax; skip.
			p.skipToNewline()
			return md, false
		}
		md.Kind = MethodSingleton
	} else {
		md.Kind = MethodInstance
	}

	// Method name
	nameToken := p.peek()
	if nameToken.kind != tokIdent {
		p.skipToNewline()
		return md, false
	}
	p.consume()
	md.Name = nameToken.text

	// Absorb trailing ? or ! into method name
	if p.pos < len(p.tokens) {
		next := p.tokens[p.pos]
		if next.kind == tokQuestion || next.kind == tokBang {
			md.Name += next.text
			p.pos++
		}
	}

	// Expect ':'
	if p.peek().kind != tokColon {
		p.skipToNewline()
		return md, false
	}
	p.consume()

	// Parse overloads — first overload may be on same line, additional ones
	// start with '|' at beginning of the next non-empty line.
	overload, ok := p.parseOverload()
	if !ok {
		p.skipToNewline()
		return md, false
	}
	md.Types = append(md.Types, overload)

	// Additional overloads: lines that start with '|'
	for {
		savedPos := p.pos
		p.skipNewlines()
		if p.pos >= len(p.tokens) {
			break
		}
		if p.tokens[p.pos].kind != tokPipe {
			p.pos = savedPos
			break
		}
		p.pos++ // consume '|'
		o2, ok2 := p.parseOverload()
		if !ok2 {
			p.skipToNewline()
			break
		}
		md.Types = append(md.Types, o2)
	}

	return md, true
}

// parseOverload parses a single `(params) -> ReturnType` overload signature.
func (p *rbsParser) parseOverload() (MethodType, bool) {
	var mt MethodType

	// Params list: must start with '('
	if p.peek().kind != tokLParen {
		return mt, false
	}
	p.consume()

	// Parse params until ')'
	for p.peek().kind != tokRParen && p.peek().kind != tokEOF {
		param, ok := p.parseParam()
		if !ok {
			// Skip to ')' and continue.
			for p.peek().kind != tokRParen && p.peek().kind != tokEOF {
				p.consume()
			}
			break
		}
		mt.Params = append(mt.Params, param)
		if p.peek().kind == tokComma {
			p.consume()
		}
	}
	if p.peek().kind == tokRParen {
		p.consume()
	}

	// Arrow
	if p.peek().kind != tokArrow {
		return mt, false
	}
	p.consume()

	// Return type
	retType, ok := p.parseType()
	if !ok {
		return mt, false
	}
	mt.Return = retType
	return mt, true
}

// parseParam parses one parameter: [*] Type name
func (p *rbsParser) parseParam() (Param, bool) {
	var param Param

	// Splat
	if p.peek().kind == tokStar {
		p.consume()
	}

	t, ok := p.parseType()
	if !ok {
		return param, false
	}
	param.Type = t

	// Optional parameter name (an identifier that is NOT a type keyword)
	if p.peek().kind == tokIdent && !isTypeKeyword(p.peek().text) {
		nameToken := p.consume()
		param.Name = nameToken.text
		// Absorb trailing ? for optional param names like `foo?`
		if p.pos < len(p.tokens) && p.tokens[p.pos].kind == tokQuestion {
			param.Name += "?"
			p.pos++
			param.Optional = true
		}
	} else if param.Name == "" {
		// Unnamed parameter — generate a synthetic name
		param.Name = "arg"
	}

	return param, true
}

// parseType parses a single RBS type expression.
func (p *rbsParser) parseType() (*Type, bool) {
	t := p.peek()
	if t.kind == tokEOF {
		return nil, false
	}

	var baseType *Type

	switch {
	case t.kind == tokLParen:
		// Parenthesised type or union: (T | nil) / (nil | T)
		p.consume()
		inner, ok := p.parseType()
		if !ok {
			return nil, false
		}
		if p.peek().kind == tokPipe {
			p.consume()
			other, ok2 := p.parseType()
			if !ok2 {
				return nil, false
			}
			baseType = collapseUnion(inner, other)
		} else {
			baseType = inner
		}
		if p.peek().kind == tokRParen {
			p.consume()
		}

	case t.kind == tokLBracket:
		// Tuple: [A, B, C]
		p.consume()
		var members []*Type
		for p.peek().kind != tokRBracket && p.peek().kind != tokEOF {
			m, ok := p.parseType()
			if !ok {
				return nil, false
			}
			members = append(members, m)
			if p.peek().kind == tokComma {
				p.consume()
			}
		}
		if p.peek().kind == tokRBracket {
			p.consume()
		}
		baseType = &Type{Kind: TypeTuple, Members: members}

	case t.kind == tokCaret:
		// Proc: ^(A, B) -> R
		p.consume()
		if p.peek().kind != tokLParen {
			return nil, false
		}
		p.consume()
		var paramTypes []*Type
		for p.peek().kind != tokRParen && p.peek().kind != tokEOF {
			pt, ok := p.parseType()
			if !ok {
				return nil, false
			}
			paramTypes = append(paramTypes, pt)
			// Absorb optional param name
			if p.peek().kind == tokIdent && !isTypeKeyword(p.peek().text) {
				p.consume()
			}
			if p.peek().kind == tokComma {
				p.consume()
			}
		}
		if p.peek().kind == tokRParen {
			p.consume()
		}
		if p.peek().kind != tokArrow {
			return nil, false
		}
		p.consume()
		ret, ok := p.parseType()
		if !ok {
			return nil, false
		}
		baseType = &Type{Kind: TypeProc, Params: paramTypes, Return: ret}

	case t.kind == tokIdent:
		p.consume()
		baseType = identToType(t.text)
		// Parameterized types: Array[T], Hash[K, V]
		if p.peek().kind == tokLBracket {
			p.consume()
			if baseType.Kind == TypeArray {
				elem, ok := p.parseType()
				if ok {
					baseType.Elem = elem
				}
				p.skipUntil(tokRBracket)
			} else if baseType.Kind == TypeHash {
				key, ok := p.parseType()
				if ok {
					baseType.Key = key
				}
				if p.peek().kind == tokComma {
					p.consume()
				}
				val, ok := p.parseType()
				if ok {
					baseType.Value = val
				}
				p.skipUntil(tokRBracket)
			} else {
				// Generic type we can't translate — skip params.
				p.skipUntil(tokRBracket)
			}
			if p.peek().kind == tokRBracket {
				p.consume()
			}
		}

	default:
		return nil, false
	}

	// Trailing ? — makes optional
	if p.pos < len(p.tokens) && p.tokens[p.pos].kind == tokQuestion {
		p.pos++
		if baseType.Kind == TypeNil {
			// nil? is just nil
		} else {
			baseType = &Type{Kind: TypeOptional, Elem: baseType}
		}
	}

	return baseType, true
}

func (p *rbsParser) skipUntil(k tokKind) {
	depth := 0
	for p.pos < len(p.tokens) {
		t := p.tokens[p.pos]
		if t.kind == tokLBracket {
			depth++
		} else if t.kind == tokRBracket {
			if depth == 0 {
				return
			}
			depth--
		} else if t.kind == k && depth == 0 {
			return
		} else if t.kind == tokEOF {
			return
		}
		p.pos++
	}
}

// parseAttrDecl parses an attr_reader/attr_accessor/attr_writer declaration.
func (p *rbsParser) parseAttrDecl() (MethodDecl, bool) {
	var md MethodDecl
	md.IsAttr = true
	md.Kind = MethodInstance

	nameToken := p.peek()
	if nameToken.kind != tokIdent {
		p.skipToNewline()
		return md, false
	}
	p.consume()
	md.Name = nameToken.text

	if p.peek().kind != tokColon {
		p.skipToNewline()
		return md, false
	}
	p.consume()

	attrType, ok := p.parseType()
	if !ok {
		p.skipToNewline()
		return md, false
	}
	md.Types = []MethodType{{Return: attrType}}
	return md, true
}

// ---- Type helpers ------------------------------------------------------------

// identToType converts an RBS type name token to a Type node.
func identToType(name string) *Type {
	switch name {
	case "Integer", "int":
		return &Type{Kind: TypeInteger}
	case "Float", "float":
		return &Type{Kind: TypeFloat}
	case "String", "string":
		return &Type{Kind: TypeString}
	case "Symbol":
		return &Type{Kind: TypeSymbol}
	case "bool", "Boolean", "TrueClass", "FalseClass":
		return &Type{Kind: TypeBool}
	case "nil", "NilClass":
		return &Type{Kind: TypeNil}
	case "void":
		return &Type{Kind: TypeVoid}
	case "untyped":
		return &Type{Kind: TypeUntyped}
	case "top":
		return &Type{Kind: TypeTop}
	case "bot":
		return &Type{Kind: TypeBot}
	case "self":
		return &Type{Kind: TypeSelf}
	case "instance":
		return &Type{Kind: TypeInstance}
	case "class":
		return &Type{Kind: TypeClass}
	case "Array":
		return &Type{Kind: TypeArray}
	case "Hash":
		return &Type{Kind: TypeHash}
	default:
		return &Type{Kind: TypeNamed, Name: name}
	}
}

// isTypeKeyword returns true for tokens that are RBS type names, not param names.
func isTypeKeyword(s string) bool {
	switch s {
	case "Integer", "Float", "String", "Symbol", "bool", "Boolean",
		"TrueClass", "FalseClass", "nil", "NilClass", "void", "untyped",
		"top", "bot", "self", "instance", "class", "Array", "Hash",
		"int", "float", "string":
		return true
	}
	// Capitalised word is likely a type name.
	if len(s) > 0 && s[0] >= 'A' && s[0] <= 'Z' {
		return true
	}
	return false
}

// collapseUnion collapses a 2-arm union into Optional when one arm is nil.
func collapseUnion(a, b *Type) *Type {
	if a.Kind == TypeNil {
		return &Type{Kind: TypeOptional, Elem: b}
	}
	if b.Kind == TypeNil {
		return &Type{Kind: TypeOptional, Elem: a}
	}
	// Multi-way union — TypeUnknown signals the typemap to skip it.
	return &Type{Kind: TypeUnknown}
}

// ---- Public API --------------------------------------------------------------

// parseRBSTexts parses a slice of RBS source strings and returns all
// class/module declarations found across them. Unknown or unrecognised
// syntax is silently skipped; no error is returned for invalid RBS.
func parseRBSTexts(texts []string) ([]ClassDecl, error) {
	var all []ClassDecl
	for _, text := range texts {
		toks := tokenize(text)
		p := &rbsParser{tokens: toks}
		classes := p.parseFile("")
		all = append(all, classes...)
	}
	return all, nil
}

// ---- ParseFromTarball / ParseFromDir ----------------------------------------

// ParseFromTarball extracts and parses all .rbs files from a gem tarball
// (data.tar.gz sub-archive inside the .gem file). It returns a GemSurface with
// SourceBundled if any .rbs files are found, SourceNone otherwise.
func ParseFromTarball(gemName, gemVersion string, r io.Reader) (*GemSurface, error) {
	gz, err := gzip.NewReader(r)
	if err != nil {
		return nil, err
	}
	defer gz.Close()

	surface := &GemSurface{
		Gem:     gemName,
		Version: gemVersion,
		Source:  SourceNone,
	}

	tr := tar.NewReader(gz)
	var rbsContents []string
	for {
		hdr, err := tr.Next()
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, err
		}
		if hdr.Typeflag != tar.TypeReg {
			continue
		}
		clean := filepath.Clean(hdr.Name)
		if filepath.Ext(clean) != ".rbs" {
			continue
		}
		dir := filepath.ToSlash(filepath.Dir(clean))
		if !strings.HasPrefix(dir, "sig") && !strings.Contains(dir, "/sig") {
			continue
		}
		data, err := io.ReadAll(tr)
		if err != nil {
			return nil, err
		}
		rbsContents = append(rbsContents, string(data))
	}

	if len(rbsContents) == 0 {
		return surface, nil
	}
	surface.Source = SourceBundled
	classes, err := parseRBSTexts(rbsContents)
	if err != nil {
		return nil, err
	}
	surface.Classes = classes
	return surface, nil
}

// ParseFromDir parses .rbs files from a directory on disk.
func ParseFromDir(gemName, gemVersion, dir string) (*GemSurface, error) {
	surface := &GemSurface{
		Gem:     gemName,
		Version: gemVersion,
		Source:  SourceGemRBSCollection,
	}
	var contents []string
	err := filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || filepath.Ext(path) != ".rbs" {
			return nil
		}
		data, err := io.ReadAll(openFile(path))
		if err != nil {
			return err
		}
		contents = append(contents, string(data))
		return nil
	})
	if err != nil {
		return nil, err
	}
	if len(contents) == 0 {
		surface.Source = SourceNone
		return surface, nil
	}
	classes, err := parseRBSTexts(contents)
	if err != nil {
		return nil, err
	}
	surface.Classes = classes
	return surface, nil
}

// openFile is a thin wrapper so ParseFromDir is testable without os import.
var openFile = func(path string) io.ReadCloser {
	// Implemented in parse_os.go to avoid os import in this file.
	panic("openFile not wired")
}
