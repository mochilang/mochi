package apisurface

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"
)

// GoType is the typed bridge-side representation of a Go type
// expression as it appears in ApiSurface JSON. The set of concrete
// implementations is closed; phase 5 (type mapping) and phase 6
// (wrapper synthesiser) switch over them.
type GoType interface {
	isGoType()
	// String renders the type back to canonical source form. The
	// output is byte-equal to the input for any string that
	// successfully parsed (modulo non-canonical whitespace).
	String() string
}

// BasicType names a Go predeclared type: int, int8..int64, uint,
// uint8..uint64, uintptr, float32, float64, complex64, complex128,
// string, bool, byte, rune, error, any, comparable.
type BasicType struct {
	Name string
}

// NamedType references a named type. PackagePath is the import path
// of the declaring package; an empty PackagePath means the same
// package as the surface being parsed. TypeArgs are the generic
// instantiation arguments, if any.
type NamedType struct {
	PackagePath string
	Name        string
	TypeArgs    []GoType
}

// PointerType is *T.
type PointerType struct {
	Elem GoType
}

// SliceType is []T.
type SliceType struct {
	Elem GoType
}

// ArrayType is [N]T. Len is the literal element count.
type ArrayType struct {
	Len  int64
	Elem GoType
}

// MapType is map[K]V.
type MapType struct {
	Key, Value GoType
}

// ChanDir is the direction of a channel type.
type ChanDir int

const (
	ChanBoth ChanDir = iota
	ChanSend
	ChanRecv
)

// ChanType is chan T, <-chan T, or chan<- T.
type ChanType struct {
	Dir  ChanDir
	Elem GoType
}

// FuncType is func(P1, P2) (R1, R2). Variadic is true when the last
// element of Params is the ... parameter; in that case Params'
// final element is wrapped in EllipsisType.
type FuncType struct {
	Params  []GoType
	Results []GoType
	Variadic bool
}

// EllipsisType is ...T -- legal only as the last parameter of a
// FuncType. The parser handles it in that position only.
type EllipsisType struct {
	Elem GoType
}

// InterfaceType is a literal interface{...} body. Phase 4 keeps the
// raw source form; phase 5 unfolds it if needed.
type InterfaceType struct {
	Source string
}

// StructType is a literal struct{...} body. Same as InterfaceType
// in spirit -- the body is kept raw because anonymous struct/iface
// types are rare and full re-parsing belongs in phase 5.
type StructType struct {
	Source string
}

func (BasicType) isGoType()     {}
func (NamedType) isGoType()     {}
func (PointerType) isGoType()   {}
func (SliceType) isGoType()     {}
func (ArrayType) isGoType()     {}
func (MapType) isGoType()       {}
func (ChanType) isGoType()      {}
func (FuncType) isGoType()      {}
func (EllipsisType) isGoType()  {}
func (InterfaceType) isGoType() {}
func (StructType) isGoType()    {}

func (t BasicType) String() string { return t.Name }

func (t NamedType) String() string {
	var sb strings.Builder
	if t.PackagePath != "" {
		sb.WriteString(t.PackagePath)
		sb.WriteByte('.')
	}
	sb.WriteString(t.Name)
	if len(t.TypeArgs) > 0 {
		sb.WriteByte('[')
		for i, a := range t.TypeArgs {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(a.String())
		}
		sb.WriteByte(']')
	}
	return sb.String()
}

func (t PointerType) String() string { return "*" + t.Elem.String() }
func (t SliceType) String() string   { return "[]" + t.Elem.String() }
func (t ArrayType) String() string   { return "[" + strconv.FormatInt(t.Len, 10) + "]" + t.Elem.String() }
func (t MapType) String() string     { return "map[" + t.Key.String() + "]" + t.Value.String() }

func (t ChanType) String() string {
	switch t.Dir {
	case ChanSend:
		return "chan<- " + t.Elem.String()
	case ChanRecv:
		return "<-chan " + t.Elem.String()
	default:
		return "chan " + t.Elem.String()
	}
}

func (t FuncType) String() string {
	var sb strings.Builder
	sb.WriteString("func(")
	for i, p := range t.Params {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(p.String())
	}
	sb.WriteByte(')')
	if len(t.Results) == 0 {
		return sb.String()
	}
	sb.WriteByte(' ')
	if len(t.Results) == 1 {
		if _, isFunc := t.Results[0].(FuncType); !isFunc {
			sb.WriteString(t.Results[0].String())
			return sb.String()
		}
	}
	sb.WriteByte('(')
	for i, r := range t.Results {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(r.String())
	}
	sb.WriteByte(')')
	return sb.String()
}

func (t EllipsisType) String() string  { return "..." + t.Elem.String() }
func (t InterfaceType) String() string { return t.Source }
func (t StructType) String() string    { return t.Source }

// basicSet is the predeclared identifier set. Used by the parser to
// classify bare identifiers without package qualifier.
var basicSet = map[string]bool{
	"bool": true, "byte": true, "complex64": true, "complex128": true,
	"error": true, "float32": true, "float64": true, "int": true,
	"int8": true, "int16": true, "int32": true, "int64": true,
	"rune": true, "string": true, "uint": true, "uint8": true,
	"uint16": true, "uint32": true, "uint64": true, "uintptr": true,
	"any": true, "comparable": true,
}

// ParseType parses a single Go type expression. Returns ErrTypeParse
// on syntax error.
func ParseType(s string) (GoType, error) {
	p := newTypeParser(s)
	t, err := p.parseType()
	if err != nil {
		return nil, err
	}
	p.skipSpaces()
	if p.pos < len(p.src) {
		return nil, fmt.Errorf("%w: trailing input %q after position %d", ErrTypeParse, p.src[p.pos:], p.pos)
	}
	return t, nil
}

// ErrTypeParse is returned for any malformed type expression.
var ErrTypeParse = fmt.Errorf("apisurface: type parse error")

type typeParser struct {
	src string
	pos int
}

func newTypeParser(s string) *typeParser { return &typeParser{src: s} }

func (p *typeParser) errf(format string, args ...any) error {
	return fmt.Errorf("%w: %s (at %d in %q)", ErrTypeParse, fmt.Sprintf(format, args...), p.pos, p.src)
}

func (p *typeParser) skipSpaces() {
	for p.pos < len(p.src) {
		r, sz := utf8.DecodeRuneInString(p.src[p.pos:])
		if !unicode.IsSpace(r) {
			return
		}
		p.pos += sz
	}
}

func (p *typeParser) peekByte() byte {
	p.skipSpaces()
	if p.pos >= len(p.src) {
		return 0
	}
	return p.src[p.pos]
}

func (p *typeParser) consume(s string) bool {
	p.skipSpaces()
	if strings.HasPrefix(p.src[p.pos:], s) {
		p.pos += len(s)
		return true
	}
	return false
}

func (p *typeParser) expect(s string) error {
	if !p.consume(s) {
		return p.errf("expected %q", s)
	}
	return nil
}

// parseType is the top-level grammar entry.
//
//	Type := '*' Type
//	      | '[' ']' Type                   (slice)
//	      | '[' Number ']' Type            (array)
//	      | 'map' '[' Type ']' Type
//	      | 'chan' Type | '<-' 'chan' Type | 'chan' '<-' Type
//	      | 'func' Signature
//	      | 'interface' '{' ... '}' (kept raw)
//	      | 'struct' '{' ... '}' (kept raw)
//	      | '...' Type (only legal mid-FuncType param list)
//	      | NamedOrBasic
func (p *typeParser) parseType() (GoType, error) {
	p.skipSpaces()
	if p.pos >= len(p.src) {
		return nil, p.errf("unexpected end of input")
	}

	// Pointer.
	if p.consume("*") {
		elem, err := p.parseType()
		if err != nil {
			return nil, err
		}
		return PointerType{Elem: elem}, nil
	}

	// Send-direction prefix: "<-chan T".
	if p.consume("<-") {
		if !p.consume("chan") {
			return nil, p.errf("expected 'chan' after '<-'")
		}
		elem, err := p.parseType()
		if err != nil {
			return nil, err
		}
		return ChanType{Dir: ChanRecv, Elem: elem}, nil
	}

	// Slice / array.
	if p.consume("[") {
		if p.consume("]") {
			elem, err := p.parseType()
			if err != nil {
				return nil, err
			}
			return SliceType{Elem: elem}, nil
		}
		// Array.
		n, err := p.parseNumber()
		if err != nil {
			return nil, err
		}
		if err := p.expect("]"); err != nil {
			return nil, err
		}
		elem, err := p.parseType()
		if err != nil {
			return nil, err
		}
		return ArrayType{Len: n, Elem: elem}, nil
	}

	// Word-prefixed forms.
	if ident, ok := p.tryKeyword("map"); ok {
		_ = ident
		if err := p.expect("["); err != nil {
			return nil, err
		}
		key, err := p.parseType()
		if err != nil {
			return nil, err
		}
		if err := p.expect("]"); err != nil {
			return nil, err
		}
		val, err := p.parseType()
		if err != nil {
			return nil, err
		}
		return MapType{Key: key, Value: val}, nil
	}
	if _, ok := p.tryKeyword("chan"); ok {
		dir := ChanBoth
		if p.consume("<-") {
			dir = ChanSend
		}
		elem, err := p.parseType()
		if err != nil {
			return nil, err
		}
		return ChanType{Dir: dir, Elem: elem}, nil
	}
	if _, ok := p.tryKeyword("func"); ok {
		return p.parseFuncSig()
	}
	if _, ok := p.tryKeyword("interface"); ok {
		body, err := p.parseBraceBody()
		if err != nil {
			return nil, err
		}
		return InterfaceType{Source: "interface" + body}, nil
	}
	if _, ok := p.tryKeyword("struct"); ok {
		body, err := p.parseBraceBody()
		if err != nil {
			return nil, err
		}
		return StructType{Source: "struct" + body}, nil
	}

	// Ellipsis (legal only inside a func signature param list; the
	// caller handles that).
	if p.consume("...") {
		elem, err := p.parseType()
		if err != nil {
			return nil, err
		}
		return EllipsisType{Elem: elem}, nil
	}

	// Named or basic.
	return p.parseNamed()
}

// parseFuncSig parses the signature that follows the 'func' keyword.
func (p *typeParser) parseFuncSig() (GoType, error) {
	if err := p.expect("("); err != nil {
		return nil, err
	}
	var params []GoType
	variadic := false
	if !p.consume(")") {
		for {
			if p.consume("...") {
				elem, err := p.parseType()
				if err != nil {
					return nil, err
				}
				params = append(params, EllipsisType{Elem: elem})
				variadic = true
				if !p.consume(",") {
					break
				}
				continue
			}
			t, err := p.parseType()
			if err != nil {
				return nil, err
			}
			params = append(params, t)
			if !p.consume(",") {
				break
			}
		}
		if err := p.expect(")"); err != nil {
			return nil, err
		}
	}
	// Results: may be absent, single, or "(T1, T2)".
	p.skipSpaces()
	var results []GoType
	if p.pos < len(p.src) {
		if p.consume("(") {
			if !p.consume(")") {
				for {
					t, err := p.parseType()
					if err != nil {
						return nil, err
					}
					results = append(results, t)
					if !p.consume(",") {
						break
					}
				}
				if err := p.expect(")"); err != nil {
					return nil, err
				}
			}
		} else if p.canStartType() {
			t, err := p.parseType()
			if err != nil {
				return nil, err
			}
			results = []GoType{t}
		}
	}
	return FuncType{Params: params, Results: results, Variadic: variadic}, nil
}

// canStartType reports whether the next non-space byte could begin a
// Type. Used to disambiguate "func() // no result" from "func() T".
func (p *typeParser) canStartType() bool {
	b := p.peekByte()
	if b == 0 {
		return false
	}
	// Result lists never start with these tokens.
	switch b {
	case ')', ',', ']', '}':
		return false
	}
	return true
}

// parseBraceBody captures a balanced-brace body for interface{...} or
// struct{...}, returning the body inclusive of the braces.
func (p *typeParser) parseBraceBody() (string, error) {
	if err := p.expect("{"); err != nil {
		return "", err
	}
	start := p.pos
	depth := 1
	for p.pos < len(p.src) {
		switch p.src[p.pos] {
		case '{':
			depth++
		case '}':
			depth--
			if depth == 0 {
				body := p.src[start:p.pos]
				p.pos++
				return "{" + body + "}", nil
			}
		}
		p.pos++
	}
	return "", p.errf("unbalanced braces")
}

func (p *typeParser) parseNumber() (int64, error) {
	p.skipSpaces()
	start := p.pos
	for p.pos < len(p.src) && p.src[p.pos] >= '0' && p.src[p.pos] <= '9' {
		p.pos++
	}
	if p.pos == start {
		return 0, p.errf("expected number")
	}
	n, err := strconv.ParseInt(p.src[start:p.pos], 10, 64)
	if err != nil {
		return 0, p.errf("bad number: %v", err)
	}
	return n, nil
}

// parseNamed parses a basic ident, a "selector" form, or a generic
// instantiation. Forms accepted:
//
//	ident                              -> BasicType or NamedType (self)
//	import/path.ident                  -> NamedType{import/path, ident}
//	ident[T, U]                        -> NamedType with TypeArgs
//	import/path.ident[T, U]            -> ditto
func (p *typeParser) parseNamed() (GoType, error) {
	prefix, err := p.parseIdentPath()
	if err != nil {
		return nil, err
	}
	// Split at last "."; everything before is package path.
	pkg := ""
	name := prefix
	if dot := strings.LastIndex(prefix, "."); dot >= 0 {
		pkg = prefix[:dot]
		name = prefix[dot+1:]
	}
	// Generic instantiation.
	var typeArgs []GoType
	if p.consume("[") {
		if !p.consume("]") {
			for {
				t, err := p.parseType()
				if err != nil {
					return nil, err
				}
				typeArgs = append(typeArgs, t)
				if !p.consume(",") {
					break
				}
			}
			if err := p.expect("]"); err != nil {
				return nil, err
			}
		}
	}
	if pkg == "" && len(typeArgs) == 0 && basicSet[name] {
		return BasicType{Name: name}, nil
	}
	return NamedType{PackagePath: pkg, Name: name, TypeArgs: typeArgs}, nil
}

// parseIdentPath parses an identifier optionally followed by
// "/path/to/pkg.Name" tail; the result is the entire path-with-dot
// string. Splits at the final dot.
//
// Examples:
//   "io.Reader"               -> "io.Reader"
//   "github.com/foo/bar.Baz"  -> "github.com/foo/bar.Baz"
//   "Foo"                     -> "Foo"
//   "X86.Pause"               -> "X86.Pause"  (only one dot; pkg="X86")
func (p *typeParser) parseIdentPath() (string, error) {
	p.skipSpaces()
	start := p.pos
	if p.pos >= len(p.src) {
		return "", p.errf("expected identifier")
	}
	r, sz := utf8.DecodeRuneInString(p.src[p.pos:])
	if !isIdentStart(r) {
		return "", p.errf("expected identifier, got %q", string(r))
	}
	p.pos += sz
	for p.pos < len(p.src) {
		r, sz := utf8.DecodeRuneInString(p.src[p.pos:])
		if isIdentCont(r) || r == '.' || r == '/' || r == '-' {
			p.pos += sz
			continue
		}
		break
	}
	return p.src[start:p.pos], nil
}

// tryKeyword consumes the literal kw only when it appears as a whole
// identifier (not followed by an identifier continuation char). For
// example, "func(" matches "func" but "function(" does not.
func (p *typeParser) tryKeyword(kw string) (string, bool) {
	p.skipSpaces()
	if !strings.HasPrefix(p.src[p.pos:], kw) {
		return "", false
	}
	after := p.pos + len(kw)
	if after < len(p.src) {
		r, _ := utf8.DecodeRuneInString(p.src[after:])
		if isIdentCont(r) {
			return "", false
		}
	}
	p.pos = after
	return kw, true
}

func isIdentStart(r rune) bool {
	return r == '_' || unicode.IsLetter(r)
}

func isIdentCont(r rune) bool {
	return r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r)
}
