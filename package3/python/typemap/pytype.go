package typemap

import (
	"fmt"
	"strings"
	"unicode"
)

// PyKind classifies a node in the parsed Python type expression AST.
type PyKind int

const (
	// PyName is a bare identifier (`int`, `T`, `List`).
	PyName PyKind = iota
	// PyAttr is an attribute access (`typing.List`, `collections.abc.Iterable`).
	PyAttr
	// PySubscript is `Base[Arg, Arg, ...]`.
	PySubscript
	// PyTuple is the bracketed sequence used inside Callable's first argument:
	// `Callable[[A, B], R]` -> the [A, B] is a PyTuple.
	PyTuple
	// PyUnion is `X | Y | Z` (PEP 604).
	PyUnion
	// PyLiteral is a literal value: `"abc"`, `1`, `True`, `None`. Also used for
	// PEP 484 forward references which are written as string literals.
	PyLiteral
	// PyEllipsis is `...`.
	PyEllipsis
)

// PyType is a node in the Python type expression AST. Only the fields
// relevant to the Kind are populated.
type PyType struct {
	Kind PyKind
	// Name is the identifier for PyName, the attribute name for PyAttr.
	Name string
	// Base is the receiver for PyAttr / PySubscript.
	Base *PyType
	// Args is the bracketed argument list for PySubscript / PyTuple, or the
	// branches for PyUnion.
	Args []PyType
	// Literal is the raw literal token for PyLiteral (including surrounding
	// quotes for string literals).
	Literal string
}

// String renders the PyType back to its source form. Mostly used in tests and
// error messages.
func (p PyType) String() string {
	switch p.Kind {
	case PyName:
		return p.Name
	case PyAttr:
		return p.Base.String() + "." + p.Name
	case PySubscript:
		var args []string
		for _, a := range p.Args {
			args = append(args, a.String())
		}
		return p.Base.String() + "[" + strings.Join(args, ", ") + "]"
	case PyTuple:
		var args []string
		for _, a := range p.Args {
			args = append(args, a.String())
		}
		return "[" + strings.Join(args, ", ") + "]"
	case PyUnion:
		var branches []string
		for _, a := range p.Args {
			branches = append(branches, a.String())
		}
		return strings.Join(branches, " | ")
	case PyLiteral:
		return p.Literal
	case PyEllipsis:
		return "..."
	}
	return "<bad>"
}

// QualifiedName returns the dotted identifier path for PyName / PyAttr, e.g.
// "typing.List". Returns the empty string for non-identifier kinds.
func (p PyType) QualifiedName() string {
	switch p.Kind {
	case PyName:
		return p.Name
	case PyAttr:
		base := p.Base.QualifiedName()
		if base == "" {
			return ""
		}
		return base + "." + p.Name
	}
	return ""
}

// ParsePyType parses a Python type expression string into a PyType tree.
//
// Supported grammar (intentionally narrow):
//
//	type        := union
//	union       := primary ( '|' primary )*
//	primary     := atom ( '[' arglist ']' | '.' NAME )*
//	atom        := NAME | NUMBER | STRING | 'None' | 'True' | 'False' | '...' | '(' type ')'
//	arglist     := arg ( ',' arg )*
//	arg         := type | '[' arglist ']'   // the bracketed list form is for Callable
//
// String literals are kept as the raw token (including quotes) so callers can
// distinguish forward references from a bare identifier.
func ParsePyType(src string) (PyType, error) {
	p := &pyParser{src: strings.TrimSpace(src)}
	t, err := p.parseUnion()
	if err != nil {
		return PyType{}, err
	}
	p.skipWS()
	if p.pos < len(p.src) {
		return PyType{}, fmt.Errorf("pytype: trailing input at offset %d: %q", p.pos, p.src[p.pos:])
	}
	return t, nil
}

type pyParser struct {
	src string
	pos int
}

func (p *pyParser) skipWS() {
	for p.pos < len(p.src) && (p.src[p.pos] == ' ' || p.src[p.pos] == '\t') {
		p.pos++
	}
}

func (p *pyParser) parseUnion() (PyType, error) {
	first, err := p.parsePrimary()
	if err != nil {
		return PyType{}, err
	}
	var branches []PyType
	for {
		p.skipWS()
		if p.pos >= len(p.src) || p.src[p.pos] != '|' {
			break
		}
		p.pos++
		next, err := p.parsePrimary()
		if err != nil {
			return PyType{}, err
		}
		if len(branches) == 0 {
			branches = append(branches, first)
		}
		branches = append(branches, next)
	}
	if len(branches) > 0 {
		return PyType{Kind: PyUnion, Args: branches}, nil
	}
	return first, nil
}

func (p *pyParser) parsePrimary() (PyType, error) {
	atom, err := p.parseAtom()
	if err != nil {
		return PyType{}, err
	}
	for {
		p.skipWS()
		if p.pos >= len(p.src) {
			return atom, nil
		}
		switch p.src[p.pos] {
		case '[':
			p.pos++
			args, err := p.parseArglist()
			if err != nil {
				return PyType{}, err
			}
			p.skipWS()
			if p.pos >= len(p.src) || p.src[p.pos] != ']' {
				return PyType{}, fmt.Errorf("pytype: missing ']' at offset %d", p.pos)
			}
			p.pos++
			base := atom
			atom = PyType{Kind: PySubscript, Base: &base, Args: args}
		case '.':
			p.pos++
			name, err := p.parseIdent()
			if err != nil {
				return PyType{}, err
			}
			base := atom
			atom = PyType{Kind: PyAttr, Base: &base, Name: name}
		default:
			return atom, nil
		}
	}
}

func (p *pyParser) parseArglist() ([]PyType, error) {
	var args []PyType
	for {
		p.skipWS()
		if p.pos >= len(p.src) {
			return nil, fmt.Errorf("pytype: unterminated argument list")
		}
		if p.src[p.pos] == ']' {
			return args, nil
		}
		if p.src[p.pos] == '[' {
			// Callable[[A, B], R] form: the first arg is itself a bracketed list.
			p.pos++
			inner, err := p.parseArglist()
			if err != nil {
				return nil, err
			}
			p.skipWS()
			if p.pos >= len(p.src) || p.src[p.pos] != ']' {
				return nil, fmt.Errorf("pytype: missing ']' inside arglist at offset %d", p.pos)
			}
			p.pos++
			args = append(args, PyType{Kind: PyTuple, Args: inner})
		} else {
			t, err := p.parseUnion()
			if err != nil {
				return nil, err
			}
			args = append(args, t)
		}
		p.skipWS()
		if p.pos < len(p.src) && p.src[p.pos] == ',' {
			p.pos++
			continue
		}
		return args, nil
	}
}

func (p *pyParser) parseAtom() (PyType, error) {
	p.skipWS()
	if p.pos >= len(p.src) {
		return PyType{}, fmt.Errorf("pytype: unexpected end of input")
	}
	c := p.src[p.pos]
	switch {
	case c == '(':
		p.pos++
		inner, err := p.parseUnion()
		if err != nil {
			return PyType{}, err
		}
		p.skipWS()
		if p.pos >= len(p.src) || p.src[p.pos] != ')' {
			return PyType{}, fmt.Errorf("pytype: missing ')' at offset %d", p.pos)
		}
		p.pos++
		return inner, nil
	case c == '.':
		// `...`
		if p.pos+2 < len(p.src) && p.src[p.pos+1] == '.' && p.src[p.pos+2] == '.' {
			p.pos += 3
			return PyType{Kind: PyEllipsis}, nil
		}
		return PyType{}, fmt.Errorf("pytype: unexpected '.' at offset %d", p.pos)
	case c == '"' || c == '\'':
		return p.parseStringLiteral()
	case c >= '0' && c <= '9':
		return p.parseNumberLiteral()
	case c == '-':
		return p.parseNumberLiteral()
	case isIdentStart(rune(c)):
		name, err := p.parseIdent()
		if err != nil {
			return PyType{}, err
		}
		switch name {
		case "True", "False", "None":
			return PyType{Kind: PyLiteral, Literal: name}, nil
		}
		return PyType{Kind: PyName, Name: name}, nil
	}
	return PyType{}, fmt.Errorf("pytype: unexpected character %q at offset %d", c, p.pos)
}

func (p *pyParser) parseIdent() (string, error) {
	p.skipWS()
	start := p.pos
	if p.pos >= len(p.src) || !isIdentStart(rune(p.src[p.pos])) {
		return "", fmt.Errorf("pytype: expected identifier at offset %d", p.pos)
	}
	p.pos++
	for p.pos < len(p.src) && isIdentCont(rune(p.src[p.pos])) {
		p.pos++
	}
	return p.src[start:p.pos], nil
}

func (p *pyParser) parseStringLiteral() (PyType, error) {
	q := p.src[p.pos]
	start := p.pos
	p.pos++
	for p.pos < len(p.src) {
		if p.src[p.pos] == '\\' && p.pos+1 < len(p.src) {
			p.pos += 2
			continue
		}
		if p.src[p.pos] == q {
			p.pos++
			return PyType{Kind: PyLiteral, Literal: p.src[start:p.pos]}, nil
		}
		p.pos++
	}
	return PyType{}, fmt.Errorf("pytype: unterminated string at offset %d", start)
}

func (p *pyParser) parseNumberLiteral() (PyType, error) {
	start := p.pos
	if p.src[p.pos] == '-' {
		p.pos++
	}
	for p.pos < len(p.src) && (p.src[p.pos] >= '0' && p.src[p.pos] <= '9' || p.src[p.pos] == '.') {
		p.pos++
	}
	if start == p.pos {
		return PyType{}, fmt.Errorf("pytype: invalid number at offset %d", start)
	}
	return PyType{Kind: PyLiteral, Literal: p.src[start:p.pos]}, nil
}

func isIdentStart(r rune) bool {
	return r == '_' || unicode.IsLetter(r)
}

func isIdentCont(r rune) bool {
	return r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r)
}
