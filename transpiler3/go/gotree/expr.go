package gotree

import (
	"strconv"
	"strings"
)

// Expr is any value-producing Go AST node.
type Expr interface {
	exprNode()
	write(w *Writer)
}

// Ident is a bare identifier reference. The lowerer is
// responsible for picking a name that is already safe under
// Go's reserved word and shadowing rules; gotree does not
// re-mangle.
type Ident struct {
	Name string
}

func (*Ident) exprNode()        {}
func (i *Ident) write(w *Writer) { w.Raw(i.Name) }

// BasicLit is a Go basic literal (string, int, float, char,
// imaginary). Kind names mirror go/token's BasicLit kinds; the
// lowerer is responsible for Go-escape-encoding strings before
// constructing the node.
type BasicLit struct {
	Kind  BasicLitKind
	Value string
}

// BasicLitKind enumerates the basic literal kinds Phase 0
// represents. Phase 2 extends this with IMAG once Mochi grows
// complex literals (no current plan).
type BasicLitKind int

const (
	StringLit BasicLitKind = iota
	IntLit
	FloatLit
	CharLit
)

func (*BasicLit) exprNode() {}
func (b *BasicLit) write(w *Writer) {
	switch b.Kind {
	case StringLit:
		w.Raw(strconv.Quote(b.Value))
	default:
		w.Raw(b.Value)
	}
}

// CallExpr is a function-call expression: Fun(Args...).
type CallExpr struct {
	Fun  Expr
	Args []Expr
}

func (*CallExpr) exprNode() {}
func (c *CallExpr) write(w *Writer) {
	c.Fun.write(w)
	w.Raw("(")
	for i, a := range c.Args {
		if i > 0 {
			w.Raw(", ")
		}
		a.write(w)
	}
	w.Raw(")")
}

// SelectorExpr is X.Sel.
type SelectorExpr struct {
	X   Expr
	Sel string
}

func (*SelectorExpr) exprNode() {}
func (s *SelectorExpr) write(w *Writer) {
	s.X.write(w)
	w.Raw(".")
	w.Raw(s.Sel)
}

// IndexExpr is X[Index]. Phase 0 covers the unary form; the
// generic call form X[T] uses IndexListExpr.
type IndexExpr struct {
	X     Expr
	Index Expr
}

func (*IndexExpr) exprNode() {}
func (i *IndexExpr) write(w *Writer) {
	i.X.write(w)
	w.Raw("[")
	i.Index.write(w)
	w.Raw("]")
}

// IndexListExpr is X[A, B, ...] for generic type arguments.
type IndexListExpr struct {
	X       Expr
	Indices []Expr
}

func (*IndexListExpr) exprNode() {}
func (i *IndexListExpr) write(w *Writer) {
	i.X.write(w)
	w.Raw("[")
	for k, idx := range i.Indices {
		if k > 0 {
			w.Raw(", ")
		}
		idx.write(w)
	}
	w.Raw("]")
}

// SliceExpr is X[Low:High:Max]. Any of the three bounds may be
// nil; the writer skips nil bounds while preserving the colons.
type SliceExpr struct {
	X    Expr
	Low  Expr
	High Expr
	Max  Expr // optional 3-arg slice cap
}

func (*SliceExpr) exprNode() {}
func (s *SliceExpr) write(w *Writer) {
	s.X.write(w)
	w.Raw("[")
	if s.Low != nil {
		s.Low.write(w)
	}
	w.Raw(":")
	if s.High != nil {
		s.High.write(w)
	}
	if s.Max != nil {
		w.Raw(":")
		s.Max.write(w)
	}
	w.Raw("]")
}

// BinaryExpr is X Op Y.
type BinaryExpr struct {
	X  Expr
	Op string
	Y  Expr
}

func (*BinaryExpr) exprNode() {}
func (b *BinaryExpr) write(w *Writer) {
	b.X.write(w)
	w.Raw(" ")
	w.Raw(b.Op)
	w.Raw(" ")
	b.Y.write(w)
}

// UnaryExpr is Op X (prefix), e.g. -x, !x, &x, *x, <-x.
type UnaryExpr struct {
	Op string
	X  Expr
}

func (*UnaryExpr) exprNode() {}
func (u *UnaryExpr) write(w *Writer) {
	w.Raw(u.Op)
	u.X.write(w)
}

// ParenExpr is (X). Use sparingly; gofmt removes redundant
// parens, but they are sometimes required for precedence.
type ParenExpr struct{ X Expr }

func (*ParenExpr) exprNode() {}
func (p *ParenExpr) write(w *Writer) {
	w.Raw("(")
	p.X.write(w)
	w.Raw(")")
}

// StarExpr is *X used either as a pointer-type expression or
// as the pointer-dereference operator. The writer cannot
// distinguish; the lowerer must use it consistently.
type StarExpr struct{ X Expr }

func (*StarExpr) exprNode() {}
func (s *StarExpr) write(w *Writer) {
	w.Raw("*")
	s.X.write(w)
}

// CompositeLit is Type{Elts...}, used for struct, slice, array,
// and map literals. Type may be nil for inferred literals (e.g.
// []int{...}{1,2,3} inside another composite).
type CompositeLit struct {
	Type Expr
	Elts []Expr
}

func (*CompositeLit) exprNode() {}
func (c *CompositeLit) write(w *Writer) {
	if c.Type != nil {
		c.Type.write(w)
	}
	w.Raw("{")
	for i, e := range c.Elts {
		if i > 0 {
			w.Raw(", ")
		}
		e.write(w)
	}
	w.Raw("}")
}

// KeyValueExpr is Key: Value, used inside CompositeLit for
// struct field initialisers and map literals.
type KeyValueExpr struct {
	Key   Expr
	Value Expr
}

func (*KeyValueExpr) exprNode() {}
func (k *KeyValueExpr) write(w *Writer) {
	k.Key.write(w)
	w.Raw(": ")
	k.Value.write(w)
}

// FuncLit is func(Params) Results { Body }.
type FuncLit struct {
	Type *FuncType
	Body *BlockStmt
}

func (*FuncLit) exprNode() {}
func (f *FuncLit) write(w *Writer) {
	w.Raw("func")
	f.Type.write(w)
	w.Raw(" ")
	f.Body.writeInline(w)
}

// TypeAssertExpr is X.(T).
type TypeAssertExpr struct {
	X    Expr
	Type Expr
}

func (*TypeAssertExpr) exprNode() {}
func (t *TypeAssertExpr) write(w *Writer) {
	t.X.write(w)
	w.Raw(".(")
	t.Type.write(w)
	w.Raw(")")
}

// FuncType is a function signature node, used both as a type
// expression and as the type half of a FuncLit / FuncDecl.
type FuncType struct {
	TypeParams []TypeParam
	Params     []Field
	Results    []Field
}

func (*FuncType) exprNode() {}
func (f *FuncType) write(w *Writer) {
	if len(f.TypeParams) > 0 {
		w.Raw("[")
		for i, p := range f.TypeParams {
			if i > 0 {
				w.Raw(", ")
			}
			w.Raw(p.Name)
			w.Raw(" ")
			p.Constraint.write(w)
		}
		w.Raw("]")
	}
	w.Raw("(")
	for i, p := range f.Params {
		if i > 0 {
			w.Raw(", ")
		}
		writeField(w, p)
	}
	w.Raw(")")
	switch len(f.Results) {
	case 0:
	case 1:
		r := f.Results[0]
		if len(r.Names) == 0 {
			w.Raw(" ")
			r.Type.write(w)
		} else {
			w.Raw(" (")
			writeField(w, r)
			w.Raw(")")
		}
	default:
		w.Raw(" (")
		for i, r := range f.Results {
			if i > 0 {
				w.Raw(", ")
			}
			writeField(w, r)
		}
		w.Raw(")")
	}
}

// ArrayType is [Len]Elt. Len nil means a slice ([]Elt).
type ArrayType struct {
	Len Expr
	Elt Expr
}

func (*ArrayType) exprNode() {}
func (a *ArrayType) write(w *Writer) {
	w.Raw("[")
	if a.Len != nil {
		a.Len.write(w)
	}
	w.Raw("]")
	a.Elt.write(w)
}

// MapType is map[Key]Value.
type MapType struct {
	Key   Expr
	Value Expr
}

func (*MapType) exprNode() {}
func (m *MapType) write(w *Writer) {
	w.Raw("map[")
	m.Key.write(w)
	w.Raw("]")
	m.Value.write(w)
}

// ChanDir enumerates Go channel direction qualifiers.
type ChanDir int

const (
	// SendRecv is `chan T`, the default bidirectional channel.
	SendRecv ChanDir = iota
	// SendOnly is `chan<- T`.
	SendOnly
	// RecvOnly is `<-chan T`.
	RecvOnly
)

// ChanType is chan T / chan<- T / <-chan T.
type ChanType struct {
	Dir   ChanDir
	Value Expr
}

func (*ChanType) exprNode() {}
func (c *ChanType) write(w *Writer) {
	switch c.Dir {
	case SendOnly:
		w.Raw("chan<- ")
	case RecvOnly:
		w.Raw("<-chan ")
	default:
		w.Raw("chan ")
	}
	c.Value.write(w)
}

// InterfaceType is interface{ Methods... }.
type InterfaceType struct {
	Methods []Field
}

func (*InterfaceType) exprNode() {}
func (i *InterfaceType) write(w *Writer) {
	w.Raw("interface{")
	for k, m := range i.Methods {
		if k > 0 {
			w.Raw("; ")
		}
		writeField(w, m)
	}
	w.Raw("}")
}

// StructType is struct{ Fields... }. The writer emits a single
// line; gofmt re-flows multi-field structs onto separate lines.
type StructType struct {
	Fields []Field
}

func (*StructType) exprNode() {}
func (s *StructType) write(w *Writer) {
	w.Raw("struct{")
	for k, f := range s.Fields {
		if k > 0 {
			w.Raw("; ")
		}
		writeField(w, f)
		if f.Tag != "" {
			w.Raw(" `")
			w.Raw(f.Tag)
			w.Raw("`")
		}
	}
	w.Raw("}")
}

// RawExpr is an escape hatch for hand-written Go fragments
// when a lowering pass needs syntax not yet modelled by gotree.
// Use sparingly; prefer modelling the syntax as a structured
// node so format.Source can reflow it.
type RawExpr struct{ Src string }

func (*RawExpr) exprNode() {}
func (r *RawExpr) write(w *Writer) {
	w.Raw(strings.TrimSpace(r.Src))
}
