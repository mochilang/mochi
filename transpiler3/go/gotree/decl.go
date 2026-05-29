package gotree

// Decl is any top-level Go declaration: an import group, a
// var/const/type group, or a function declaration.
type Decl interface {
	declNode()
	write(w *Writer)
}

// GenDecl is a `var (...)`, `const (...)`, or `type (...)`
// group declaration. For a single-spec group, the writer emits
// the un-grouped form (`var x = 1`); for multi-spec it emits
// the grouped form so gofmt can align the columns.
type GenDecl struct {
	Doc   *CommentGroup
	Tok   string // "var" | "const" | "type"
	Specs []Spec
}

func (*GenDecl) declNode() {}
func (g *GenDecl) write(w *Writer) {
	g.Doc.write(w)
	if len(g.Specs) == 0 {
		return
	}
	if len(g.Specs) == 1 {
		w.Raw(g.Tok)
		w.Raw(" ")
		g.Specs[0].writeSpec(w, g.Tok)
		w.Newline()
		return
	}
	w.Line(g.Tok + " (")
	w.Indent()
	for _, s := range g.Specs {
		for range w.depth {
			w.Raw("\t")
		}
		s.writeSpec(w, g.Tok)
		w.Newline()
	}
	w.Dedent()
	w.Line(")")
}

// Spec is one entry inside a GenDecl. The dispatch on the
// outer Tok lets a single Spec type (e.g. ValueSpec) serve
// both var and const groups.
type Spec interface {
	writeSpec(w *Writer, tok string)
}

// ValueSpec is `Names Type = Values`. Either Type or Values
// may be nil (but not both: `var x` requires a type).
type ValueSpec struct {
	Doc    *CommentGroup
	Names  []string
	Type   Expr
	Values []Expr
}

func (v *ValueSpec) writeSpec(w *Writer, _ string) {
	for i, n := range v.Names {
		if i > 0 {
			w.Raw(", ")
		}
		w.Raw(n)
	}
	if v.Type != nil {
		w.Raw(" ")
		v.Type.write(w)
	}
	if len(v.Values) > 0 {
		w.Raw(" = ")
		for i, val := range v.Values {
			if i > 0 {
				w.Raw(", ")
			}
			val.write(w)
		}
	}
}

// TypeSpec is one `type Name = Type` (alias) or `type Name Type`
// (defined) entry inside a `type (...)` group.
type TypeSpec struct {
	Doc        *CommentGroup
	Name       string
	TypeParams []TypeParam
	Assign     bool // true = `type Name = ...` alias
	Type       Expr
}

func (t *TypeSpec) writeSpec(w *Writer, _ string) {
	w.Raw(t.Name)
	if len(t.TypeParams) > 0 {
		w.Raw("[")
		for i, p := range t.TypeParams {
			if i > 0 {
				w.Raw(", ")
			}
			w.Raw(p.Name)
			w.Raw(" ")
			p.Constraint.write(w)
		}
		w.Raw("]")
	}
	if t.Assign {
		w.Raw(" = ")
	} else {
		w.Raw(" ")
	}
	t.Type.write(w)
}

// FuncDecl is `func (Recv) Name[TypeParams](Params) Results Body`.
type FuncDecl struct {
	Doc  *CommentGroup
	Recv *Field // nil for top-level funcs
	Name string
	Type *FuncType
	Body *BlockStmt // nil = forward decl
}

func (*FuncDecl) declNode() {}
func (f *FuncDecl) write(w *Writer) {
	f.Doc.write(w)
	w.Raw("func ")
	if f.Recv != nil {
		w.Raw("(")
		writeField(w, *f.Recv)
		w.Raw(") ")
	}
	w.Raw(f.Name)
	f.Type.write(w)
	if f.Body == nil {
		w.Newline()
		return
	}
	w.Raw(" ")
	f.Body.writeInline(w)
}

// RawDecl carries a pre-rendered top-level Go declaration. Used by
// the lowerer to inline small generic helpers (e.g. `mochiListSlice`)
// without threading their FuncDecl AST through the gotree layer.
// The writer emits Code verbatim followed by a newline.
type RawDecl struct {
	Doc  *CommentGroup
	Code string
}

func (*RawDecl) declNode() {}
func (d *RawDecl) write(w *Writer) {
	d.Doc.write(w)
	w.Line(d.Code)
}

// TypeParam is one entry in a type parameter list `[T any, U ~int]`.
type TypeParam struct {
	Name       string
	Constraint Expr
}

// Field is a (Names, Type, Tag) triple. Names may be empty for
// anonymous fields and for the result list when results are
// unnamed. Tag is the raw tag content without surrounding
// backticks (the writer adds them).
type Field struct {
	Doc   *CommentGroup
	Names []string
	Type  Expr
	Tag   string
}

func writeField(w *Writer, f Field) {
	for i, n := range f.Names {
		if i > 0 {
			w.Raw(", ")
		}
		w.Raw(n)
	}
	if len(f.Names) > 0 {
		w.Raw(" ")
	}
	f.Type.write(w)
}
