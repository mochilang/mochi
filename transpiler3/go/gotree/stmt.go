package gotree

// Stmt is any Go statement node.
type Stmt interface {
	stmtNode()
	write(w *Writer)
}

// BlockStmt is { Stmts... }. The writer adds the surrounding
// braces and a newline after each statement.
type BlockStmt struct {
	List []Stmt
}

func (*BlockStmt) stmtNode() {}
func (b *BlockStmt) write(w *Writer) {
	w.Line("{")
	w.Indent()
	for _, s := range b.List {
		s.write(w)
	}
	w.Dedent()
	w.Line("}")
}

// writeInline emits the block without a leading indent so it
// can hug the closing paren of a function signature.
func (b *BlockStmt) writeInline(w *Writer) {
	w.Raw("{\n")
	w.Indent()
	for _, s := range b.List {
		s.write(w)
	}
	w.Dedent()
	w.Line("}")
}

// ExprStmt is a bare expression statement, e.g. `f()`.
type ExprStmt struct{ X Expr }

func (*ExprStmt) stmtNode() {}
func (e *ExprStmt) write(w *Writer) {
	for range w.depth {
		w.Raw("\t")
	}
	e.X.write(w)
	w.Newline()
}

// AssignStmt is Lhs Op Rhs, e.g. `x := 1`, `a, b = b, a`,
// `x += 1`. Tok must be one of `=`, `:=`, `+=`, `-=`, etc.
type AssignStmt struct {
	Lhs []Expr
	Tok string
	Rhs []Expr
}

func (*AssignStmt) stmtNode() {}
func (a *AssignStmt) write(w *Writer) {
	for range w.depth {
		w.Raw("\t")
	}
	for i, l := range a.Lhs {
		if i > 0 {
			w.Raw(", ")
		}
		l.write(w)
	}
	w.Raw(" ")
	w.Raw(a.Tok)
	w.Raw(" ")
	for i, r := range a.Rhs {
		if i > 0 {
			w.Raw(", ")
		}
		r.write(w)
	}
	w.Newline()
}

// DeclStmt is a declaration used as a statement (a local
// `var`, `const`, or `type` inside a block).
type DeclStmt struct{ Decl Decl }

func (*DeclStmt) stmtNode()       {}
func (d *DeclStmt) write(w *Writer) { d.Decl.write(w) }

// ReturnStmt is `return Results...`.
type ReturnStmt struct {
	Results []Expr
}

func (*ReturnStmt) stmtNode() {}
func (r *ReturnStmt) write(w *Writer) {
	for range w.depth {
		w.Raw("\t")
	}
	w.Raw("return")
	for i, x := range r.Results {
		if i == 0 {
			w.Raw(" ")
		} else {
			w.Raw(", ")
		}
		x.write(w)
	}
	w.Newline()
}

// IfStmt is `if Init; Cond Body else Else`. Init and Else may
// be nil. Else must be either a *BlockStmt or an *IfStmt.
type IfStmt struct {
	Init Stmt
	Cond Expr
	Body *BlockStmt
	Else Stmt
}

func (*IfStmt) stmtNode() {}
func (i *IfStmt) write(w *Writer) {
	for range w.depth {
		w.Raw("\t")
	}
	w.Raw("if ")
	if i.Init != nil {
		writeInlineStmt(w, i.Init)
		w.Raw("; ")
	}
	i.Cond.write(w)
	w.Raw(" ")
	i.Body.writeInline(w)
	if i.Else != nil {
		for range w.depth {
			w.Raw("\t")
		}
		w.Raw(" else ")
		i.Else.write(w)
	}
}

// ForStmt is `for Init; Cond; Post Body`. Any header part may
// be nil; a fully nil header renders as `for { ... }`.
type ForStmt struct {
	Init Stmt
	Cond Expr
	Post Stmt
	Body *BlockStmt
}

func (*ForStmt) stmtNode() {}
func (f *ForStmt) write(w *Writer) {
	for range w.depth {
		w.Raw("\t")
	}
	w.Raw("for")
	if f.Init == nil && f.Post == nil && f.Cond != nil {
		w.Raw(" ")
		f.Cond.write(w)
	} else if f.Init != nil || f.Post != nil {
		w.Raw(" ")
		if f.Init != nil {
			writeInlineStmt(w, f.Init)
		}
		w.Raw("; ")
		if f.Cond != nil {
			f.Cond.write(w)
		}
		w.Raw("; ")
		if f.Post != nil {
			writeInlineStmt(w, f.Post)
		}
	}
	w.Raw(" ")
	f.Body.writeInline(w)
}

// RangeStmt is `for Key, Value := range X Body`. Tok must be
// `=` or `:=`. Key and Value may be nil.
type RangeStmt struct {
	Key   Expr
	Value Expr
	Tok   string
	X     Expr
	Body  *BlockStmt
}

func (*RangeStmt) stmtNode() {}
func (r *RangeStmt) write(w *Writer) {
	for range w.depth {
		w.Raw("\t")
	}
	w.Raw("for ")
	switch {
	case r.Key != nil && r.Value != nil:
		r.Key.write(w)
		w.Raw(", ")
		r.Value.write(w)
		w.Raw(" " + r.Tok + " ")
	case r.Key != nil:
		r.Key.write(w)
		w.Raw(" " + r.Tok + " ")
	}
	w.Raw("range ")
	r.X.write(w)
	w.Raw(" ")
	r.Body.writeInline(w)
}

// SwitchStmt is `switch Init; Tag { Cases... }`. Tag may be
// nil; either Init or Tag may be nil.
type SwitchStmt struct {
	Init  Stmt
	Tag   Expr
	Cases []*CaseClause
}

func (*SwitchStmt) stmtNode() {}
func (s *SwitchStmt) write(w *Writer) {
	for range w.depth {
		w.Raw("\t")
	}
	w.Raw("switch")
	if s.Init != nil {
		w.Raw(" ")
		writeInlineStmt(w, s.Init)
		w.Raw(";")
	}
	if s.Tag != nil {
		w.Raw(" ")
		s.Tag.write(w)
	}
	w.Line(" {")
	for _, c := range s.Cases {
		c.write(w)
	}
	w.Line("}")
}

// CaseClause is one case body inside a SwitchStmt. List nil
// means `default:`.
type CaseClause struct {
	List []Expr
	Body []Stmt
}

func (*CaseClause) stmtNode() {}
func (c *CaseClause) write(w *Writer) {
	for range w.depth {
		w.Raw("\t")
	}
	if len(c.List) == 0 {
		w.Raw("default:")
	} else {
		w.Raw("case ")
		for i, e := range c.List {
			if i > 0 {
				w.Raw(", ")
			}
			e.write(w)
		}
		w.Raw(":")
	}
	w.Newline()
	w.Indent()
	for _, s := range c.Body {
		s.write(w)
	}
	w.Dedent()
}

// TypeSwitchStmt is `switch X := Y.(type) { ... }`.
type TypeSwitchStmt struct {
	Assign Stmt // `X := Y.(type)` or just `Y.(type)`
	Cases  []*CaseClause
}

func (*TypeSwitchStmt) stmtNode() {}
func (t *TypeSwitchStmt) write(w *Writer) {
	for range w.depth {
		w.Raw("\t")
	}
	w.Raw("switch ")
	writeInlineStmt(w, t.Assign)
	w.Line(" {")
	for _, c := range t.Cases {
		c.write(w)
	}
	w.Line("}")
}

// SelectStmt is `select { CommClauses... }`.
type SelectStmt struct {
	Cases []*CommClause
}

func (*SelectStmt) stmtNode() {}
func (s *SelectStmt) write(w *Writer) {
	for range w.depth {
		w.Raw("\t")
	}
	w.Line("select {")
	for _, c := range s.Cases {
		c.write(w)
	}
	w.Line("}")
}

// CommClause is one case body of a SelectStmt. Comm nil means
// `default:`. Comm must be either an *AssignStmt (recv) or an
// *ExprStmt wrapping a send (chan <- value).
type CommClause struct {
	Comm Stmt
	Body []Stmt
}

func (*CommClause) stmtNode() {}
func (c *CommClause) write(w *Writer) {
	for range w.depth {
		w.Raw("\t")
	}
	if c.Comm == nil {
		w.Raw("default:")
	} else {
		w.Raw("case ")
		writeInlineStmt(w, c.Comm)
		w.Raw(":")
	}
	w.Newline()
	w.Indent()
	for _, s := range c.Body {
		s.write(w)
	}
	w.Dedent()
}

// GoStmt is `go Call`.
type GoStmt struct{ Call *CallExpr }

func (*GoStmt) stmtNode() {}
func (g *GoStmt) write(w *Writer) {
	for range w.depth {
		w.Raw("\t")
	}
	w.Raw("go ")
	g.Call.write(w)
	w.Newline()
}

// DeferStmt is `defer Call`.
type DeferStmt struct{ Call *CallExpr }

func (*DeferStmt) stmtNode() {}
func (d *DeferStmt) write(w *Writer) {
	for range w.depth {
		w.Raw("\t")
	}
	w.Raw("defer ")
	d.Call.write(w)
	w.Newline()
}

// BranchStmt is `break`, `continue`, `goto`, or `fallthrough`,
// optionally with a label.
type BranchStmt struct {
	Tok   string
	Label string
}

func (*BranchStmt) stmtNode() {}
func (b *BranchStmt) write(w *Writer) {
	for range w.depth {
		w.Raw("\t")
	}
	w.Raw(b.Tok)
	if b.Label != "" {
		w.Raw(" ")
		w.Raw(b.Label)
	}
	w.Newline()
}

// IncDecStmt is `X++` or `X--`.
type IncDecStmt struct {
	X   Expr
	Tok string
}

func (*IncDecStmt) stmtNode() {}
func (i *IncDecStmt) write(w *Writer) {
	for range w.depth {
		w.Raw("\t")
	}
	i.X.write(w)
	w.Raw(i.Tok)
	w.Newline()
}

// SendStmt is `Chan <- Value`.
type SendStmt struct {
	Chan  Expr
	Value Expr
}

func (*SendStmt) stmtNode() {}
func (s *SendStmt) write(w *Writer) {
	for range w.depth {
		w.Raw("\t")
	}
	s.Chan.write(w)
	w.Raw(" <- ")
	s.Value.write(w)
	w.Newline()
}

// LabeledStmt is `Label: Stmt`.
type LabeledStmt struct {
	Label string
	Stmt  Stmt
}

func (*LabeledStmt) stmtNode() {}
func (l *LabeledStmt) write(w *Writer) {
	for range w.depth {
		w.Raw("\t")
	}
	w.Raw(l.Label)
	w.Raw(":")
	w.Newline()
	l.Stmt.write(w)
}

// EmptyStmt renders nothing; useful as a placeholder.
type EmptyStmt struct{}

func (*EmptyStmt) stmtNode()       {}
func (*EmptyStmt) write(w *Writer) {}

// writeInlineStmt writes s without leading indent or trailing
// newline, for use inside compound statement headers like
// IfStmt.Init and ForStmt.Init. It cooperates with the per-stmt
// write methods by temporarily zeroing w.depth and stripping
// the trailing newline.
func writeInlineStmt(w *Writer, s Stmt) {
	if s == nil {
		return
	}
	depth := w.depth
	w.depth = 0
	start := w.buf.Len()
	s.write(w)
	w.depth = depth
	if w.buf.Len() > start {
		b := w.buf.Bytes()
		for w.buf.Len() > start && b[w.buf.Len()-1] == '\n' {
			w.buf.Truncate(w.buf.Len() - 1)
		}
	}
}
