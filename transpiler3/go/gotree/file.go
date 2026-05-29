package gotree

// File is the top-level shadow AST node: one .go source file.
// PackageName is the identifier in the `package` clause; the
// import group and decl list follow in source order.
type File struct {
	Doc         *CommentGroup
	PackageName string
	Imports     []ImportSpec
	Decls       []Decl
}

// Render walks the file into a bytes.Buffer of raw Go source
// then runs the buffer through go/format.Source. The output is
// byte-identical to what `gofmt` would produce on the same
// program.
func (f *File) Render() ([]byte, error) {
	w := NewWriter()
	f.write(w)
	return Format(w.Bytes())
}

func (f *File) write(w *Writer) {
	f.Doc.write(w)
	w.Line("package " + f.PackageName)
	w.Newline()
	if len(f.Imports) > 0 {
		writeImports(w, f.Imports)
		w.Newline()
	}
	for i, d := range f.Decls {
		if i > 0 {
			w.Newline()
		}
		d.write(w)
	}
}
