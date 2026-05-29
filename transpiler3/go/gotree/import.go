package gotree

import (
	"sort"
	"strconv"
	"strings"
)

// ImportSpec is one entry in an import group. Alias is empty
// for a default import, "_" for a blank import, "." for a dot
// import, or a non-empty identifier for an aliased import.
type ImportSpec struct {
	Alias string
	Path  string
}

// sortImports orders specs by path, putting the stdlib group
// first and the module-internal/third-party group second, with
// a blank line between groups. gofmt would do the grouping
// itself, but emitting it pre-sorted keeps Render's output
// stable when format.Source rewrites whitespace.
func sortImports(specs []ImportSpec) ([]ImportSpec, []ImportSpec) {
	var stdlib, other []ImportSpec
	for _, s := range specs {
		if isStdlibPath(s.Path) {
			stdlib = append(stdlib, s)
		} else {
			other = append(other, s)
		}
	}
	sort.Slice(stdlib, func(i, j int) bool { return stdlib[i].Path < stdlib[j].Path })
	sort.Slice(other, func(i, j int) bool { return other[i].Path < other[j].Path })
	return stdlib, other
}

// isStdlibPath returns true if path has no dot before the
// first slash, which is the same heuristic goimports uses to
// classify stdlib vs module imports.
func isStdlibPath(path string) bool {
	first, _, _ := strings.Cut(path, "/")
	return !strings.Contains(first, ".")
}

func writeImports(w *Writer, specs []ImportSpec) {
	if len(specs) == 0 {
		return
	}
	stdlib, other := sortImports(specs)
	w.Line("import (")
	w.Indent()
	for _, s := range stdlib {
		w.Line(formatImport(s))
	}
	if len(stdlib) > 0 && len(other) > 0 {
		w.Line("")
	}
	for _, s := range other {
		w.Line(formatImport(s))
	}
	w.Dedent()
	w.Line(")")
}

func formatImport(s ImportSpec) string {
	q := strconv.Quote(s.Path)
	if s.Alias == "" {
		return q
	}
	return s.Alias + " " + q
}
