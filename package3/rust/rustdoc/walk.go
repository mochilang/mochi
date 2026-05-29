package rustdoc

import (
	"fmt"
	"sort"

	"mochi/package3/rust/errors"
)

// Walk traverses doc from its Root module, collecting every public
// item reachable via Module.Items into an ApiSurface. Items that are
// not lowerable (lifetime-parameterised, generic without
// monomorphisation, raw-pointer typed, ...) become a SkipReport rather
// than failing the whole walk.
//
// The walker honours the visibility filter: only items whose
// Visibility.IsPublic() is true are emitted. Use-re-exports
// (`pub use foo::Bar`) are followed if their target ID resolves to a
// public item.
//
// The walker is deterministic: items within a module are emitted in
// rustdoc's declared order (Module.Items is already deterministic),
// and SkipReports come out in walk order.
func Walk(doc *Document) (*ApiSurface, error) {
	if doc == nil {
		return nil, fmt.Errorf("rustdoc: walk: nil document")
	}
	root, ok := doc.Index[doc.Root]
	if !ok {
		return nil, fmt.Errorf("rustdoc: walk: root id %q not in index", doc.Root)
	}
	crateName := root.Name
	w := &walker{
		doc:       doc,
		seen:      make(map[string]bool),
		crateName: crateName,
		surface: &ApiSurface{
			CrateName:     crateName,
			CrateVersion:  doc.CrateVersion,
			FormatVersion: doc.FormatVersion,
		},
	}
	w.walkItem(doc.Root, nil)
	return w.surface, nil
}

type walker struct {
	doc       *Document
	seen      map[string]bool
	crateName string
	surface   *ApiSurface
}

func (w *walker) walkItem(id string, path []string) {
	if w.seen[id] {
		return
	}
	w.seen[id] = true
	item, ok := w.doc.Index[id]
	if !ok {
		// External or stripped item; record nothing.
		return
	}
	if !item.Visibility.IsPublic() && item.Inner.Kind() != "module" {
		// Non-public items don't reach the surface.
		return
	}
	itemPath := path
	if item.Name != "" {
		itemPath = append(append([]string(nil), path...), item.Name)
	}
	switch item.Inner.Kind() {
	case "module":
		w.walkModule(id, item, itemPath)
	case "function":
		w.emitFunction(id, item, itemPath)
	case "struct":
		w.emitStruct(id, item, itemPath)
	case "enum":
		w.emitEnum(id, item, itemPath)
	case "type_alias":
		w.emitTypeAlias(id, item, itemPath)
	case "constant":
		w.emitConstant(id, item, itemPath)
	case "trait":
		w.emitTrait(id, item, itemPath)
	case "use":
		w.followUse(item, path)
	case "extern_crate", "primitive", "static", "macro", "proc_macro", "impl", "assoc_const", "assoc_type", "variant", "struct_field":
		// Skipped at this layer; type-mapping (phase 3) handles where useful.
		w.surface.skip(id, itemPath, mapKindToSkipReason(item.Inner.Kind()), item.Inner.Kind())
	default:
		w.surface.skip(id, itemPath, errors.SkipUnknown, item.Inner.Kind())
	}
}

func (w *walker) walkModule(id string, item Item, path []string) {
	mod := item.Inner.Module
	if mod == nil {
		return
	}
	for _, child := range mod.Items {
		w.walkItem(child, path)
	}
}

func (w *walker) followUse(item Item, parentPath []string) {
	u := item.Inner.Use
	if u == nil || u.IsGlob || u.ID == "" {
		return
	}
	// Follow with the *target*'s own path; the use re-export is
	// effectively transparent for the surface.
	w.walkItem(u.ID, parentPath)
}

func (w *walker) emitFunction(id string, item Item, path []string) {
	fn := item.Inner.Function
	if fn == nil {
		return
	}
	if len(fn.Generics.Params) > 0 {
		w.surface.skip(id, path, errors.SkipGeneric, "generic params on function")
		return
	}
	if fn.Header.IsUnsafe {
		w.surface.skip(id, path, errors.SkipExternFnUnsafe, "unsafe fn")
		return
	}
	if fn.Header.ABI.Name != "" && fn.Header.ABI.Name != "Rust" && fn.Header.ABI.Name != "C" {
		w.surface.skip(id, path, errors.SkipCustomAbi, fn.Header.ABI.Name)
		return
	}
	entry := FunctionEntry{
		ID:       id,
		Path:     append([]string(nil), path...),
		Header:   fn.Header,
		Generics: fn.Generics,
	}
	for _, in := range fn.Sig.Inputs {
		entry.Inputs = append(entry.Inputs, ParamEntry{Name: in.Name, Type: in.Type})
	}
	if fn.Sig.Output != nil {
		out := *fn.Sig.Output
		entry.Output = &out
	}
	w.surface.Functions = append(w.surface.Functions, entry)
}

func (w *walker) emitStruct(id string, item Item, path []string) {
	s := item.Inner.Struct
	if s == nil {
		return
	}
	if len(s.Generics.Params) > 0 {
		w.surface.skip(id, path, errors.SkipGeneric, "generic struct")
		return
	}
	entry := StructEntry{ID: id, Path: append([]string(nil), path...), Generics: s.Generics}
	switch {
	case s.Kind.Plain != nil:
		entry.Kind = "plain"
		entry.Fields = w.collectFields(s.Kind.Plain.Fields)
	case s.Kind.Tuple != nil:
		entry.Kind = "tuple"
		entry.Fields = w.collectTupleFields(s.Kind.Tuple)
	case s.Kind.Unit != nil:
		entry.Kind = "unit"
	}
	w.surface.Structs = append(w.surface.Structs, entry)
}

func (w *walker) collectFields(ids []string) []FieldEntry {
	out := make([]FieldEntry, 0, len(ids))
	for _, id := range ids {
		item, ok := w.doc.Index[id]
		if !ok {
			continue
		}
		if item.Inner.StructField == nil {
			continue
		}
		out = append(out, FieldEntry{ID: id, Name: item.Name, Type: *item.Inner.StructField})
	}
	return out
}

func (w *walker) collectTupleFields(ids []*string) []FieldEntry {
	out := make([]FieldEntry, 0, len(ids))
	for i, idPtr := range ids {
		if idPtr == nil {
			out = append(out, FieldEntry{Name: fmt.Sprintf("%d", i)})
			continue
		}
		item, ok := w.doc.Index[*idPtr]
		if !ok {
			continue
		}
		if item.Inner.StructField == nil {
			continue
		}
		out = append(out, FieldEntry{ID: *idPtr, Name: fmt.Sprintf("%d", i), Type: *item.Inner.StructField})
	}
	return out
}

func (w *walker) emitEnum(id string, item Item, path []string) {
	e := item.Inner.Enum
	if e == nil {
		return
	}
	if len(e.Generics.Params) > 0 {
		w.surface.skip(id, path, errors.SkipGeneric, "generic enum")
		return
	}
	entry := EnumEntry{ID: id, Path: append([]string(nil), path...), Generics: e.Generics}
	for _, vid := range e.Variants {
		vItem, ok := w.doc.Index[vid]
		if !ok || vItem.Inner.Variant == nil {
			continue
		}
		v := vItem.Inner.Variant
		ve := VariantEntry{ID: vid, Name: vItem.Name}
		switch {
		case v.Kind.Plain != nil:
			ve.Kind = "plain"
		case v.Kind.Tuple != nil:
			ve.Kind = "tuple"
			for _, idPtr := range v.Kind.Tuple {
				if idPtr == nil {
					continue
				}
				fItem, ok := w.doc.Index[*idPtr]
				if !ok || fItem.Inner.StructField == nil {
					continue
				}
				ve.Tuple = append(ve.Tuple, *fItem.Inner.StructField)
			}
		case v.Kind.Struct != nil:
			ve.Kind = "struct"
			ve.Fields = w.collectFields(v.Kind.Struct.Fields)
		}
		entry.Variants = append(entry.Variants, ve)
	}
	w.surface.Enums = append(w.surface.Enums, entry)
}

func (w *walker) emitTypeAlias(id string, item Item, path []string) {
	a := item.Inner.TypeAlias
	if a == nil {
		return
	}
	if len(a.Generics.Params) > 0 {
		w.surface.skip(id, path, errors.SkipGeneric, "generic type alias")
		return
	}
	w.surface.TypeAliases = append(w.surface.TypeAliases, TypeAliasEntry{
		ID:       id,
		Path:     append([]string(nil), path...),
		Type:     a.Type,
		Generics: a.Generics,
	})
}

func (w *walker) emitConstant(id string, item Item, path []string) {
	c := item.Inner.Constant
	if c == nil {
		return
	}
	w.surface.Constants = append(w.surface.Constants, ConstantEntry{
		ID:   id,
		Path: append([]string(nil), path...),
		Type: c.Type,
	})
}

func (w *walker) emitTrait(id string, item Item, path []string) {
	t := item.Inner.Trait
	if t == nil {
		return
	}
	w.surface.Traits = append(w.surface.Traits, TraitEntry{
		ID:       id,
		Path:     append([]string(nil), path...),
		IsUnsafe: t.IsUnsafe,
		IsAuto:   t.IsAuto,
		Generics: t.Generics,
	})
	// A trait declaration itself does not produce an extern binding;
	// record a skip so the user can see why the binding is absent.
	w.surface.skip(id, path, errors.SkipTrait, "trait declaration")
}

// mapKindToSkipReason maps an item-kind tag to the closest
// SkipReason. Defaults to SkipUnknown when no specific match exists.
func mapKindToSkipReason(kind string) errors.SkipReason {
	switch kind {
	case "macro", "proc_macro":
		return errors.SkipMacro
	case "static":
		return errors.SkipConstant
	case "extern_crate", "primitive", "impl", "assoc_const", "assoc_type", "variant", "struct_field":
		return errors.SkipUnknown
	}
	return errors.SkipUnknown
}

// SortSkipped sorts the SkipReport slice by item path for stable
// golden output. Mutates in place.
func (s *ApiSurface) SortSkipped() {
	sort.SliceStable(s.Skipped, func(i, j int) bool {
		if s.Skipped[i].ItemPath == s.Skipped[j].ItemPath {
			return s.Skipped[i].Reason.String() < s.Skipped[j].Reason.String()
		}
		return s.Skipped[i].ItemPath < s.Skipped[j].ItemPath
	})
}

// Counts returns a snapshot of per-kind counts for a quick fixture
// assertion. Useful in tests that pin a "this many functions, this
// many structs" invariant against a known crate.
type Counts struct {
	Functions   int
	Structs     int
	Enums       int
	TypeAliases int
	Constants   int
	Traits      int
	Skipped     int
}

// Snapshot returns the Counts for s.
func (s *ApiSurface) Snapshot() Counts {
	return Counts{
		Functions:   len(s.Functions),
		Structs:     len(s.Structs),
		Enums:       len(s.Enums),
		TypeAliases: len(s.TypeAliases),
		Constants:   len(s.Constants),
		Traits:      len(s.Traits),
		Skipped:     len(s.Skipped),
	}
}
