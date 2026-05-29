package typemap

import (
	"errors"
	"fmt"

	"mochi/package3/go/apisurface"
)

// ErrUnmappable is returned when a Go type has no Mochi mapping.
// Phase 6 treats this as a skipReason and adds a SkipNote to the
// generated wrapper output rather than failing the whole module.
var ErrUnmappable = errors.New("typemap: unmappable Go type")

// Mapper turns apisurface.GoType expressions into Mochi types +
// transfer directives. The mapper is stateful only in the lookup
// cache: identical type expressions return the same Mapping.
type Mapper struct {
	surface *apisurface.Surface
	cache   map[string]*Mapping
}

// NewMapper constructs a Mapper bound to surface. The surface is
// consulted for cross-reference resolution (e.g. mapping a named
// type to its underlying struct fields). A nil surface means
// cross-references are ignored; named types map to handles.
func NewMapper(surface *apisurface.Surface) *Mapper {
	return &Mapper{surface: surface, cache: make(map[string]*Mapping)}
}

// Map maps t to a Mochi type. Returns ErrUnmappable wrapped with
// the offending shape on failure.
func (m *Mapper) Map(t apisurface.GoType) (*Mapping, error) {
	key := ""
	if t != nil {
		key = t.String()
		if cached, ok := m.cache[key]; ok {
			return cached, nil
		}
	}
	out, err := m.mapType(t)
	if err == nil && t != nil {
		m.cache[key] = out
	}
	return out, err
}

func (m *Mapper) mapType(t apisurface.GoType) (*Mapping, error) {
	switch v := t.(type) {
	case nil:
		return nil, fmt.Errorf("%w: nil type", ErrUnmappable)
	case apisurface.BasicType:
		return mapBasic(v)
	case apisurface.PointerType:
		return m.mapPointer(v)
	case apisurface.SliceType:
		return m.mapSlice(v)
	case apisurface.ArrayType:
		return m.mapArray(v)
	case apisurface.MapType:
		return m.mapMochiMap(v)
	case apisurface.ChanType:
		return mapChan(v)
	case apisurface.FuncType:
		return m.mapFunc(v)
	case apisurface.EllipsisType:
		return m.mapEllipsis(v)
	case apisurface.InterfaceType:
		return mapInterface(v)
	case apisurface.StructType:
		return mapStruct(v)
	case apisurface.NamedType:
		return m.mapNamed(v)
	default:
		return nil, fmt.Errorf("%w: %T", ErrUnmappable, t)
	}
}

// scalarMap defines the Go-basic-to-Mochi-scalar mapping. Every
// concrete Go basic type appears here; "any" and "comparable" map
// to AnyType (with a Note marking the unboxing requirement).
var scalarMap = map[string]ScalarType{
	"bool":    {Name: "bool"},
	"string":  {Name: "string"},
	"int":     {Name: "int"},
	"int8":    {Name: "int"},
	"int16":   {Name: "int"},
	"int32":   {Name: "int"},
	"int64":   {Name: "int"},
	"uint":    {Name: "int"},
	"uint8":   {Name: "int"},
	"uint16":  {Name: "int"},
	"uint32":  {Name: "int"},
	"uint64":  {Name: "int"},
	"uintptr": {Name: "int"},
	"byte":    {Name: "int"},
	"rune":    {Name: "int"},
	"float32": {Name: "float"},
	"float64": {Name: "float"},
	"error":   {Name: "error"},
}

func mapBasic(b apisurface.BasicType) (*Mapping, error) {
	if s, ok := scalarMap[b.Name]; ok {
		notes := []string(nil)
		if b.Name != s.Name && s.Name != "int" && s.Name != "float" {
			// Currently never reached; left for future expansion.
		}
		if isWideningSourced(b.Name) {
			notes = append(notes, "Go "+b.Name+" widens to Mochi int")
		}
		return &Mapping{Mochi: s, Direction: Copy, Notes: notes}, nil
	}
	if b.Name == "any" || b.Name == "comparable" {
		return &Mapping{Mochi: AnyType{}, Direction: Handle, Notes: []string{"Go " + b.Name + " maps to Mochi any via handle"}}, nil
	}
	return nil, fmt.Errorf("%w: basic %s", ErrUnmappable, b.Name)
}

func isWideningSourced(name string) bool {
	switch name {
	case "int8", "int16", "int32", "uint", "uint8", "uint16", "uint32", "byte", "rune", "uintptr":
		return true
	}
	return false
}

func (m *Mapper) mapPointer(p apisurface.PointerType) (*Mapping, error) {
	inner, err := m.Map(p.Elem)
	if err != nil {
		return nil, fmt.Errorf("%w: pointer elem: %v", ErrUnmappable, err)
	}
	// Pointer-to-scalar / list / map / record -> option<T>, Copy.
	// Pointer-to-handle -> Handle (no double-wrapping).
	switch inner.Mochi.(type) {
	case HandleType:
		return inner, nil
	}
	return &Mapping{Mochi: OptionType{Elem: inner.Mochi}, Direction: inner.Direction, Notes: append([]string{"Go pointer maps to Mochi option"}, inner.Notes...)}, nil
}

func (m *Mapper) mapSlice(s apisurface.SliceType) (*Mapping, error) {
	if b, ok := s.Elem.(apisurface.BasicType); ok && b.Name == "byte" {
		return &Mapping{Mochi: ScalarType{Name: "bytes"}, Direction: Copy, Notes: []string{"[]byte maps to Mochi bytes"}}, nil
	}
	inner, err := m.Map(s.Elem)
	if err != nil {
		return nil, fmt.Errorf("%w: slice elem: %v", ErrUnmappable, err)
	}
	dir := inner.Direction
	if dir == View {
		dir = Copy // a list view of element views needs flattening; for now copy.
	}
	return &Mapping{Mochi: ListType{Elem: inner.Mochi}, Direction: dir}, nil
}

func (m *Mapper) mapArray(a apisurface.ArrayType) (*Mapping, error) {
	// Arrays are length-locked and bridged as fixed-length lists.
	inner, err := m.Map(a.Elem)
	if err != nil {
		return nil, fmt.Errorf("%w: array elem: %v", ErrUnmappable, err)
	}
	return &Mapping{Mochi: ListType{Elem: inner.Mochi}, Direction: Copy, Notes: []string{fmt.Sprintf("Go [%d] array maps to Mochi list (length erased)", a.Len)}}, nil
}

func (m *Mapper) mapMochiMap(mp apisurface.MapType) (*Mapping, error) {
	k, err := m.Map(mp.Key)
	if err != nil {
		return nil, fmt.Errorf("%w: map key: %v", ErrUnmappable, err)
	}
	if _, ok := k.Mochi.(ScalarType); !ok {
		return nil, fmt.Errorf("%w: map key is not a scalar: %s", ErrUnmappable, k.Mochi)
	}
	v, err := m.Map(mp.Value)
	if err != nil {
		return nil, fmt.Errorf("%w: map value: %v", ErrUnmappable, err)
	}
	return &Mapping{Mochi: MochiMap{Key: k.Mochi, Value: v.Mochi}, Direction: Copy}, nil
}

func mapChan(c apisurface.ChanType) (*Mapping, error) {
	name := "chan<" + c.Elem.String() + ">"
	notes := []string{"channels are bridged via the cgo handle pool (phase 14)"}
	return &Mapping{Mochi: HandleType{Name: name}, Direction: Handle, Notes: notes}, nil
}

func (m *Mapper) mapFunc(f apisurface.FuncType) (*Mapping, error) {
	var params []MochiType
	for i, p := range f.Params {
		// Strip ellipsis: callers see a regular list of T.
		if e, ok := p.(apisurface.EllipsisType); ok && i == len(f.Params)-1 {
			inner, err := m.Map(e.Elem)
			if err != nil {
				return nil, fmt.Errorf("%w: variadic param: %v", ErrUnmappable, err)
			}
			params = append(params, ListType{Elem: inner.Mochi})
			continue
		}
		mp, err := m.Map(p)
		if err != nil {
			return nil, fmt.Errorf("%w: func param: %v", ErrUnmappable, err)
		}
		params = append(params, mp.Mochi)
	}
	var results []MochiType
	for _, r := range f.Results {
		mp, err := m.Map(r)
		if err != nil {
			return nil, fmt.Errorf("%w: func result: %v", ErrUnmappable, err)
		}
		results = append(results, mp.Mochi)
	}
	return &Mapping{Mochi: FuncType{Params: params, Results: results}, Direction: Handle, Notes: []string{"func values bridge via the cgo handle pool"}}, nil
}

func (m *Mapper) mapEllipsis(e apisurface.EllipsisType) (*Mapping, error) {
	// At top level (not inside a func sig), an ellipsis collapses to
	// list<T>.
	inner, err := m.Map(e.Elem)
	if err != nil {
		return nil, err
	}
	return &Mapping{Mochi: ListType{Elem: inner.Mochi}, Direction: inner.Direction}, nil
}

func mapInterface(i apisurface.InterfaceType) (*Mapping, error) {
	// Empty interface{} maps to any; everything else is a handle.
	src := i.Source
	if src == "interface{}" || src == "interface {}" {
		return &Mapping{Mochi: AnyType{}, Direction: Handle, Notes: []string{"interface{} maps to any via handle"}}, nil
	}
	return &Mapping{Mochi: HandleType{Name: "iface"}, Direction: Handle, Notes: []string{"anonymous interface maps to a handle; method set is opaque"}}, nil
}

func mapStruct(s apisurface.StructType) (*Mapping, error) {
	return &Mapping{Mochi: HandleType{Name: "struct"}, Direction: Handle, Notes: []string{"anonymous struct literal maps to a handle (fields opaque)"}}, nil
}

func (m *Mapper) mapNamed(n apisurface.NamedType) (*Mapping, error) {
	// stdlib special-cases.
	if n.PackagePath == "" || n.PackagePath == "_self" {
		// Self-package named type: consult the surface (if any) to
		// see if it is a struct, alias, or scalar wrapper.
		if m.surface != nil {
			if td := m.lookupSelfType(n.Name); td != nil {
				return m.mapTypeDecl(td)
			}
		}
		// Fall back to handle so the bridge can still expose the
		// value as opaque.
		return &Mapping{Mochi: HandleType{Name: n.Name}, Direction: Handle, Notes: []string{"self-package named type with no surface info"}}, nil
	}
	// Cross-package named type: try to find it in the surface.
	if m.surface != nil {
		if td := m.lookupCrossType(n.PackagePath, n.Name); td != nil {
			return m.mapTypeDecl(td)
		}
	}
	// Default: handle keyed by qualified name.
	return &Mapping{Mochi: HandleType{Name: n.PackagePath + "." + n.Name}, Direction: Handle, Notes: []string{"cross-package named type defaults to handle"}}, nil
}

func (m *Mapper) lookupSelfType(name string) *apisurface.TypeDecl {
	// Walk every package; the self-package convention is ambiguous
	// when multiple packages are in play. Phase 6 will narrow the
	// scope.
	for _, pkg := range m.surface.PackagePaths() {
		if td := m.surface.LookupType(pkg, name); td != nil {
			return td
		}
	}
	return nil
}

func (m *Mapper) lookupCrossType(pkg, name string) *apisurface.TypeDecl {
	return m.surface.LookupType(pkg, name)
}

func (m *Mapper) mapTypeDecl(td *apisurface.TypeDecl) (*Mapping, error) {
	switch td.Kind {
	case apisurface.KindAlias:
		if td.AliasOf != nil {
			return m.Map(td.AliasOf)
		}
		return &Mapping{Mochi: HandleType{Name: td.Underlying.Name}, Direction: Handle, Notes: []string{"alias target unresolved"}}, nil
	case apisurface.KindBasic:
		if td.UnderlyingType != nil {
			return m.Map(td.UnderlyingType)
		}
		return &Mapping{Mochi: HandleType{Name: td.Underlying.Name}, Direction: Handle}, nil
	case apisurface.KindStruct:
		return m.mapStructDecl(td)
	case apisurface.KindInterface:
		// Interfaces always handle.
		return &Mapping{Mochi: HandleType{Name: td.Underlying.Name}, Direction: Handle, Notes: []string{"interface bridges as handle (method set on Mochi side via wrapper)"}}, nil
	case apisurface.KindSlice, apisurface.KindMap, apisurface.KindChan, apisurface.KindArray, apisurface.KindFunc, apisurface.KindPointer:
		// Unfold via the underlying.
		if td.UnderlyingType != nil {
			return m.Map(td.UnderlyingType)
		}
	}
	return &Mapping{Mochi: HandleType{Name: td.Underlying.Name}, Direction: Handle, Notes: []string{"named type with unknown kind defaults to handle"}}, nil
}

func (m *Mapper) mapStructDecl(td *apisurface.TypeDecl) (*Mapping, error) {
	// A struct is record-bridgeable iff every Exported field is
	// itself record-bridgeable as Copy (recursively). Otherwise we
	// fall back to a handle.
	fields := []RecordField{}
	for _, f := range td.Fields {
		if !f.Exported {
			// An unexported field forces handle. The bridge can't
			// see private state.
			return &Mapping{Mochi: HandleType{Name: td.Underlying.Name}, Direction: Handle, Notes: []string{"struct has unexported fields"}}, nil
		}
		fm, err := m.Map(f.Type)
		if err != nil {
			return &Mapping{Mochi: HandleType{Name: td.Underlying.Name}, Direction: Handle, Notes: []string{"struct field unmappable: " + f.Name}}, nil
		}
		if fm.Direction == Handle {
			// A field that bridges as a handle (e.g. nested *mu
			// sync.Mutex) forces the whole struct to be a handle.
			return &Mapping{Mochi: HandleType{Name: td.Underlying.Name}, Direction: Handle, Notes: []string{"struct field " + f.Name + " requires handle"}}, nil
		}
		fields = append(fields, RecordField{Name: f.Name, Type: fm.Mochi})
	}
	return &Mapping{Mochi: RecordType{Name: td.Underlying.Name, Fields: fields}, Direction: Copy}, nil
}

// MapFunc maps an apisurface.FuncDecl (a top-level func or method)
// to its Mochi signature. The returned Mapping's Mochi is always a
// FuncType.
func (m *Mapper) MapFunc(fd *apisurface.FuncDecl) (*Mapping, error) {
	if fd == nil {
		return nil, fmt.Errorf("%w: nil FuncDecl", ErrUnmappable)
	}
	ft := apisurface.FuncType{Variadic: fd.Underlying.Variadic}
	for _, p := range fd.Params {
		ft.Params = append(ft.Params, p.Type)
	}
	for _, r := range fd.Results {
		ft.Results = append(ft.Results, r.Type)
	}
	return m.mapFunc(ft)
}
