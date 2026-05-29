package metacli

import "strings"

// ApiSurface is the distilled public API surface of an assembly, ready for
// type mapping and shim generation. It is analogous to rustdoc.ApiSurface in
// package3/rust/rustdoc/.
type ApiSurface struct {
	Assembly        string
	Version         string
	TargetFramework string
	Types           []SurfaceType
}

// SurfaceType is a public type with its translated methods and properties.
type SurfaceType struct {
	FullName   string // "Newtonsoft.Json.JsonConvert"
	ShortName  string // "JsonConvert"
	Namespace  string // "Newtonsoft.Json"
	Kind       TypeDefKind
	IsAbstract bool
	IsStatic   bool
	Methods    []SurfaceMethod
	Properties []SurfaceProp
}

// SurfaceMethod is a translatable public method.
type SurfaceMethod struct {
	Name       string
	IsStatic   bool
	IsAsync    bool
	ReturnType TypeRef
	Params     []ParamDef
	XmlDoc     string
}

// SurfaceProp is a translatable public property.
type SurfaceProp struct {
	Name      string
	Type      TypeRef
	HasGetter bool
	HasSetter bool
	IsStatic  bool
	XmlDoc    string
}

// Extract builds an ApiSurface from raw AssemblyMetadata. It applies basic
// filtering (removes types marked as obsolete, nested, delegate, or with
// no translatable surface) without full type-mapping (that happens in the
// typemap package).
func Extract(meta *AssemblyMetadata) *ApiSurface {
	if meta == nil {
		return &ApiSurface{}
	}
	surface := &ApiSurface{
		Assembly:        meta.Assembly,
		Version:         meta.Version,
		TargetFramework: meta.TargetFramework,
	}
	for _, td := range meta.Types {
		if shouldSkipType(td) {
			continue
		}
		st := buildSurfaceType(td)
		surface.Types = append(surface.Types, st)
	}
	return surface
}

// shouldSkipType returns true for types that the bridge cannot translate in v1.
func shouldSkipType(td TypeDef) bool {
	if td.IsObsolete {
		return true
	}
	if td.Kind == KindDelegate {
		return true
	}
	// Nested types are identified by a '+' in the name (CLR convention).
	if strings.Contains(td.Name, "+") {
		return true
	}
	return false
}

// buildSurfaceType converts a TypeDef into a SurfaceType, filtering methods
// and properties that should not be exposed.
func buildSurfaceType(td TypeDef) SurfaceType {
	ns := td.Namespace
	name := td.Name
	var fullName string
	if ns != "" {
		fullName = ns + "." + name
	} else {
		fullName = name
	}
	st := SurfaceType{
		FullName:   fullName,
		ShortName:  name,
		Namespace:  ns,
		Kind:       td.Kind,
		IsAbstract: td.IsAbstract,
		IsStatic:   td.IsStatic,
	}
	for _, m := range td.Methods {
		if m.IsObsolete {
			continue
		}
		st.Methods = append(st.Methods, SurfaceMethod{
			Name:       m.Name,
			IsStatic:   m.IsStatic,
			IsAsync:    m.IsAsync,
			ReturnType: m.ReturnType,
			Params:     m.Parameters,
			XmlDoc:     m.XmlDoc,
		})
	}
	for _, p := range td.Properties {
		if p.IsObsolete {
			continue
		}
		if p.IsIndexer {
			continue
		}
		st.Properties = append(st.Properties, SurfaceProp{
			Name:      p.Name,
			Type:      p.Type,
			HasGetter: p.HasGetter,
			HasSetter: p.HasSetter,
			IsStatic:  p.IsStatic,
			XmlDoc:    p.XmlDoc,
		})
	}
	return st
}
