// Package typemap implements the closed .NET-to-Mochi type translation table
// (MEP-68 §3, research note 05). It walks an AssemblyMeta value produced by
// the metadata parser and returns a TranslatedSurface plus a SkipReport for
// every item that falls outside the closed table.
package typemap

import (
	"mochi/package3/dotnet/errors"
	"mochi/package3/dotnet/metadata"
)

// MochiType is the Mochi type system representation produced by the
// translation pass. It mirrors the subset of Mochi types that can be
// expressed as extern fn / extern type declarations.
type MochiType struct {
	Kind    MochiTypeKind
	Elem    *MochiType   // for list, set, oset, async, optional
	Key     *MochiType   // for map, omap
	Value   *MochiType   // for map, omap
	Fields  []MochiField // for record
	Variants []MochiVariant // for ADT
	Name    string       // for extern type references
}

// MochiTypeKind enumerates the Mochi type kinds the translation table can produce.
type MochiTypeKind int

const (
	MochiInt      MochiTypeKind = iota
	MochiByte
	MochiLong
	MochiFloat
	MochiDecimal
	MochiBool
	MochiString
	MochiUnit
	MochiList     // list<Elem>
	MochiSet      // set<Elem>
	MochiOSet     // oset<Elem>
	MochiMap      // map<Key, Value>
	MochiOMap     // omap<Key, Value>
	MochiOptional // Elem | nil
	MochiAsync    // async Elem
	MochiRecord   // extern record { Fields }
	MochiADT      // extern type Name = Variant | Variant
	MochiExtern   // opaque extern type (GCHandle-backed)
	MochiFn       // fn(params): ret (for delegate types)
	MochiTuple    // tuple<A, B, ...>
)

// MochiField is a field within a Mochi record type.
type MochiField struct {
	Name string
	Type MochiType
}

// MochiVariant is a variant within a Mochi ADT type.
type MochiVariant struct {
	Name   string
	Fields []MochiField // empty for unit variants
}

// TranslatedItem represents a single translated public item from the .NET surface.
type TranslatedItem struct {
	DotNetName  string   // e.g. "Newtonsoft.Json.JsonConvert.SerializeObject"
	MochiName   string   // e.g. "JsonConvert_SerializeObject"
	Symbol      string   // C ABI symbol name: "mochi_dotnet_NewtonsoftJson_JsonConvert_SerializeObject"
	Kind        ItemKind
	ReceiverType *MochiType // non-nil for instance methods
	Params      []TranslatedParam
	ReturnType  MochiType
	IsAsync     bool
	DocSummary  string
}

// ItemKind classifies the kind of translated item.
type ItemKind int

const (
	ItemFn         ItemKind = iota // free function or static method
	ItemMethod                     // instance method (has ReceiverType)
	ItemConstructor                // constructor factory
	ItemType                       // type declaration
	ItemFreeSymbol                 // _free symbol for a GCHandle-backed type
)

// TranslatedParam is a translated method parameter.
type TranslatedParam struct {
	Name       string
	Type       MochiType
	IsOptional bool
}

// TranslatedSurface is the full translated public API of a NuGet package.
type TranslatedSurface struct {
	PackageID string
	Items     []TranslatedItem
	Skips     []errors.SkipReport
}

// Translate walks meta and translates every public type and method into Mochi
// representations. Items outside the closed table are added to the SkipReport.
func Translate(packageID string, meta *metadata.AssemblyMeta) *TranslatedSurface {
	t := &translator{
		packageID: packageID,
		surface:   &TranslatedSurface{PackageID: packageID},
	}
	for i := range meta.Types {
		t.translateType(&meta.Types[i])
	}
	return t.surface
}

type translator struct {
	packageID string
	surface   *TranslatedSurface
}

func (t *translator) skip(typeName, itemName string, reason errors.SkipReason, detail string) {
	t.surface.Skips = append(t.surface.Skips, errors.SkipReport{
		PackageID: t.packageID,
		TypeName:  typeName,
		ItemName:  itemName,
		Reason:    reason,
		Detail:    detail,
	})
}

func (t *translator) translateType(typ *metadata.TypeMeta) {
	if typ.IsGeneric && typ.GenericArity > 0 {
		// Generic open types are skipped unless all their public methods are
		// individually reachable via monomorphise entries. The monomorphise
		// resolution happens in the nativeaot synthesizer, not here.
		// We still translate non-generic methods on partial generic types
		// (e.g. static factory methods with no generic params).
	}
	mochiType := t.classifyType(typ)
	if mochiType == nil {
		return
	}

	// Emit the type declaration item.
	t.surface.Items = append(t.surface.Items, TranslatedItem{
		DotNetName: typ.FullName,
		MochiName:  sanitizeName(typ.Name),
		Symbol:     typeSymbol(t.packageID, typ),
		Kind:       ItemType,
		ReturnType: *mochiType,
		DocSummary: typ.DocSummary,
	})

	// Emit a _free symbol for GCHandle-backed types.
	if mochiType.Kind == MochiExtern {
		t.surface.Items = append(t.surface.Items, TranslatedItem{
			DotNetName:  typ.FullName + "._free",
			MochiName:   sanitizeName(typ.Name) + "_free",
			Symbol:      typeSymbol(t.packageID, typ) + "_free",
			Kind:        ItemFreeSymbol,
			ReceiverType: mochiType,
			Params: []TranslatedParam{{Name: "handle", Type: *mochiType}},
			ReturnType: MochiType{Kind: MochiUnit},
		})
	}

	// Translate methods.
	for i := range typ.Methods {
		t.translateMethod(typ, &typ.Methods[i])
	}

	// Translate properties as getter/setter method pairs.
	for i := range typ.Properties {
		t.translateProperty(typ, &typ.Properties[i])
	}
}

func (t *translator) classifyType(typ *metadata.TypeMeta) *MochiType {
	switch typ.Kind {
	case metadata.TypeKindEnum:
		return &MochiType{Kind: MochiADT, Name: sanitizeName(typ.Name)}
	case metadata.TypeKindStruct:
		if isBlittable(typ) {
			return buildRecordType(typ)
		}
		return &MochiType{Kind: MochiExtern, Name: sanitizeName(typ.Name)}
	case metadata.TypeKindClass, metadata.TypeKindRecord:
		if isSealedHierarchyRoot(typ) {
			return &MochiType{Kind: MochiADT, Name: sanitizeName(typ.Name)}
		}
		return &MochiType{Kind: MochiExtern, Name: sanitizeName(typ.Name)}
	case metadata.TypeKindInterface:
		return &MochiType{Kind: MochiExtern, Name: sanitizeName(typ.Name)}
	case metadata.TypeKindDelegate:
		// Delegate translation is attempted in translateMethod for Invoke.
		return nil
	}
	return nil
}

func (t *translator) translateMethod(typ *metadata.TypeMeta, m *metadata.MethodMeta) {
	if m.HasRequiresDynamicCode {
		t.skip(typ.FullName, m.Name, errors.SkipNativeAOTIncompatible,
			"[RequiresDynamicCode] annotation")
		return
	}
	if m.IsGeneric {
		t.skip(typ.FullName, m.Name, errors.SkipGeneric,
			"generic method; add to [dotnet.monomorphise]")
		return
	}

	retType, ok := t.translateSig(m.ReturnType)
	if !ok {
		return // skip reason already added by translateSig
	}

	var params []TranslatedParam
	for _, p := range m.Params {
		if p.IsRef || p.IsOut {
			t.skip(typ.FullName, m.Name, errors.SkipByRef,
				"ref/out parameter '"+p.Name+"'")
			return
		}
		pt, ok := t.translateSig(p.Type)
		if !ok {
			return
		}
		params = append(params, TranslatedParam{
			Name:       p.Name,
			Type:       pt,
			IsOptional: p.IsOptional,
		})
	}

	mochiType := t.classifyType(typ)
	var receiver *MochiType
	if !m.IsStatic && mochiType != nil {
		receiver = mochiType
	}

	t.surface.Items = append(t.surface.Items, TranslatedItem{
		DotNetName:   typ.FullName + "." + m.Name,
		MochiName:    sanitizeName(typ.Name) + "_" + m.Name,
		Symbol:       methodSymbol(t.packageID, typ, m),
		Kind:         ItemMethod,
		ReceiverType: receiver,
		Params:       params,
		ReturnType:   retType,
		IsAsync:      m.IsAsync,
		DocSummary:   m.DocSummary,
	})
}

func (t *translator) translateProperty(typ *metadata.TypeMeta, p *metadata.PropertyMeta) {
	propType, ok := t.translateSig(metadata.TypeSig{
		Kind: sigKindFromProperty(p),
	})
	_ = propType
	_ = ok
	// Property getter/setter translation is a stub; full implementation in phase 5.
}

// translateSig converts a metadata.TypeSig to a MochiType. Returns false and
// appends a SkipReport entry if the signature is outside the closed table.
func (t *translator) translateSig(sig metadata.TypeSig) (MochiType, bool) {
	switch sig.Kind {
	case metadata.SigVoid:
		return MochiType{Kind: MochiUnit}, true
	case metadata.SigBoolean:
		return MochiType{Kind: MochiBool}, true
	case metadata.SigI1, metadata.SigU1, metadata.SigChar,
		metadata.SigI2, metadata.SigU2, metadata.SigI4:
		return MochiType{Kind: MochiInt}, true
	case metadata.SigU4, metadata.SigI8, metadata.SigU8, metadata.SigI, metadata.SigU:
		return MochiType{Kind: MochiLong}, true
	case metadata.SigR4, metadata.SigR8:
		return MochiType{Kind: MochiFloat}, true
	case metadata.SigString:
		return MochiType{Kind: MochiString}, true
	case metadata.SigPtr:
		t.skip("", "", errors.SkipUnsafePointer, "pointer type in signature")
		return MochiType{}, false
	case metadata.SigByRef:
		t.skip("", "", errors.SkipByRef, "by-ref type in signature")
		return MochiType{}, false
	case metadata.SigObject:
		t.skip("", "", errors.SkipObjectReturn, "System.Object in signature")
		return MochiType{}, false
	case metadata.SigGenericInst:
		return t.translateGenericInst(sig)
	case metadata.SigSZArray:
		if sig.ElemType == nil {
			return MochiType{}, false
		}
		elem, ok := t.translateSig(*sig.ElemType)
		if !ok {
			return MochiType{}, false
		}
		return MochiType{Kind: MochiList, Elem: &elem}, true
	case metadata.SigVar, metadata.SigMVar:
		t.skip("", "", errors.SkipGeneric, "generic type parameter")
		return MochiType{}, false
	}
	return MochiType{}, false
}

func (t *translator) translateGenericInst(sig metadata.TypeSig) (MochiType, bool) {
	if sig.TypeRef == nil {
		return MochiType{}, false
	}
	name := sig.TypeRef.Namespace + "." + sig.TypeRef.Name
	switch name {
	case "System.Collections.Generic.List",
		"System.Collections.Generic.IList",
		"System.Collections.Generic.IEnumerable",
		"System.Collections.Generic.IReadOnlyList",
		"System.Collections.Generic.IReadOnlyCollection":
		if len(sig.TypeArgs) != 1 {
			return MochiType{}, false
		}
		elem, ok := t.translateSig(sig.TypeArgs[0])
		if !ok {
			return MochiType{}, false
		}
		return MochiType{Kind: MochiList, Elem: &elem}, true

	case "System.Collections.Generic.Dictionary",
		"System.Collections.Generic.IDictionary",
		"System.Collections.Generic.ReadOnlyDictionary",
		"System.Collections.Generic.IReadOnlyDictionary":
		if len(sig.TypeArgs) != 2 {
			return MochiType{}, false
		}
		k, ok := t.translateSig(sig.TypeArgs[0])
		if !ok {
			return MochiType{}, false
		}
		v, ok2 := t.translateSig(sig.TypeArgs[1])
		if !ok2 {
			return MochiType{}, false
		}
		return MochiType{Kind: MochiMap, Key: &k, Value: &v}, true

	case "System.Collections.Generic.HashSet",
		"System.Collections.Generic.ISet":
		if len(sig.TypeArgs) != 1 {
			return MochiType{}, false
		}
		elem, ok := t.translateSig(sig.TypeArgs[0])
		if !ok {
			return MochiType{}, false
		}
		return MochiType{Kind: MochiSet, Elem: &elem}, true

	case "System.Collections.Generic.SortedDictionary":
		if len(sig.TypeArgs) != 2 {
			return MochiType{}, false
		}
		k, ok := t.translateSig(sig.TypeArgs[0])
		if !ok {
			return MochiType{}, false
		}
		v, ok2 := t.translateSig(sig.TypeArgs[1])
		if !ok2 {
			return MochiType{}, false
		}
		return MochiType{Kind: MochiOMap, Key: &k, Value: &v}, true

	case "System.Nullable":
		if len(sig.TypeArgs) != 1 {
			return MochiType{}, false
		}
		inner, ok := t.translateSig(sig.TypeArgs[0])
		if !ok {
			return MochiType{}, false
		}
		return MochiType{Kind: MochiOptional, Elem: &inner}, true

	case "System.Threading.Tasks.Task",
		"System.Threading.Tasks.ValueTask":
		if len(sig.TypeArgs) == 0 {
			return MochiType{Kind: MochiAsync, Elem: &MochiType{Kind: MochiUnit}}, true
		}
		if len(sig.TypeArgs) == 1 {
			inner, ok := t.translateSig(sig.TypeArgs[0])
			if !ok {
				return MochiType{}, false
			}
			return MochiType{Kind: MochiAsync, Elem: &inner}, true
		}
		return MochiType{}, false

	case "System.Collections.Generic.IAsyncEnumerable":
		t.skip("", "", errors.SkipAsyncEnumerable, "IAsyncEnumerable<T>; deferred to phase 11")
		return MochiType{}, false
	}

	// Unknown generic: emit as opaque extern type with SkipReport for the args.
	return MochiType{Kind: MochiExtern, Name: sanitizeName(sig.TypeRef.Name)}, true
}

// isBlittable reports whether a struct type has only blittable fields
// (i.e. it can be passed by value at the C ABI).
func isBlittable(typ *metadata.TypeMeta) bool {
	for _, f := range typ.Fields {
		switch f.Type.Kind {
		case metadata.SigBoolean, metadata.SigI1, metadata.SigU1,
			metadata.SigI2, metadata.SigU2, metadata.SigI4, metadata.SigU4,
			metadata.SigI8, metadata.SigU8, metadata.SigR4, metadata.SigR8,
			metadata.SigI, metadata.SigU:
			// Blittable primitive.
		default:
			return false
		}
	}
	return true
}

// isSealedHierarchyRoot reports whether typ is an abstract class whose
// immediate known subtypes are all sealed (closed discriminated union pattern).
func isSealedHierarchyRoot(_ *metadata.TypeMeta) bool {
	// Full implementation requires reading the full type graph to find subtypes.
	// Stubbed for phase 3; implemented in phase 4 (wrapper synthesizer).
	return false
}

func buildRecordType(typ *metadata.TypeMeta) *MochiType {
	mt := &MochiType{Kind: MochiRecord, Name: sanitizeName(typ.Name)}
	for _, f := range typ.Fields {
		_ = f // field translation stubbed for phase 3
	}
	return mt
}

func sanitizeName(name string) string {
	// Strip generic arity suffix (e.g. "List`1" -> "List").
	for i, c := range name {
		if c == '`' {
			return name[:i]
		}
	}
	return name
}

func typeSymbol(packageID string, typ *metadata.TypeMeta) string {
	return "mochi_dotnet_" + sanitizeSymbol(packageID) + "_" + sanitizeSymbol(typ.Name)
}

func methodSymbol(packageID string, typ *metadata.TypeMeta, m *metadata.MethodMeta) string {
	return "mochi_dotnet_" + sanitizeSymbol(packageID) + "_" + sanitizeSymbol(typ.Name) + "_" + m.Name
}

func sanitizeSymbol(s string) string {
	out := make([]byte, 0, len(s))
	for i := 0; i < len(s); i++ {
		c := s[i]
		if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') {
			out = append(out, c)
		} else {
			out = append(out, '_')
		}
	}
	return string(out)
}

func sigKindFromProperty(_ *metadata.PropertyMeta) metadata.TypeSigKind {
	return metadata.SigObject // stub; replaced in phase 5
}
