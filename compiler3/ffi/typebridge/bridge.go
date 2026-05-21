package typebridge

import (
	"go/types"
	"sort"
)

// GoToMochi maps a go/types.Type to a structural Mochi-side Type.
// Unsupported Go shapes degrade to KindOpaque with a documented
// OpaqueReason; the function never returns Type{}.
//
// Panics only if t == nil; that is a caller bug.
func GoToMochi(t types.Type) Type {
	if t == nil {
		panic("typebridge: GoToMochi called with nil types.Type")
	}
	return goToMochi(t, &walkCtx{seen: map[*types.Named]struct{}{}})
}

// walkCtx carries the cycle guard plus a flag that suppresses method
// extraction for types reached through another type's method
// signature. Methods are only populated for the top-level Named (or
// Named-underlying-iface) the caller asked about; nested references
// reach their own method sets through their own GoToMochi entry.
type walkCtx struct {
	seen     map[*types.Named]struct{}
	inMethod bool
}

func goToMochi(t types.Type, ctx *walkCtx) Type {
	t = types.Unalias(t)
	switch u := t.(type) {
	case *types.Basic:
		return basicToMochi(u)
	case *types.Slice:
		elem := goToMochi(u.Elem(), ctx)
		if elem.Kind == KindUint && elem.Width == 8 {
			return Type{Kind: KindBytes}
		}
		return Type{Kind: KindList, Elem: &elem}
	case *types.Array:
		elem := goToMochi(u.Elem(), ctx)
		return Type{Kind: KindArray, Elem: &elem, ArrayLen: u.Len()}
	case *types.Map:
		k := goToMochi(u.Key(), ctx)
		v := goToMochi(u.Elem(), ctx)
		return Type{Kind: KindMap, Key: &k, Elem: &v}
	case *types.Pointer:
		e := goToMochi(u.Elem(), ctx)
		return Type{Kind: KindRef, Elem: &e}
	case *types.Chan:
		e := goToMochi(u.Elem(), ctx)
		return Type{Kind: KindChan, Elem: &e, ChanDir: chanDirFromGo(u.Dir())}
	case *types.Signature:
		return signatureToMochi(u, ctx)
	case *types.Struct:
		return structToMochi(u, ctx)
	case *types.Interface:
		return interfaceToMochi(u, "", "", ctx)
	case *types.Named:
		return namedToMochi(u, ctx)
	case *types.TypeParam:
		return Type{Kind: KindTypeParam, Name: u.Obj().Name()}
	case *types.Tuple:
		return Type{Kind: KindOpaque, OpaqueReason: OpaqueTuple, GoType: types.TypeString(t, nil)}
	default:
		return Type{Kind: KindOpaque, OpaqueReason: OpaqueUnknown, GoType: types.TypeString(t, nil)}
	}
}

func basicToMochi(b *types.Basic) Type {
	info := b.Info()
	if info&types.IsUntyped != 0 {
		switch b.Kind() {
		case types.UntypedBool:
			return Type{Kind: KindBool}
		case types.UntypedInt:
			return Type{Kind: KindInt}
		case types.UntypedRune:
			return Type{Kind: KindInt, Width: 32}
		case types.UntypedFloat:
			return Type{Kind: KindFloat, Width: 64}
		case types.UntypedComplex:
			return Type{Kind: KindOpaque, OpaqueReason: OpaqueComplex, GoType: "complex128"}
		case types.UntypedString:
			return Type{Kind: KindString}
		case types.UntypedNil:
			return Type{Kind: KindUntyped, GoType: "nil"}
		}
	}
	switch b.Kind() {
	case types.Bool:
		return Type{Kind: KindBool}
	case types.Int:
		return Type{Kind: KindInt, Width: 0}
	case types.Int8:
		return Type{Kind: KindInt, Width: 8}
	case types.Int16:
		return Type{Kind: KindInt, Width: 16}
	case types.Int32:
		return Type{Kind: KindInt, Width: 32}
	case types.Int64:
		return Type{Kind: KindInt, Width: 64}
	case types.Uint:
		return Type{Kind: KindUint, Width: 0}
	case types.Uint8:
		return Type{Kind: KindUint, Width: 8}
	case types.Uint16:
		return Type{Kind: KindUint, Width: 16}
	case types.Uint32:
		return Type{Kind: KindUint, Width: 32}
	case types.Uint64:
		return Type{Kind: KindUint, Width: 64}
	case types.Uintptr:
		return Type{Kind: KindOpaque, OpaqueReason: OpaqueUintptr, GoType: "uintptr"}
	case types.Float32:
		return Type{Kind: KindFloat, Width: 32}
	case types.Float64:
		return Type{Kind: KindFloat, Width: 64}
	case types.Complex64:
		return Type{Kind: KindOpaque, OpaqueReason: OpaqueComplex, GoType: "complex64"}
	case types.Complex128:
		return Type{Kind: KindOpaque, OpaqueReason: OpaqueComplex, GoType: "complex128"}
	case types.String:
		return Type{Kind: KindString}
	case types.UnsafePointer:
		return Type{Kind: KindOpaque, OpaqueReason: OpaqueUnsafePointer, GoType: "unsafe.Pointer"}
	}
	return Type{Kind: KindOpaque, OpaqueReason: OpaqueUnknown, GoType: b.Name()}
}

func chanDirFromGo(d types.ChanDir) ChanDir {
	switch d {
	case types.SendOnly:
		return ChanSend
	case types.RecvOnly:
		return ChanRecv
	case types.SendRecv:
		return ChanBoth
	}
	return ChanInvalid
}

func signatureToMochi(sig *types.Signature, ctx *walkCtx) Type {
	out := Type{Kind: KindFunc, Variadic: sig.Variadic()}
	params := sig.Params()
	for i := 0; i < params.Len(); i++ {
		out.Params = append(out.Params, goToMochi(params.At(i).Type(), ctx))
	}
	results := sig.Results()
	for i := 0; i < results.Len(); i++ {
		out.Results = append(out.Results, goToMochi(results.At(i).Type(), ctx))
	}
	return out
}

func structToMochi(s *types.Struct, ctx *walkCtx) Type {
	out := Type{Kind: KindStruct}
	hasUnexported := false
	for i := 0; i < s.NumFields(); i++ {
		f := s.Field(i)
		tag := s.Tag(i)
		ft := goToMochi(f.Type(), ctx)
		if !f.Exported() {
			hasUnexported = true
		}
		out.Fields = append(out.Fields, Field{
			Name:     f.Name(),
			Type:     ft,
			Tag:      tag,
			Embedded: f.Embedded(),
			Exported: f.Exported(),
		})
	}
	if hasUnexported {
		out.OpaqueReason = OpaqueUnexportedField
	}
	return out
}

func interfaceToMochi(u *types.Interface, name, pkgPath string, ctx *walkCtx) Type {
	out := Type{Kind: KindIface, Name: name, PkgPath: pkgPath}
	if ctx.inMethod {
		// Nested interface reached through a method signature; emit the
		// identity only, not the method set. The resolver pulls methods
		// for nested types on its own pass.
		return out
	}
	sub := &walkCtx{seen: ctx.seen, inMethod: true}
	for i := 0; i < u.NumMethods(); i++ {
		m := u.Method(i)
		sig := goToMochi(m.Type(), sub)
		out.Methods = append(out.Methods, Method{
			Name:      m.Name(),
			Exported:  m.Exported(),
			Signature: sig,
		})
	}
	sortMethods(out.Methods)
	return out
}

func namedToMochi(n *types.Named, ctx *walkCtx) Type {
	obj := n.Obj()
	pkgPath := ""
	if obj.Pkg() != nil {
		pkgPath = obj.Pkg().Path()
	}
	if _, ok := ctx.seen[n]; ok {
		// Shallow re-entry to break self-referential cycles. The Phase
		// 2 resolver re-joins this via the binding table.
		return Type{Kind: KindNamed, Name: obj.Name(), PkgPath: pkgPath, OpaqueReason: OpaqueRecursiveType, GoType: types.TypeString(n, nil)}
	}
	ctx.seen[n] = struct{}{}
	defer delete(ctx.seen, n)

	underlying := n.Underlying()

	var typeArgs []Type
	if ta := n.TypeArgs(); ta != nil && ta.Len() > 0 {
		for i := 0; i < ta.Len(); i++ {
			typeArgs = append(typeArgs, goToMochi(ta.At(i), ctx))
		}
	}

	if iface, ok := underlying.(*types.Interface); ok {
		out := interfaceToMochi(iface, obj.Name(), pkgPath, ctx)
		out.TypeArgs = typeArgs
		return out
	}
	under := goToMochi(underlying, ctx)
	var methods []Method
	if !ctx.inMethod {
		methods = extractMethodSet(n, ctx)
	}
	return Type{
		Kind:     KindNamed,
		Name:     obj.Name(),
		PkgPath:  pkgPath,
		Elem:     &under,
		TypeArgs: typeArgs,
		Methods:  methods,
		GoType:   types.TypeString(n, nil),
	}
}

func extractMethodSet(n *types.Named, ctx *walkCtx) []Method {
	var methods []Method
	already := map[string]bool{}
	sub := &walkCtx{seen: ctx.seen, inMethod: true}
	collect := func(ms *types.MethodSet) {
		for i := 0; i < ms.Len(); i++ {
			sel := ms.At(i)
			f, ok := sel.Obj().(*types.Func)
			if !ok {
				continue
			}
			if already[f.Name()] {
				continue
			}
			already[f.Name()] = true
			sig := goToMochi(f.Type(), sub)
			methods = append(methods, Method{
				Name:      f.Name(),
				Exported:  f.Exported(),
				Signature: sig,
			})
		}
	}
	collect(types.NewMethodSet(n))
	collect(types.NewMethodSet(types.NewPointer(n)))
	sortMethods(methods)
	return methods
}

func sortMethods(ms []Method) {
	sort.Slice(ms, func(i, j int) bool { return ms[i].Name < ms[j].Name })
}
