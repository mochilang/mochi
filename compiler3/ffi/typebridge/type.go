package typebridge

import (
	"fmt"
	"io"
)

// Kind tags the structural shape of Type. Every field on Type is
// only meaningful for the Kind values listed in that field's doc
// comment.
type Kind uint8

const (
	KindInvalid   Kind = iota // zero value; never produced by GoToMochi
	KindBool                  // bool
	KindInt                   // signed integer; Width is bit width (0 = machine int)
	KindUint                  // unsigned integer; Width is bit width (0 = machine uint)
	KindFloat                 // IEEE 754; Width is 32 or 64
	KindString                // string
	KindBytes                 // []byte (called out to let the emitter pick fast paths)
	KindList                  // slice []T; Elem is set
	KindArray                 // [N]T; Elem set, ArrayLen >= 0
	KindMap                   // map[K]V; Key, Elem set
	KindStruct                // struct { ... }; Fields set
	KindRef                   // *T; Elem set
	KindIface                 // interface{ ... } or named interface; Name (optional), PkgPath, Methods set
	KindNamed                 // named non-interface type; Name, PkgPath, Elem (underlying), Methods set
	KindFunc                  // func(...) ...; Params, Results, Variadic set
	KindChan                  // chan T; Elem, ChanDir set
	KindTypeParam             // *types.TypeParam not yet instantiated; Name set
	KindOpaque                // bridge cannot decompose; OpaqueReason, GoType set
	KindUntyped               // untyped constant (rare; emitter narrows to default type)
)

// String returns the Kind's short identifier. Used by Format and by
// diagnostic messages.
func (k Kind) String() string {
	switch k {
	case KindInvalid:
		return "invalid"
	case KindBool:
		return "bool"
	case KindInt:
		return "int"
	case KindUint:
		return "uint"
	case KindFloat:
		return "float"
	case KindString:
		return "string"
	case KindBytes:
		return "bytes"
	case KindList:
		return "list"
	case KindArray:
		return "array"
	case KindMap:
		return "map"
	case KindStruct:
		return "struct"
	case KindRef:
		return "ref"
	case KindIface:
		return "iface"
	case KindNamed:
		return "named"
	case KindFunc:
		return "func"
	case KindChan:
		return "chan"
	case KindTypeParam:
		return "typeparam"
	case KindOpaque:
		return "opaque"
	case KindUntyped:
		return "untyped"
	}
	return "?"
}

// Width is the bit width for KindInt, KindUint, KindFloat. Width 0
// means Go's machine-sized int / uint (no width tag). For KindFloat,
// Width is always 32 or 64.
type Width uint8

// ChanDir mirrors go/types.ChanDir but is stable across Go releases.
type ChanDir uint8

const (
	ChanInvalid ChanDir = iota
	ChanSend
	ChanRecv
	ChanBoth
)

// String returns the Go-source-level prefix for the channel direction.
// `chan T` for ChanBoth, `chan<- T` for ChanSend, `<-chan T` for
// ChanRecv. Empty for ChanInvalid.
func (d ChanDir) String() string {
	switch d {
	case ChanSend:
		return "chan<-"
	case ChanRecv:
		return "<-chan"
	case ChanBoth:
		return "chan"
	}
	return ""
}

// OpaqueReason names why the bridge produced KindOpaque. Every value
// is exercised by stdlib_test.go; no Type with Kind == KindOpaque
// may carry OpaqueNone.
type OpaqueReason uint8

const (
	OpaqueNone            OpaqueReason = iota
	OpaqueUnsafePointer                // unsafe.Pointer
	OpaqueUintptr                      // uintptr (not a regular integer for Mochi)
	OpaqueComplex                      // complex64 / complex128 (deferred)
	OpaqueUnexportedField              // struct shape contains unexported fields (informational; we still include them)
	OpaqueAnonInterface                // anonymous interface with methods we cannot give a stable name
	OpaqueRecursiveType                // self-referential type elided
	OpaqueTuple                        // *types.Tuple outside a function-result position
	OpaqueUnknown                      // catch-all; should be zero on stdlib soak
)

// String returns the constant's short name for use in IR dumps and
// diagnostics.
func (r OpaqueReason) String() string {
	switch r {
	case OpaqueNone:
		return "none"
	case OpaqueUnsafePointer:
		return "unsafe.Pointer"
	case OpaqueUintptr:
		return "uintptr"
	case OpaqueComplex:
		return "complex"
	case OpaqueUnexportedField:
		return "unexported-field"
	case OpaqueAnonInterface:
		return "anon-iface"
	case OpaqueRecursiveType:
		return "recursive"
	case OpaqueTuple:
		return "tuple"
	case OpaqueUnknown:
		return "unknown"
	}
	return "?"
}

// Field is one field of a KindStruct.
type Field struct {
	Name     string
	Type     Type
	Tag      string
	Embedded bool
	Exported bool
}

// Method is one method on a KindNamed or KindIface. Signature is
// always a KindFunc Type, with the receiver omitted.
type Method struct {
	Name      string
	Exported  bool
	Signature Type
}

// Type is the structural Mochi-side image of a go/types.Type. The
// zero value (Kind == KindInvalid) is never produced by GoToMochi
// and is a programming error if observed.
type Type struct {
	Kind         Kind
	Width        Width
	Elem         *Type        // KindList / KindArray / KindMap (value) / KindRef / KindChan / KindNamed (underlying)
	Key          *Type        // KindMap
	ArrayLen     int64        // KindArray
	Fields       []Field      // KindStruct
	Params       []Type       // KindFunc
	Results      []Type       // KindFunc
	Variadic     bool         // KindFunc
	Name         string       // KindNamed / KindIface (named) / KindTypeParam
	PkgPath      string       // KindNamed / KindIface (named)
	TypeArgs     []Type       // KindNamed / KindIface (instantiated generic): concrete type arguments
	Methods      []Method     // KindNamed / KindIface
	ChanDir      ChanDir      // KindChan
	OpaqueReason OpaqueReason // KindOpaque
	GoType       string       // KindOpaque (verbatim Go source); KindNamed (qualified types.TypeString)
}

// Format implements fmt.Formatter. %v prints MochiToGo(t); %+v adds
// the OpaqueReason for KindOpaque; %#v prints a Go-source-level
// literal of the Type struct for use in goldens.
func (t Type) Format(f fmt.State, verb rune) {
	switch verb {
	case 'v':
		if f.Flag('#') {
			fmt.Fprintf(f, "%#v", struct {
				Kind         Kind
				Width        Width
				Elem         *Type
				Key          *Type
				ArrayLen     int64
				Fields       []Field
				Params       []Type
				Results      []Type
				Variadic     bool
				Name         string
				PkgPath      string
				TypeArgs     []Type
				Methods      []Method
				ChanDir      ChanDir
				OpaqueReason OpaqueReason
				GoType       string
			}{t.Kind, t.Width, t.Elem, t.Key, t.ArrayLen, t.Fields, t.Params, t.Results, t.Variadic, t.Name, t.PkgPath, t.TypeArgs, t.Methods, t.ChanDir, t.OpaqueReason, t.GoType})
			return
		}
		io.WriteString(f, MochiToGo(t))
		if f.Flag('+') && t.Kind == KindOpaque {
			fmt.Fprintf(f, " /*opaque: %s*/", t.OpaqueReason)
		}
	case 's':
		io.WriteString(f, MochiToGo(t))
	default:
		io.WriteString(f, MochiToGo(t))
	}
}

// String returns MochiToGo(t).
func (t Type) String() string { return MochiToGo(t) }
