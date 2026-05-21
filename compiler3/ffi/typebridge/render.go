package typebridge

import (
	"fmt"
	"strconv"
	"strings"
)

// MochiToGo renders a Type as a Go source-level type expression. The
// result is used verbatim by the Phase 4 emitter; it must be
// syntactically valid Go.
//
// For non-KindOpaque inputs, round-trip identity holds:
// parsing MochiToGo(GoToMochi(t)) back through go/types yields a
// types.Type that is types.Identical to t. roundtrip_test.go
// exercises this property across a 60+ shape corpus.
func MochiToGo(t Type) string {
	var b strings.Builder
	writeMochiToGo(&b, t)
	return b.String()
}

func writeMochiToGo(b *strings.Builder, t Type) {
	switch t.Kind {
	case KindInvalid:
		b.WriteString("<invalid>")
	case KindBool:
		b.WriteString("bool")
	case KindInt:
		b.WriteString(intName("int", t.Width))
	case KindUint:
		b.WriteString(intName("uint", t.Width))
	case KindFloat:
		switch t.Width {
		case 32:
			b.WriteString("float32")
		case 64:
			b.WriteString("float64")
		default:
			b.WriteString("float64")
		}
	case KindString:
		b.WriteString("string")
	case KindBytes:
		b.WriteString("[]byte")
	case KindList:
		b.WriteString("[]")
		if t.Elem != nil {
			writeMochiToGo(b, *t.Elem)
		}
	case KindArray:
		b.WriteByte('[')
		b.WriteString(strconv.FormatInt(t.ArrayLen, 10))
		b.WriteByte(']')
		if t.Elem != nil {
			writeMochiToGo(b, *t.Elem)
		}
	case KindMap:
		b.WriteString("map[")
		if t.Key != nil {
			writeMochiToGo(b, *t.Key)
		}
		b.WriteByte(']')
		if t.Elem != nil {
			writeMochiToGo(b, *t.Elem)
		}
	case KindStruct:
		writeStruct(b, t.Fields)
	case KindNamed:
		b.WriteString(qualifiedName(t.PkgPath, t.Name))
		writeTypeArgs(b, t.TypeArgs)
	case KindIface:
		if t.Name != "" {
			b.WriteString(qualifiedName(t.PkgPath, t.Name))
			writeTypeArgs(b, t.TypeArgs)
			return
		}
		if len(t.Methods) == 0 {
			b.WriteString("any")
			return
		}
		b.WriteString("interface{")
		for i, m := range t.Methods {
			if i > 0 {
				b.WriteString("; ")
			}
			b.WriteString(m.Name)
			writeFuncBody(b, m.Signature)
		}
		b.WriteString("}")
	case KindRef:
		b.WriteByte('*')
		if t.Elem != nil {
			writeMochiToGo(b, *t.Elem)
		}
	case KindFunc:
		b.WriteString("func")
		writeFuncBody(b, t)
	case KindChan:
		b.WriteString(t.ChanDir.String())
		b.WriteByte(' ')
		if t.Elem != nil {
			writeMochiToGo(b, *t.Elem)
		}
	case KindTypeParam:
		b.WriteString(t.Name)
	case KindOpaque:
		b.WriteString(t.GoType)
	case KindUntyped:
		if t.GoType != "" {
			b.WriteString(t.GoType)
		} else {
			b.WriteString("any")
		}
	default:
		b.WriteString("<unknown>")
	}
}

func intName(prefix string, w Width) string {
	switch w {
	case 0:
		return prefix
	case 8, 16, 32, 64:
		return fmt.Sprintf("%s%d", prefix, w)
	}
	return prefix
}

func writeStruct(b *strings.Builder, fields []Field) {
	if len(fields) == 0 {
		b.WriteString("struct{}")
		return
	}
	b.WriteString("struct{")
	for i, f := range fields {
		if i > 0 {
			b.WriteString("; ")
		}
		if f.Embedded {
			writeMochiToGo(b, f.Type)
		} else {
			b.WriteString(f.Name)
			b.WriteByte(' ')
			writeMochiToGo(b, f.Type)
		}
		if f.Tag != "" {
			b.WriteByte(' ')
			b.WriteString(strconv.Quote(f.Tag))
		}
	}
	b.WriteString("}")
}

func writeFuncBody(b *strings.Builder, sig Type) {
	b.WriteByte('(')
	for i, p := range sig.Params {
		if i > 0 {
			b.WriteString(", ")
		}
		if sig.Variadic && i == len(sig.Params)-1 {
			b.WriteString("...")
			if p.Kind == KindList && p.Elem != nil {
				writeMochiToGo(b, *p.Elem)
			} else if p.Kind == KindBytes {
				b.WriteString("byte")
			} else {
				writeMochiToGo(b, p)
			}
			continue
		}
		writeMochiToGo(b, p)
	}
	b.WriteByte(')')
	switch len(sig.Results) {
	case 0:
		// nothing
	case 1:
		b.WriteByte(' ')
		writeMochiToGo(b, sig.Results[0])
	default:
		b.WriteString(" (")
		for i, r := range sig.Results {
			if i > 0 {
				b.WriteString(", ")
			}
			writeMochiToGo(b, r)
		}
		b.WriteByte(')')
	}
}

func writeTypeArgs(b *strings.Builder, args []Type) {
	if len(args) == 0 {
		return
	}
	b.WriteByte('[')
	for i, a := range args {
		if i > 0 {
			b.WriteString(", ")
		}
		writeMochiToGo(b, a)
	}
	b.WriteByte(']')
}

// qualifiedName renders pkg.Name using the last path component as
// the alias. This matches the default `types.RelativeTo(nil)`
// behaviour for Go source rendering.
func qualifiedName(pkgPath, name string) string {
	if pkgPath == "" {
		return name
	}
	pkg := pkgPath
	if i := strings.LastIndex(pkg, "/"); i >= 0 {
		pkg = pkg[i+1:]
	}
	return pkg + "." + name
}
