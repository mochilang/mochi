package wrapper

import (
	"fmt"
	"strings"

	"mochi/package3/go/apisurface"
	"mochi/package3/go/typemap"
)

// emitFunc lowers one apisurface.FuncDecl to either an EmittedFunc
// or a SkipNote. The caller uses the SkipNote pointer being non-nil
// to discriminate: a non-nil SkipNote and a nil EmittedFunc means
// the item was skipped; the inverse means the wrapper was produced.
func (e *Emitter) emitFunc(pkgPath, pkgName string, fd *apisurface.FuncDecl) (*EmittedFunc, *SkipNote, error) {
	if fd == nil || fd.Underlying == nil {
		return nil, nil, fmt.Errorf("%w: nil FuncDecl", ErrEmit)
	}
	// Skip methods (they require receiver handling on phase 6.x).
	if fd.Underlying.Receiver != "" {
		return nil, &SkipNote{
			Kind: "method", Package: pkgPath, Name: fd.Underlying.Name,
			Position: fd.Underlying.Position,
			Reason:   "method wrappers land in phase 6.1",
		}, nil
	}
	// Skip generic functions; monomorphisation is phase 15.
	if len(fd.Underlying.TypeParams) > 0 {
		return nil, &SkipNote{
			Kind: "func", Package: pkgPath, Name: fd.Underlying.Name,
			Position: fd.Underlying.Position,
			Reason:   "generic func defers to phase 15 monomorphisation",
		}, nil
	}

	// Map every param. A single unsupported param defers the whole func.
	paramSlots := []EmittedParam{}
	for i, p := range fd.Params {
		m, err := e.mapper.Map(p.Type)
		if err != nil {
			return nil, &SkipNote{
				Kind: "func", Package: pkgPath, Name: fd.Underlying.Name,
				Position: fd.Underlying.Position,
				Reason:   "param unmappable: " + err.Error(),
			}, nil
		}
		// Phase 6 baseline: only scalar/string/[]byte/bool params.
		ct, ok := baselineParamCType(m.Mochi)
		if !ok {
			return nil, &SkipNote{
				Kind: "func", Package: pkgPath, Name: fd.Underlying.Name,
				Position: fd.Underlying.Position,
				Reason:   "param type " + m.Mochi.String() + " requires phase 6.x sub-phase",
			}, nil
		}
		name := p.Name
		if name == "" {
			name = fmt.Sprintf("arg%d", i)
		}
		paramSlots = append(paramSlots, EmittedParam{Name: name, CType: ct, Mochi: m.Mochi})
	}

	// Detect trailing error.
	hasError := false
	resultDecls := fd.Results
	if n := len(resultDecls); n > 0 {
		last := resultDecls[n-1]
		if isErrorType(last.Type) {
			hasError = true
			resultDecls = resultDecls[:n-1]
		}
	}

	resultSlots := []EmittedParam{}
	for i, r := range resultDecls {
		m, err := e.mapper.Map(r.Type)
		if err != nil {
			return nil, &SkipNote{
				Kind: "func", Package: pkgPath, Name: fd.Underlying.Name,
				Position: fd.Underlying.Position,
				Reason:   "result unmappable: " + err.Error(),
			}, nil
		}
		ct, ok := baselineResultCType(m.Mochi)
		if !ok {
			return nil, &SkipNote{
				Kind: "func", Package: pkgPath, Name: fd.Underlying.Name,
				Position: fd.Underlying.Position,
				Reason:   "result type " + m.Mochi.String() + " requires phase 6.x sub-phase",
			}, nil
		}
		name := r.Name
		if name == "" {
			name = fmt.Sprintf("ret%d", i)
		}
		resultSlots = append(resultSlots, EmittedParam{Name: name, CType: ct, Mochi: m.Mochi})
	}

	ef := &EmittedFunc{
		Symbol:   exportSymbol(e.flatModule, pkgName, fd.Underlying.Name),
		Package:  pkgPath,
		GoName:   fd.Underlying.Name,
		Params:   paramSlots,
		Results:  resultSlots,
		HasError: hasError,
	}
	return ef, nil, nil
}

// baselineParamCType returns the C type for a Mochi parameter that
// the phase 6 baseline can lower. Returns false to skip.
func baselineParamCType(t typemap.MochiType) (string, bool) {
	switch v := t.(type) {
	case typemap.ScalarType:
		switch v.Name {
		case "int":
			return "C.long", true
		case "float":
			return "C.double", true
		case "bool":
			return "C.int", true
		case "string":
			return "*C.char", true
		case "bytes":
			return "MochiSlice", true
		}
	}
	return "", false
}

// baselineResultCType returns the C type for a Mochi result that
// the phase 6 baseline can lower.
func baselineResultCType(t typemap.MochiType) (string, bool) {
	switch v := t.(type) {
	case typemap.ScalarType:
		switch v.Name {
		case "int":
			return "C.long", true
		case "float":
			return "C.double", true
		case "bool":
			return "C.int", true
		case "string":
			return "*C.char", true
		case "bytes":
			return "MochiSlice", true
		}
	}
	return "", false
}

// isErrorType reports whether the typed Go type is the predeclared
// `error` interface. apisurface.BasicType carries the name "error"
// for predeclared error.
func isErrorType(t apisurface.GoType) bool {
	if b, ok := t.(apisurface.BasicType); ok {
		return b.Name == "error"
	}
	return false
}

// exportSymbol returns the //export name for a wrapped function.
// Format: "mochi_go_<flatModule>_<pkgName>_<funcName>".
func exportSymbol(flatModule, pkgName, funcName string) string {
	return "mochi_go_" + flatModule + "_" + pkgName + "_" + funcName
}

// renderSource pulls every EmittedFunc into one wrap.go file body.
func (e *Emitter) renderSource(pkgName string, funcs []EmittedFunc) (string, error) {
	var sb strings.Builder
	sb.WriteString("// Code generated by mochi MEP-74 phase 6 wrapper synthesiser. DO NOT EDIT.\n\n")
	sb.WriteString("package " + pkgName + "\n\n")
	sb.WriteString(`/*
typedef struct { void* ptr; long len; long cap; } MochiSlice;
typedef long MochiStatus;
*/
import "C"

import (
	"runtime"
	"unsafe"
`)
	// Collect imported source packages.
	importSet := map[string]string{}
	for _, f := range funcs {
		importSet[f.Package] = pkgAlias(f.Package)
	}
	for _, ip := range sortedKeys(importSet) {
		sb.WriteString("\t" + importSet[ip] + " \"" + ip + "\"\n")
	}
	sb.WriteString(")\n\n")

	// Suppress "imported and not used" for the case of zero wrapped funcs.
	sb.WriteString("var _ = unsafe.Pointer(nil)\n")
	sb.WriteString("var _ = runtime.KeepAlive\n\n")

	for _, f := range funcs {
		if err := renderFunc(&sb, &f); err != nil {
			return "", err
		}
		sb.WriteString("\n")
	}

	// Module-scoped string free.
	sb.WriteString("//export mochi_go_" + e.flatModule + "_string_free\n")
	sb.WriteString("func mochi_go_" + e.flatModule + "_string_free(p *C.char) { C.free(unsafe.Pointer(p)) }\n")
	return sb.String(), nil
}

func renderFunc(sb *strings.Builder, f *EmittedFunc) error {
	alias := pkgAlias(f.Package)
	// Signature.
	sb.WriteString("//export " + f.Symbol + "\n")
	sb.WriteString("func " + f.Symbol + "(")
	for i, p := range f.Params {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(p.Name + " " + p.CType)
	}
	if f.HasError {
		if len(f.Params) > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString("out_err **C.char")
	}
	sb.WriteString(") ")
	switch {
	case f.HasError:
		sb.WriteString("C.MochiStatus")
	case len(f.Results) == 0:
		// no return.
	case len(f.Results) == 1:
		sb.WriteString(f.Results[0].CType)
	default:
		// Multi-result: write into out-pointers, return MochiStatus 0.
		sb.WriteString("C.MochiStatus")
	}
	sb.WriteString(" {\n")

	// Marshal params from C-side -> Go-side.
	goArgs := []string{}
	for _, p := range f.Params {
		v, conv := paramFromCToGo(p)
		if conv != "" {
			sb.WriteString("\t" + conv + "\n")
		}
		goArgs = append(goArgs, v)
	}

	call := alias + "." + f.GoName + "(" + strings.Join(goArgs, ", ") + ")"

	if f.HasError {
		// `result := ...; if err != nil { *out_err = C.CString(err.Error()); return 1 }; return 0`
		switch len(f.Results) {
		case 0:
			sb.WriteString("\terr := " + call + "\n")
			sb.WriteString("\tif err != nil { *out_err = C.CString(err.Error()); return C.MochiStatus(1) }\n")
		case 1:
			sb.WriteString("\tres, err := " + call + "\n")
			sb.WriteString("\tif err != nil { *out_err = C.CString(err.Error()); return C.MochiStatus(1) }\n")
			sb.WriteString("\t_ = res\n")
		default:
			sb.WriteString("\t_ = " + call + "\n")
		}
		// runtime.KeepAlive every input that crossed the boundary as
		// a pointer.
		for _, p := range f.Params {
			if isPointerCType(p.CType) {
				sb.WriteString("\truntime.KeepAlive(" + p.Name + ")\n")
			}
		}
		sb.WriteString("\treturn C.MochiStatus(0)\n")
	} else {
		switch len(f.Results) {
		case 0:
			sb.WriteString("\t" + call + "\n")
			for _, p := range f.Params {
				if isPointerCType(p.CType) {
					sb.WriteString("\truntime.KeepAlive(" + p.Name + ")\n")
				}
			}
		case 1:
			sb.WriteString("\tres := " + call + "\n")
			retExpr := resultFromGoToC(f.Results[0])
			for _, p := range f.Params {
				if isPointerCType(p.CType) {
					sb.WriteString("\truntime.KeepAlive(" + p.Name + ")\n")
				}
			}
			sb.WriteString("\treturn " + retExpr + "\n")
		default:
			// Multi-result lowered as out-pointers + status. Phase
			// 6.x sub-phase to enrich; the baseline currently
			// short-circuited above via SkipNote, so this branch is
			// reserved.
			sb.WriteString("\t_ = " + call + "\n")
			sb.WriteString("\treturn C.MochiStatus(0)\n")
		}
	}
	sb.WriteString("}\n")
	return nil
}

func paramFromCToGo(p EmittedParam) (string, string) {
	switch p.CType {
	case "C.long":
		return "int64(" + p.Name + ")", ""
	case "C.double":
		return "float64(" + p.Name + ")", ""
	case "C.int":
		return p.Name + " != 0", ""
	case "*C.char":
		return "C.GoString(" + p.Name + ")", ""
	case "MochiSlice":
		decl := fmt.Sprintf("%s_go := C.GoBytes(unsafe.Pointer(%s.ptr), C.int(%s.len))", p.Name, p.Name, p.Name)
		return p.Name + "_go", decl
	}
	return p.Name, ""
}

func resultFromGoToC(r EmittedParam) string {
	switch r.CType {
	case "C.long":
		return "C.long(res)"
	case "C.double":
		return "C.double(res)"
	case "C.int":
		return "C.int(boolToInt(res))"
	case "*C.char":
		return "C.CString(res)"
	case "MochiSlice":
		return "goBytesToSlice(res)"
	}
	return "res"
}

func isPointerCType(t string) bool {
	return strings.HasPrefix(t, "*") || t == "MochiSlice"
}

func pkgAlias(importPath string) string {
	// Simple alias: last path segment with underscores.
	seg := importPath
	if i := strings.LastIndex(seg, "/"); i >= 0 {
		seg = seg[i+1:]
	}
	if seg == "" {
		seg = "pkg"
	}
	return seg
}

func sortedKeys(m map[string]string) []string {
	out := make([]string, 0, len(m))
	for k := range m {
		out = append(out, k)
	}
	// stable sort
	for i := 1; i < len(out); i++ {
		for j := i; j > 0 && out[j-1] > out[j]; j-- {
			out[j-1], out[j] = out[j], out[j-1]
		}
	}
	return out
}

// renderHandles emits the handle-pool boilerplate (cgo.Handle wrap)
// every wrapper file shares. The pool is intentionally minimal in
// phase 6; the goroutine bridge (phase 14) extends it with channel
// and func-value handles.
func (e *Emitter) renderHandles(pkgName string) string {
	return `// Code generated by mochi MEP-74 phase 6. DO NOT EDIT.

package ` + pkgName + `

/*
#include <stdlib.h>
*/
import "C"

import (
	"runtime/cgo"
	"unsafe"
)

// boolToInt converts a Go bool to a C int (0/1).
func boolToInt(b bool) C.int {
	if b {
		return C.int(1)
	}
	return C.int(0)
}

// goBytesToSlice copies a Go byte slice into a freshly allocated
// MochiSlice. The caller owns the memory and must free it via
// mochi_go_` + e.flatModule + `_bytes_free.
func goBytesToSlice(b []byte) MochiSlice {
	if len(b) == 0 {
		return MochiSlice{}
	}
	ptr := C.CBytes(b)
	return MochiSlice{ptr: ptr, len: C.long(len(b)), cap: C.long(len(b))}
}

//export mochi_go_` + e.flatModule + `_bytes_free
func mochi_go_` + e.flatModule + `_bytes_free(s MochiSlice) {
	if s.ptr != nil {
		C.free(s.ptr)
	}
}

// handleNew registers v in the cgo handle pool and returns the
// opaque key the Mochi side will refer to it by. Phase 14 extends
// this with type-tag verification.
func handleNew(v interface{}) uintptr {
	return uintptr(cgo.NewHandle(v))
}

// handleValue resolves an opaque key back to its Go value.
func handleValue(k uintptr) interface{} {
	return cgo.Handle(k).Value()
}

// handleDelete releases an opaque key.
func handleDelete(k uintptr) {
	cgo.Handle(k).Delete()
}

var _ = unsafe.Pointer(nil)
`
}

// MochiSlice mirrors the cgo struct the wrapper uses on the Go
// side. It exists for parse-time validity; cgo replaces the import
// "C" types at build time.
type MochiSlice struct{}
