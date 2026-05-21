package gogen

import (
	"strings"
	"testing"

	"mochi/compiler3/ir"
)

// TestEmitGoCallSealedRoundTrip asserts that when a GoBinding sets
// SealHandles, the emitter wraps every argument in ffi.Seal[T] and
// the return value in ffi.Unseal[T], and imports `mochi/runtime/mochi/ffi`.
func TestEmitGoCallSealedRoundTrip(t *testing.T) {
	fn := &ir.Function{Name: "shout", Result: ir.TypeStr}
	fn.GoBindings = []ir.GoBinding{{
		Pkg:         "strings",
		Alias:       "strings",
		Name:        "ToUpper",
		ArgTypes:    []string{"string"},
		Result:      "string",
		SealHandles: true,
	}}
	sID := fn.AddValue(ir.Value{Type: ir.TypeStr, Op: ir.OpParam})
	fn.Params = []uint32{sID}
	entryID := fn.AddBlock()
	ret := fn.AddValue(ir.Value{Type: ir.TypeStr, Op: ir.OpCallGo, Args: []uint32{sID}, Const: 0})
	entry := fn.Block(entryID)
	entry.Values = []uint32{ret}
	entry.Term = ir.Terminator{Kind: ir.TermReturn, Value: ret}

	src, err := Emit(&Program{PkgName: "main", Funcs: []*ir.Function{fn}})
	if err != nil {
		t.Fatalf("Emit: %v\n%s", err, src)
	}
	s := string(src)
	if !strings.Contains(s, "\"mochi/runtime/mochi/ffi\"") {
		t.Errorf("missing ffi import:\n%s", s)
	}
	if !strings.Contains(s, "ffi.Seal[string](") {
		t.Errorf("missing ffi.Seal wrap on arg:\n%s", s)
	}
	if !strings.Contains(s, "ffi.Unseal[string](strings.ToUpper(") {
		t.Errorf("missing ffi.Unseal wrap on return:\n%s", s)
	}
}

// TestEmitGoCallSealedConsumerBuilds is the Phase 10 gate: a Mochi
// program that crosses the FFI boundary with sealing on compiles and
// runs against the real runtime/mochi/ffi helpers, producing the
// expected output.
func TestEmitGoCallSealedConsumerBuilds(t *testing.T) {
	fn := &ir.Function{Name: "shout", Result: ir.TypeStr}
	fn.GoBindings = []ir.GoBinding{{
		Pkg:         "strings",
		Alias:       "strings",
		Name:        "ToUpper",
		ArgTypes:    []string{"string"},
		Result:      "string",
		SealHandles: true,
	}}
	sID := fn.AddValue(ir.Value{Type: ir.TypeStr, Op: ir.OpParam})
	fn.Params = []uint32{sID}
	entryID := fn.AddBlock()
	ret := fn.AddValue(ir.Value{Type: ir.TypeStr, Op: ir.OpCallGo, Args: []uint32{sID}, Const: 0})
	entry := fn.Block(entryID)
	entry.Values = []uint32{ret}
	entry.Term = ir.Terminator{Kind: ir.TermReturn, Value: ret}

	src, err := Emit(&Program{PkgName: "main", Funcs: []*ir.Function{fn}})
	if err != nil {
		t.Fatalf("Emit: %v\n%s", err, src)
	}
	withMain := string(src) + "\nfunc main() {\n\tprintln(shout(\"hello\"))\n}\n"
	if got := runGoModule(t, withMain); got != "HELLO\n" {
		t.Errorf("sealed shout(\"hello\") = %q, want HELLO", got)
	}
}

// TestEmitGoCallUnsealed asserts the default (SealHandles=false) path
// still produces the simple bare call site, no ffi import, no Seal
// wrappers. Guards against regressing the non-sealed corpus.
func TestEmitGoCallUnsealed(t *testing.T) {
	fn := ir.FixtureGoCallToUpper()
	// belt-and-braces: explicitly assert SealHandles is false.
	for i := range fn.GoBindings {
		fn.GoBindings[i].SealHandles = false
	}
	src, err := Emit(&Program{PkgName: "main", Funcs: []*ir.Function{fn}})
	if err != nil {
		t.Fatalf("Emit: %v\n%s", err, src)
	}
	s := string(src)
	if strings.Contains(s, "ffi.Seal") || strings.Contains(s, "ffi.Unseal") {
		t.Errorf("unsealed binding produced seal wrappers:\n%s", s)
	}
	if strings.Contains(s, "\"mochi/runtime/mochi/ffi\"") {
		t.Errorf("unsealed binding pulled ffi import:\n%s", s)
	}
}
