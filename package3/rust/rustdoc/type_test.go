package rustdoc

import (
	"encoding/json"
	"testing"
)

func TestTypeResolvedPath(t *testing.T) {
	var got Type
	if err := json.Unmarshal([]byte(`{"resolved_path": {"id":"0:0:1","path":"std::vec::Vec"}}`), &got); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if got.Kind() != "resolved_path" {
		t.Errorf("Kind = %q", got.Kind())
	}
	if got.ResolvedPath == nil || got.ResolvedPath.Path != "std::vec::Vec" {
		t.Errorf("ResolvedPath = %+v", got.ResolvedPath)
	}
}

func TestTypePrimitive(t *testing.T) {
	var got Type
	if err := json.Unmarshal([]byte(`{"primitive": "u8"}`), &got); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if got.Kind() != "primitive" {
		t.Errorf("Kind = %q", got.Kind())
	}
	if got.Primitive != "u8" {
		t.Errorf("Primitive = %q", got.Primitive)
	}
}

func TestTypeGeneric(t *testing.T) {
	var got Type
	if err := json.Unmarshal([]byte(`{"generic": "T"}`), &got); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if got.Kind() != "generic" || got.Generic != "T" {
		t.Errorf("got = %+v", got)
	}
}

func TestTypeTuple(t *testing.T) {
	var got Type
	if err := json.Unmarshal([]byte(`{"tuple": [{"primitive":"i32"},{"primitive":"f64"}]}`), &got); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if got.Kind() != "tuple" || len(got.Tuple) != 2 {
		t.Errorf("got = %+v", got)
	}
}

func TestTypeSlice(t *testing.T) {
	var got Type
	if err := json.Unmarshal([]byte(`{"slice": {"primitive": "u8"}}`), &got); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if got.Kind() != "slice" || got.Slice == nil || got.Slice.Primitive != "u8" {
		t.Errorf("got = %+v", got)
	}
}

func TestTypeArray(t *testing.T) {
	var got Type
	if err := json.Unmarshal([]byte(`{"array": {"type": {"primitive":"u8"}, "len": "32"}}`), &got); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if got.Kind() != "array" || got.Array == nil || got.Array.Len != "32" {
		t.Errorf("got = %+v", got)
	}
}

func TestTypeBorrowedRef(t *testing.T) {
	var got Type
	if err := json.Unmarshal([]byte(`{"borrowed_ref": {"lifetime": "'a", "is_mutable": false, "type": {"primitive":"str"}}}`), &got); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if got.Kind() != "borrowed_ref" {
		t.Errorf("Kind = %q", got.Kind())
	}
	if got.BorrowedRef.Lifetime != "'a" {
		t.Errorf("Lifetime = %q", got.BorrowedRef.Lifetime)
	}
}

func TestTypeRawPointer(t *testing.T) {
	var got Type
	if err := json.Unmarshal([]byte(`{"raw_pointer": {"is_mutable": true, "type": {"primitive":"u8"}}}`), &got); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if got.Kind() != "raw_pointer" || !got.RawPointer.IsMutable {
		t.Errorf("got = %+v", got)
	}
}

func TestTypeDynTrait(t *testing.T) {
	var got Type
	in := `{"dyn_trait": {"traits": [{"trait": {"id":"0:0:1","path":"core::fmt::Debug"}}], "lifetime": "'static"}}`
	if err := json.Unmarshal([]byte(in), &got); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if got.Kind() != "dyn_trait" || got.DynTrait.Lifetime != "'static" {
		t.Errorf("got = %+v", got)
	}
}

func TestTypeImplTrait(t *testing.T) {
	var got Type
	in := `{"impl_trait": [{"trait_bound": {"trait": {"id":"0:0:1","path":"core::iter::Iterator"}}}]}`
	if err := json.Unmarshal([]byte(in), &got); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if got.Kind() != "impl_trait" || len(got.ImplTrait) != 1 {
		t.Errorf("got = %+v", got)
	}
}

func TestTypeUnknownVariant(t *testing.T) {
	var got Type
	in := `{"hypothetical_future_variant_42": {"foo": "bar"}}`
	if err := json.Unmarshal([]byte(in), &got); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if got.Kind() != "unknown:hypothetical_future_variant_42" {
		t.Errorf("Kind = %q; want unknown:hypothetical_future_variant_42", got.Kind())
	}
}

func TestItemEnumUnknown(t *testing.T) {
	var got ItemEnum
	in := `{"future_item_kind": {"foo": "bar"}}`
	if err := json.Unmarshal([]byte(in), &got); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if got.Kind() != "unknown:future_item_kind" {
		t.Errorf("Kind = %q; want unknown:future_item_kind", got.Kind())
	}
}

func TestFunctionInputUnmarshal(t *testing.T) {
	var got FunctionInput
	if err := json.Unmarshal([]byte(`["bytes", {"primitive": "u8"}]`), &got); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if got.Name != "bytes" || got.Type.Primitive != "u8" {
		t.Errorf("got = %+v", got)
	}
}

func TestFunctionInputBadShape(t *testing.T) {
	var got FunctionInput
	if err := json.Unmarshal([]byte(`{"name":"x","type":{"primitive":"u8"}}`), &got); err == nil {
		t.Errorf("Unmarshal(object) err = nil; want error (input must be 2-tuple)")
	}
}

func TestABIBareString(t *testing.T) {
	var got ABI
	if err := json.Unmarshal([]byte(`"C"`), &got); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if got.Name != "C" || got.Unwind {
		t.Errorf("got = %+v", got)
	}
}

func TestABITaggedObject(t *testing.T) {
	var got ABI
	if err := json.Unmarshal([]byte(`{"C": {"unwind": true}}`), &got); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if got.Name != "C" || !got.Unwind {
		t.Errorf("got = %+v", got)
	}
}
