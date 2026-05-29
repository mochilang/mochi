package typemap_test

import (
	"testing"

	"mochi/package3/java/typemap"
)

func TestParseGenericSignature_Raw(t *testing.T) {
	sig, ok := typemap.ParseGenericSignature("java.lang.String")
	if !ok {
		t.Fatal("expected ok for raw type")
	}
	if sig.Base != "java.lang.String" {
		t.Errorf("Base = %q; want java.lang.String", sig.Base)
	}
	if !sig.IsRaw() {
		t.Error("IsRaw() should be true for non-generic type")
	}
}

func TestParseGenericSignature_SingleParam(t *testing.T) {
	sig, ok := typemap.ParseGenericSignature("java.util.List<java.lang.String>")
	if !ok {
		t.Fatal("expected ok")
	}
	if sig.Base != "java.util.List" {
		t.Errorf("Base = %q; want java.util.List", sig.Base)
	}
	if sig.Arity() != 1 {
		t.Errorf("Arity = %d; want 1", sig.Arity())
	}
	if sig.Params[0] != "java.lang.String" {
		t.Errorf("Params[0] = %q; want java.lang.String", sig.Params[0])
	}
}

func TestParseGenericSignature_TwoParams(t *testing.T) {
	sig, ok := typemap.ParseGenericSignature("java.util.Map<java.lang.String,java.lang.Integer>")
	if !ok {
		t.Fatal("expected ok")
	}
	if sig.Arity() != 2 {
		t.Errorf("Arity = %d; want 2", sig.Arity())
	}
}

func TestParseGenericSignature_Nested(t *testing.T) {
	sig, ok := typemap.ParseGenericSignature("java.util.List<java.util.List<java.lang.String>>")
	if !ok {
		t.Fatal("expected ok for nested generic")
	}
	if sig.Arity() != 1 {
		t.Errorf("Arity = %d; want 1", sig.Arity())
	}
	if sig.Params[0] != "java.util.List<java.lang.String>" {
		t.Errorf("Params[0] = %q", sig.Params[0])
	}
}

func TestParseGenericSignature_Malformed(t *testing.T) {
	_, ok := typemap.ParseGenericSignature("java.util.List<incomplete")
	if ok {
		t.Error("expected !ok for malformed generic")
	}
}

func TestGenericSignature_String(t *testing.T) {
	sig, _ := typemap.ParseGenericSignature("java.util.List<java.lang.String>")
	if got := sig.String(); got != "java.util.List<java.lang.String>" {
		t.Errorf("String() = %q; want java.util.List<java.lang.String>", got)
	}
}

func TestGenericSignature_EraseParams(t *testing.T) {
	sig, _ := typemap.ParseGenericSignature("java.util.List<java.lang.String>")
	erased := sig.EraseParams()
	if !erased.IsRaw() {
		t.Error("EraseParams() result should be raw")
	}
	if erased.Base != "java.util.List" {
		t.Errorf("erased.Base = %q; want java.util.List", erased.Base)
	}
}

func TestGenericSignature_WildcardErased(t *testing.T) {
	sig, _ := typemap.ParseGenericSignature("java.util.List<?>")
	if !sig.WildcardErased() {
		t.Error("WildcardErased should be true for List<?>")
	}
	sig2, _ := typemap.ParseGenericSignature("java.util.List<java.lang.String>")
	if sig2.WildcardErased() {
		t.Error("WildcardErased should be false for List<String>")
	}
}
