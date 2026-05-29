package reflect_test

import (
	"testing"

	javareflect "mochi/package3/java/reflect"
)

func TestEraseType_Raw(t *testing.T) {
	if got := javareflect.EraseType("java.lang.String"); got != "java.lang.String" {
		t.Errorf("EraseType(%q) = %q; want same", "java.lang.String", got)
	}
}

func TestEraseType_Generic(t *testing.T) {
	cases := []struct {
		in   string
		want string
	}{
		{"java.util.List<java.lang.String>", "java.util.List"},
		{"java.util.Map<K,V>", "java.util.Map"},
		{"java.util.concurrent.CompletableFuture<java.lang.Integer>", "java.util.concurrent.CompletableFuture"},
	}
	for _, c := range cases {
		got := javareflect.EraseType(c.in)
		if got != c.want {
			t.Errorf("EraseType(%q) = %q; want %q", c.in, got, c.want)
		}
	}
}

func TestIsErased(t *testing.T) {
	if !javareflect.IsErased("java.lang.String") {
		t.Error("IsErased should be true for raw type")
	}
	if javareflect.IsErased("java.util.List<java.lang.String>") {
		t.Error("IsErased should be false for generic type")
	}
}

func TestErasedParams(t *testing.T) {
	params := javareflect.ErasedParams("java.util.Map<java.lang.String,java.lang.Integer>")
	if len(params) != 2 {
		t.Fatalf("want 2 params; got %d: %v", len(params), params)
	}
	if params[0] != "java.lang.String" {
		t.Errorf("params[0] = %q; want java.lang.String", params[0])
	}
	if params[1] != "java.lang.Integer" {
		t.Errorf("params[1] = %q; want java.lang.Integer", params[1])
	}
}

func TestErasedParams_Raw(t *testing.T) {
	if p := javareflect.ErasedParams("java.lang.String"); p != nil {
		t.Errorf("ErasedParams of raw type should be nil; got %v", p)
	}
}

func TestNormaliseTypeName(t *testing.T) {
	cases := []struct {
		in   string
		want string
	}{
		{"java.util.Map$Entry", "java.util.Map.Entry"},
		{"int[]", "int"},
		{"  java.lang.String  ", "java.lang.String"},
		{"java.lang.String[]", "java.lang.String"},
	}
	for _, c := range cases {
		got := javareflect.NormaliseTypeName(c.in)
		if got != c.want {
			t.Errorf("NormaliseTypeName(%q) = %q; want %q", c.in, got, c.want)
		}
	}
}
