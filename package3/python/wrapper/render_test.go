package wrapper

import (
	"strings"
	"testing"

	"mochi/package3/python/typemap"
)

func TestRenderPyAnnoScalars(t *testing.T) {
	cases := map[string]string{
		"int":    "int",
		"float":  "float",
		"bool":   "bool",
		"string": "str",
		"bytes":  "bytes",
		"None":   "None",
	}
	for name, want := range cases {
		got := renderPyAnno(typemap.NewScalar(name))
		if got != want {
			t.Errorf("%s -> %s, want %s", name, got, want)
		}
	}
}

func TestRenderPyAnnoCollections(t *testing.T) {
	cases := []struct {
		in   typemap.MochiType
		want string
	}{
		{typemap.NewList(typemap.NewScalar("int")), "list[int]"},
		{typemap.NewSet(typemap.NewScalar("int")), "set[int]"},
		{typemap.NewMap(typemap.NewScalar("string"), typemap.NewScalar("int")), "dict[str, int]"},
		{typemap.NewTuple(typemap.NewScalar("int"), typemap.NewScalar("string")), "Tuple[int, str]"},
		{typemap.NewOptional(typemap.NewScalar("int")), "Optional[int]"},
		{typemap.NewAsync(typemap.NewScalar("int")), "Awaitable[int]"},
		{typemap.NewStream(typemap.NewScalar("int")), "AsyncIterator[int]"},
	}
	for _, c := range cases {
		got := renderPyAnno(c.in)
		if got != c.want {
			t.Errorf("%+v -> %s, want %s", c.in, got, c.want)
		}
	}
}

func TestRenderPyAnnoSumAndFun(t *testing.T) {
	su := typemap.NewSum(typemap.NewScalar("int"), typemap.NewScalar("string"))
	if got := renderPyAnno(su); got != "Union[int, str]" {
		t.Errorf("sum: %s", got)
	}
	fn := typemap.NewFun(typemap.NewScalar("bool"), typemap.NewScalar("int"), typemap.NewScalar("string"))
	if got := renderPyAnno(fn); got != "Callable[[int, str], bool]" {
		t.Errorf("fun: %s", got)
	}
}

func TestRenderPyAnnoNested(t *testing.T) {
	t1 := typemap.NewMap(typemap.NewScalar("string"),
		typemap.NewList(typemap.NewOptional(typemap.NewScalar("int"))))
	want := "dict[str, list[Optional[int]]]"
	if got := renderPyAnno(t1); got != want {
		t.Errorf("nested: %s, want %s", got, want)
	}
}

func TestRenderPyAnnoRefFallback(t *testing.T) {
	if got := renderPyAnno(typemap.NewRef("Greeter")); got != "Greeter" {
		t.Errorf("ref: %s", got)
	}
	if got := renderPyAnno(typemap.NewTypeVar("T")); got != "T" {
		t.Errorf("typevar: %s", got)
	}
}

func TestRenderPyAnnoUnknownFallsBackToAny(t *testing.T) {
	if got := renderPyAnno(typemap.MochiType{Kind: typemap.KindUnknown}); got != "Any" {
		t.Errorf("unknown -> %s, want Any", got)
	}
}

func TestParamSignature(t *testing.T) {
	if got := paramSignature(nil); got != "" {
		t.Errorf("empty -> %q", got)
	}
	got := paramSignature([]typemap.MochiType{
		typemap.NewScalar("int"),
		typemap.NewScalar("string"),
		typemap.NewScalar("bool"),
	})
	if got != "arg0, arg1, arg2" {
		t.Errorf("got %q", got)
	}
}

func TestLastDot(t *testing.T) {
	cases := map[string]string{
		"foo":             "foo",
		"foo.bar":         "bar",
		"foo.bar.baz":     "baz",
		"":                "",
		"a.b.c.d":         "d",
	}
	for in, want := range cases {
		if got := lastDot(in); got != want {
			t.Errorf("lastDot(%q) = %q, want %q", in, got, want)
		}
	}
}

func TestIsPrivate(t *testing.T) {
	cases := map[string]bool{
		"foo":      false,
		"_foo":     true,
		"__foo":    true,
		"__init__": false, // dunders not private at this layer
		"__call__": false,
		"FOO":      false,
		"":         false,
	}
	for in, want := range cases {
		if got := isPrivate(in); got != want {
			t.Errorf("isPrivate(%q) = %v, want %v", in, got, want)
		}
	}
}

func TestIsValidPyIdent(t *testing.T) {
	good := []string{"foo", "_foo", "Foo", "foo_bar", "foo2", "abc_123"}
	bad := []string{"", "1foo", "-foo", "foo-bar", "foo bar", "foo.bar", "@foo"}
	for _, g := range good {
		if !isValidPyIdent(g) {
			t.Errorf("isValidPyIdent(%q) = false, want true", g)
		}
	}
	for _, b := range bad {
		if isValidPyIdent(b) {
			t.Errorf("isValidPyIdent(%q) = true, want false", b)
		}
	}
}

func TestWrapperHelpersAlwaysIncludesToMochiDict(t *testing.T) {
	w := &Wrapper{}
	hs := wrapperHelpers(w)
	found := false
	for _, h := range hs {
		if h == "_to_mochi_dict" {
			found = true
		}
	}
	if !found {
		t.Errorf("_to_mochi_dict missing: %v", hs)
	}
}

func TestWrapperHelpersAsyncAddsRunAsync(t *testing.T) {
	w := &Wrapper{Items: []Item{{Kind: ItemFunc, IsAsync: true}}, Loop: EventLoopPerCall}
	hs := wrapperHelpers(w)
	found := false
	for _, h := range hs {
		if h == "_run_async" {
			found = true
		}
	}
	if !found {
		t.Errorf("_run_async missing: %v", hs)
	}
}

func TestWrapperHelpersPersistentLoopAddsPersistent(t *testing.T) {
	w := &Wrapper{Items: []Item{{Kind: ItemFunc, IsAsync: true}}, Loop: EventLoopPersistent}
	hs := wrapperHelpers(w)
	found := false
	for _, h := range hs {
		if h == "_persistent_loop" {
			found = true
		}
	}
	if !found {
		t.Errorf("_persistent_loop missing for persistent loop: %v", hs)
	}
}

func TestEventLoopModeString(t *testing.T) {
	if EventLoopPerCall.String() != "per-call" {
		t.Errorf("per-call string: %s", EventLoopPerCall)
	}
	if EventLoopPersistent.String() != "persistent" {
		t.Errorf("persistent string: %s", EventLoopPersistent)
	}
	if EventLoopMode(999).String() != "unknown" {
		t.Errorf("unknown string: %s", EventLoopMode(999))
	}
}

func TestItemKindString(t *testing.T) {
	cases := map[ItemKind]string{
		ItemFunc:      "fun",
		ItemRecord:    "record",
		ItemInterface: "interface",
		ItemConstant:  "const",
		ItemKind(999): "unknown",
	}
	for k, want := range cases {
		if k.String() != want {
			t.Errorf("%d.String() = %s, want %s", k, k.String(), want)
		}
	}
}

func TestRenderPyConsistentHeaders(t *testing.T) {
	w := &Wrapper{Package: "abc", Module: "abc_externs"}
	out := renderPy(w)
	for _, head := range []string{
		"# Auto-generated by mochi pkg lock",
		"# MEP-71 wrapper for source package: abc.",
		"from __future__ import annotations",
		"import abc as _src",
		"from ._mochi_wrap import",
	} {
		if !strings.Contains(out, head) {
			t.Errorf("missing header %q in:\n%s", head, out)
		}
	}
}

func TestRenderPYIConsistentHeaders(t *testing.T) {
	w := &Wrapper{Package: "abc", Module: "abc_externs"}
	out := renderPYI(w)
	for _, head := range []string{
		"# MEP-71 wrapper stub for source package: abc.",
		"from __future__ import annotations",
		"from typing import",
	} {
		if !strings.Contains(out, head) {
			t.Errorf("missing header %q in:\n%s", head, out)
		}
	}
}
