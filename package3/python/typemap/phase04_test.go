package typemap

import (
	"testing"

	"mochi/package3/python/errors"
	"mochi/package3/python/stubs"
)

// TestPhase4TypeMapping is the umbrella sentinel for MEP-71 Phase 4. It walks
// the closed type table end-to-end: scalar / collection / union / Optional /
// Callable / class lowerings plus the refusal set.
func TestPhase4TypeMapping(t *testing.T) {
	t.Run("scalar_table", func(t *testing.T) {
		m := &Mapper{}
		cases := map[string]string{
			"int":      "int",
			"float":    "float",
			"bool":     "bool",
			"str":      "string",
			"bytes":    "bytes",
			"None":     "None",
			"NoneType": "None",
		}
		for src, want := range cases {
			d := m.Map(src)
			if !d.OK() {
				t.Errorf("scalar %q skipped: %v", src, d.Skip)
				continue
			}
			if got := d.Type.Render(); got != want {
				t.Errorf("scalar %q -> %q, want %q", src, got, want)
			}
		}
	})

	t.Run("collection_table", func(t *testing.T) {
		m := &Mapper{}
		cases := map[string]string{
			"list[int]":                 "list<int>",
			"List[int]":                 "list<int>",
			"set[int]":                  "set<int>",
			"frozenset[int]":            "set<int>",
			"dict[str, int]":            "map<string, int>",
			"Dict[str, int]":            "map<string, int>",
			"tuple[int, str]":           "tuple<int, string>",
			"tuple[int, ...]":           "list<int>",
			"Iterator[int]":             "list<int>",
			"AsyncIterator[int]":        "stream<int>",
			"Awaitable[int]":            "async int",
			"Coroutine[Any, Any, int]":  "async int",
			"collections.abc.Iterable[int]": "list<int>",
		}
		for src, want := range cases {
			d := m.Map(src)
			if !d.OK() {
				t.Errorf("collection %q skipped: %v", src, d.Skip)
				continue
			}
			if got := d.Type.Render(); got != want {
				t.Errorf("collection %q -> %q, want %q", src, got, want)
			}
		}
	})

	t.Run("union_and_optional", func(t *testing.T) {
		m := &Mapper{}
		cases := map[string]string{
			"Optional[int]":     "int?",
			"int | None":        "int?",
			"None | int":        "int?",
			"int | str":         "int | string",
			"int | str | None":  "int | string?",
			"Union[int, str]":   "int | string",
		}
		for src, want := range cases {
			d := m.Map(src)
			if !d.OK() {
				t.Errorf("union %q skipped: %v", src, d.Skip)
				continue
			}
			if got := d.Type.Render(); got != want {
				t.Errorf("union %q -> %q, want %q", src, got, want)
			}
		}
	})

	t.Run("callable", func(t *testing.T) {
		m := &Mapper{}
		d := m.Map("Callable[[int, str], bool]")
		if !d.OK() || d.Type.Render() != "fun(int, string): bool" {
			t.Errorf("got %+v", d)
		}
	})

	t.Run("refusal_set", func(t *testing.T) {
		m := &Mapper{}
		refusals := map[string]errors.SkipReason{
			"Any":                        errors.SkipUnsupportedTypingConstruct,
			"complex":                    errors.SkipNoComplexType,
			"object":                     errors.SkipUnsupportedTypingConstruct,
			"ParamSpec[P]":               errors.SkipParamSpec,
			"Callable[..., int]":         errors.SkipParamSpec,
			"Unpack[Ts]":                 errors.SkipTypeVarTuple,
			"Generator[int, None, None]": errors.SkipUnsupportedTypingConstruct,
			"Type[int]":                  errors.SkipUnsupportedTypingConstruct,
			"UnknownClass":               errors.SkipForwardRef,
			"Dict[float, int]":           errors.SkipUnsupportedTypingConstruct,
		}
		for src, want := range refusals {
			d := m.Map(src)
			if d.OK() {
				t.Errorf("%q: expected refusal", src)
				continue
			}
			if d.Skip.Reason != want {
				t.Errorf("%q: reason = %v, want %v", src, d.Skip.Reason, want)
			}
		}
	})

	t.Run("class_mappings", func(t *testing.T) {
		m := &Mapper{}
		// TypedDict
		td := stubs.ClassDecl{
			Name:        "Point",
			IsTypedDict: true,
			Fields: []stubs.FieldDecl{
				{Name: "x", Type: "int"},
				{Name: "y", Type: "int"},
			},
		}
		dd := m.MapClass(td)
		if !dd.OK() || dd.Type.Kind != KindRecord || len(dd.Type.Fields) != 2 {
			t.Errorf("TypedDict: %+v", dd)
		}
		// Frozen dataclass
		fd := stubs.ClassDecl{
			Name:        "Vec",
			IsDataclass: true,
			Decorators:  []string{"dataclass(frozen=True)"},
			Fields:      []stubs.FieldDecl{{Name: "v", Type: "list[int]"}},
		}
		dd = m.MapClass(fd)
		if !dd.OK() || dd.Type.Kind != KindRecord {
			t.Errorf("frozen dataclass: %+v", dd)
		}
		// Mutable dataclass refused
		mu := stubs.ClassDecl{Name: "Mut", IsDataclass: true, Decorators: []string{"dataclass"}}
		dd = m.MapClass(mu)
		if dd.OK() {
			t.Error("mutable dataclass should be refused")
		}
		// Protocol
		pr := stubs.ClassDecl{
			Name:       "Greeter",
			IsProtocol: true,
			Methods: []stubs.FuncDecl{
				{
					Name: "hello",
					Params: []stubs.ParamDecl{
						{Name: "self"},
						{Name: "who", Type: "str"},
					},
					ReturnType: "str",
				},
			},
		}
		dd = m.MapClass(pr)
		if !dd.OK() || dd.Type.Kind != KindInterface || len(dd.Type.Methods) != 1 {
			t.Errorf("Protocol: %+v", dd)
		}
	})

	t.Run("partial_allows_any", func(t *testing.T) {
		m := &Mapper{AllowPartial: true}
		d := m.Map("Any")
		if !d.OK() {
			t.Fatalf("AllowPartial should map Any: %v", d.Skip)
		}
	})

	t.Run("nested_round_trip", func(t *testing.T) {
		m := &Mapper{}
		d := m.Map("Dict[str, List[Optional[int]]]")
		if !d.OK() || d.Type.Render() != "map<string, list<int?>>" {
			t.Errorf("got %+v", d)
		}
	})
}
