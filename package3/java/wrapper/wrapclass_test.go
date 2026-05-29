package wrapper

import (
	"testing"
)

func TestClassNameToIdent(t *testing.T) {
	cases := [][2]string{
		{"com.google.guava.Optional", "com_google_guava_Optional"},
		{"com.example.Outer$Inner", "com_example_Outer_Inner"},
		{"java.lang.String", "java_lang_String"},
	}
	for _, c := range cases {
		if got := classNameToIdent(c[0]); got != c[1] {
			t.Errorf("classNameToIdent(%q) = %q; want %q", c[0], got, c[1])
		}
	}
}

func TestWrapMethodName(t *testing.T) {
	got := wrapMethodName("com.example.Foo", "bar", false)
	want := "MochiWrap_com_example_Foo_bar"
	if got != want {
		t.Errorf("wrapMethodName instance = %q; want %q", got, want)
	}

	got2 := wrapMethodName("com.example.Foo", "bar", true)
	want2 := "MochiWrap_com_example_Foo_static_bar"
	if got2 != want2 {
		t.Errorf("wrapMethodName static = %q; want %q", got2, want2)
	}
}
