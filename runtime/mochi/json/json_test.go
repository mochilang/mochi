package json

import (
	"bytes"
	"fmt"
	"testing"
)

func TestMarshalCompact(t *testing.T) {
	b, err := Marshal(map[string]int{"b": 2, "a": 1})
	if err != nil {
		t.Fatal(err)
	}
	got := string(b)
	want := `{"a":1,"b":2}`
	if got != want {
		t.Errorf("Marshal = %s want %s", got, want)
	}
}

func TestMarshalIndentSortedKeys(t *testing.T) {
	b, err := MarshalIndent(map[string]any{"b": 2, "a": []int{1, 2}})
	if err != nil {
		t.Fatal(err)
	}
	got := string(b)
	want := "{\n  \"a\": [\n    1,\n    2\n  ],\n  \"b\": 2\n}"
	if got != want {
		t.Errorf("MarshalIndent =\n%s\nwant\n%s", got, want)
	}
}

func TestFprintTrailingNewline(t *testing.T) {
	var buf bytes.Buffer
	if err := Fprint(&buf, map[string]int{"a": 1}); err != nil {
		t.Fatal(err)
	}
	got := buf.String()
	if got == "" || got[len(got)-1] != '\n' {
		t.Errorf("Fprint not newline-terminated: %q", got)
	}
}

func TestUnmarshal(t *testing.T) {
	var m map[string]int
	if err := Unmarshal([]byte(`{"a":1}`), &m); err != nil {
		t.Fatal(err)
	}
	if m["a"] != 1 {
		t.Errorf("Unmarshal = %v", m)
	}
}

func ExamplePrint() {
	Print(map[string]int{"a": 1, "b": 2})
	// Output:
	// {
	//   "a": 1,
	//   "b": 2
	// }
}

// TestNoHTMLEscape confirms ampersands and angle brackets pass
// through unescaped (Mochi's `json` does not produce & etc.).
func TestNoHTMLEscape(t *testing.T) {
	b, err := MarshalIndent(map[string]string{"q": "a & b < c"})
	if err != nil {
		t.Fatal(err)
	}
	if !contains(b, "a & b < c") {
		t.Errorf("HTML-escaped output: %s", b)
	}
}

func contains(b []byte, sub string) bool {
	return string(b) != "" && bytes.Contains(b, []byte(sub))
}

func ExampleMarshalIndent() {
	b, _ := MarshalIndent([]int{1, 2, 3})
	fmt.Println(string(b))
	// Output:
	// [
	//   1,
	//   2,
	//   3
	// ]
}
