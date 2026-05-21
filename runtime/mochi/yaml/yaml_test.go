package yaml

import (
	"bytes"
	"strings"
	"testing"
)

func TestMarshalRoundTrip(t *testing.T) {
	in := map[string]any{"a": 1, "b": []int{2, 3}}
	b, err := Marshal(in)
	if err != nil {
		t.Fatal(err)
	}
	var out map[string]any
	if err := Unmarshal(b, &out); err != nil {
		t.Fatal(err)
	}
	if out["a"] != 1 {
		t.Errorf("a = %v", out["a"])
	}
}

func TestFprint(t *testing.T) {
	var buf bytes.Buffer
	if err := Fprint(&buf, map[string]int{"x": 1}); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(buf.String(), "x: 1") {
		t.Errorf("Fprint = %q", buf.String())
	}
}

func ExamplePrint() {
	Print(map[string]int{"a": 1})
	// Output:
	// a: 1
}
