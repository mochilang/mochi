package fmt

import (
	"bytes"
	"testing"
)

func TestFprintlnJoinsWithSpace(t *testing.T) {
	var buf bytes.Buffer
	if err := Fprintln(&buf, "hello", 42, true); err != nil {
		t.Fatal(err)
	}
	if buf.String() != "hello 42 true\n" {
		t.Errorf("Fprintln = %q", buf.String())
	}
}

func TestSprint(t *testing.T) {
	got := Sprint("a", "b", "c")
	if got != "a b c" {
		t.Errorf("Sprint = %q", got)
	}
}

func TestFormatNil(t *testing.T) {
	if got := Format(nil); got != "nil" {
		t.Errorf("Format(nil) = %q", got)
	}
}

func TestEmpty(t *testing.T) {
	var buf bytes.Buffer
	if err := Fprintln(&buf); err != nil {
		t.Fatal(err)
	}
	if buf.String() != "\n" {
		t.Errorf("Fprintln() = %q", buf.String())
	}
}

func ExamplePrint() {
	Print("answer", 42)
	// Output: answer 42
}
