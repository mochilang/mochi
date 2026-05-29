package toml

import "testing"

func TestDecoderString(t *testing.T) {
	tree, _ := Parse(`s = "hello"
n = 1
`)
	d := NewDecoder(tree)
	s, present, err := d.String("s")
	if err != nil || !present || s != "hello" {
		t.Errorf("String(s) = (%q, %v, %v)", s, present, err)
	}
	if _, present, _ := d.String("missing"); present {
		t.Errorf("missing present = true; want false")
	}
	if _, _, err := d.String("n"); err == nil {
		t.Error("String(n) err = nil; want type error")
	}
}

func TestDecoderStringRequired(t *testing.T) {
	tree, _ := Parse(`s = "x"`)
	d := NewDecoder(tree)
	if s, err := d.StringRequired("s"); err != nil || s != "x" {
		t.Errorf("StringRequired(s) = (%q, %v)", s, err)
	}
	if _, err := d.StringRequired("missing"); err == nil {
		t.Error("StringRequired(missing) err = nil; want error")
	}
}

func TestDecoderInt(t *testing.T) {
	tree, _ := Parse(`n = 42`)
	d := NewDecoder(tree)
	n, present, err := d.Int("n")
	if err != nil || !present || n != 42 {
		t.Errorf("Int = (%d, %v, %v)", n, present, err)
	}
}

func TestDecoderBool(t *testing.T) {
	tree, _ := Parse(`b = true`)
	d := NewDecoder(tree)
	b, present, err := d.Bool("b")
	if err != nil || !present || !b {
		t.Errorf("Bool = (%v, %v, %v)", b, present, err)
	}
}

func TestDecoderTable(t *testing.T) {
	tree, _ := Parse(`[s]
host = "h"
port = 8080
`)
	d := NewDecoder(tree)
	sub, present, err := d.Table("s")
	if err != nil || !present {
		t.Fatalf("Table(s) err = %v present = %v", err, present)
	}
	host, _, _ := sub.String("host")
	if host != "h" {
		t.Errorf("nested host = %q", host)
	}
}

func TestDecoderTableArray(t *testing.T) {
	tree, _ := Parse(`[[pkg]]
name = "a"

[[pkg]]
name = "b"
`)
	d := NewDecoder(tree)
	arr, present, err := d.TableArray("pkg")
	if err != nil || !present || len(arr) != 2 {
		t.Fatalf("TableArray = (%v, %v, %v)", arr, present, err)
	}
	n0, _, _ := arr[0].String("name")
	n1, _, _ := arr[1].String("name")
	if n0 != "a" || n1 != "b" {
		t.Errorf("names = %q, %q", n0, n1)
	}
}

func TestDecoderStringArray(t *testing.T) {
	tree, _ := Parse(`xs = ["a", "b", "c"]`)
	d := NewDecoder(tree)
	xs, present, err := d.StringArray("xs")
	if err != nil || !present || len(xs) != 3 {
		t.Errorf("StringArray = (%v, %v, %v)", xs, present, err)
	}
}

func TestDecoderStringArrayWrongType(t *testing.T) {
	tree, _ := Parse(`xs = [1, 2]`)
	d := NewDecoder(tree)
	if _, _, err := d.StringArray("xs"); err == nil {
		t.Error("StringArray on int array err = nil; want type error")
	}
}

func TestDecoderKeys(t *testing.T) {
	tree, _ := Parse(`a = 1
b = 2
`)
	d := NewDecoder(tree)
	keys := d.Keys()
	if len(keys) != 2 {
		t.Errorf("Keys = %v; want 2", keys)
	}
}
