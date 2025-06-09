package vector

import (
	"math"
	"testing"
)

func floatEq(a, b, eps float32) bool {
	if a > b {
		return a-b < eps
	}
	return b-a < eps
}

func TestDistances(t *testing.T) {
	a := Vector{1, 0, 0}
	b := Vector{0, 1, 0}

	if d := L2(a, b); !floatEq(d, float32(math.Sqrt2), 1e-5) {
		t.Fatalf("L2 distance mismatch: %v", d)
	}

	if dot := Dot(a, b); dot != 0 {
		t.Fatalf("Dot product mismatch: %v", dot)
	}

	if cos := Cosine(a, b); !floatEq(cos, 1, 1e-6) {
		t.Fatalf("Cosine distance mismatch: %v", cos)
	}

	if n := Norm(a); !floatEq(n, 1, 1e-6) {
		t.Fatalf("Norm mismatch: %v", n)
	}
}

func TestFlatIndex_Basic(t *testing.T) {
	idx := NewFlat(3, L2)

	idx.Insert(Entry{ID: "a", Vector: Vector{1, 0, 0}})
	idx.Insert(Entry{ID: "b", Vector: Vector{0, 1, 0}})
	idx.Insert(Entry{ID: "c", Vector: Vector{0, 0, 1}})

	if idx.Len() != 3 {
		t.Fatalf("unexpected len %d", idx.Len())
	}

	res, err := idx.Search(Vector{0.9, 0.1, 0}, 2)
	if err != nil {
		t.Fatalf("search error: %v", err)
	}
	if len(res) != 2 {
		t.Fatalf("expected 2 results, got %d", len(res))
	}
	if res[0].ID != "a" {
		t.Fatalf("expected closest 'a', got %s", res[0].ID)
	}

	if err := idx.Delete("b"); err != nil {
		t.Fatalf("delete error: %v", err)
	}
	if idx.Len() != 2 {
		t.Fatalf("unexpected len after delete %d", idx.Len())
	}
}
