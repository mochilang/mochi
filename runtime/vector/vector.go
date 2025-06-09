// Package vector provides vector database runtime support.
package vector

import "math"

// --- Core Types ---

// Vector is a fixed-length float32 slice representing a dense embedding.
type Vector []float32

// Entry is a single item in the vector index with an ID and optional metadata.
type Entry struct {
	ID     string
	Vector Vector
	Meta   any // Optional metadata, e.g., label or payload.
}

// DistanceFunc defines a function that computes the distance between two vectors.
type DistanceFunc func(a, b Vector) float32

// --- Built-in Distance Functions ---

// L2 returns Euclidean distance between two vectors.
func L2(a, b Vector) float32 {
	var sum float32
	for i := range a {
		d := a[i] - b[i]
		sum += d * d
	}
	return float32(math.Sqrt(float64(sum)))
}

// Dot returns dot product of two vectors.
func Dot(a, b Vector) float32 {
	var sum float32
	for i := range a {
		sum += a[i] * b[i]
	}
	return sum
}

// Cosine returns 1 - cosine similarity between two vectors (i.e., a distance).
func Cosine(a, b Vector) float32 {
	return 1 - (Dot(a, b) / (Norm(a) * Norm(b)))
}

// Norm returns the L2 norm of a vector.
func Norm(v Vector) float32 {
	var sum float32
	for _, x := range v {
		sum += x * x
	}
	return float32(math.Sqrt(float64(sum)))
}

// --- Index Interface ---

// Index is the interface for vector search engines.
type Index interface {
	Insert(entry Entry) error
	Delete(id string) error
	Search(query Vector, k int) ([]Entry, error)
	Len() int
	Save(path string) error
	Load(path string) error
}

// Options for building an index.
type Options struct {
	Dim     int
	Metric  DistanceFunc // L2, Dot, Cosine
	Backend string       // "flat", "hnsw", etc.
}
