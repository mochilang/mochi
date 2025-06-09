package vector

import (
	"errors"
	"sort"
	"sync"
)

// flatIndex is a brute-force vector index for small-scale or baseline usage.
type flatIndex struct {
	mu      sync.RWMutex
	dim     int
	metric  DistanceFunc
	entries []Entry
}

// NewFlat creates a new flat index with the given dimension and metric.
func NewFlat(dim int, metric DistanceFunc) Index {
	return &flatIndex{
		dim:    dim,
		metric: metric,
	}
}

func (f *flatIndex) Insert(e Entry) error {
	if len(e.Vector) != f.dim {
		return errors.New("vector dimension mismatch")
	}
	f.mu.Lock()
	defer f.mu.Unlock()
	f.entries = append(f.entries, e)
	return nil
}

func (f *flatIndex) Delete(id string) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	for i, e := range f.entries {
		if e.ID == id {
			f.entries = append(f.entries[:i], f.entries[i+1:]...)
			return nil
		}
	}
	return errors.New("entry not found")
}

func (f *flatIndex) Search(query Vector, k int) ([]Entry, error) {
	if len(query) != f.dim {
		return nil, errors.New("query dimension mismatch")
	}
	type scored struct {
		entry Entry
		score float32
	}
	f.mu.RLock()
	defer f.mu.RUnlock()

	scores := make([]scored, 0, len(f.entries))
	for _, e := range f.entries {
		dist := f.metric(query, e.Vector)
		scores = append(scores, scored{entry: e, score: dist})
	}

	sort.Slice(scores, func(i, j int) bool {
		return scores[i].score < scores[j].score
	})

	n := k
	if n > len(scores) {
		n = len(scores)
	}
	result := make([]Entry, n)
	for i := 0; i < n; i++ {
		result[i] = scores[i].entry
	}
	return result, nil
}

func (f *flatIndex) Len() int {
	f.mu.RLock()
	defer f.mu.RUnlock()
	return len(f.entries)
}

func (f *flatIndex) Save(path string) error {
	// Placeholder: implement persistence if needed
	return nil
}

func (f *flatIndex) Load(path string) error {
	// Placeholder: implement loading if needed
	return nil
}
