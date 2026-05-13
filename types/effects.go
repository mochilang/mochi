package types

import (
	"strings"
)

// EffectLabel identifies one entry in the closed effect vocabulary
// defined by MEP-15 stage 1. Adding a label here is a language-level
// change and requires a fixture sweep; never widen ad hoc.
type EffectLabel uint8

const (
	// EffectIO covers stdout writes and stdin reads (print, json,
	// input). Stage 1 collapses stdout and stdin into a single label
	// because the existing builtin surface does not justify a split.
	EffectIO EffectLabel = iota
	// EffectFS covers local file system reads and writes (load, save).
	EffectFS
	// EffectNet covers network I/O (fetch).
	EffectNet
	// EffectTime covers wall-clock and other non-deterministic clock
	// reads (now). Distinct from EffectIO so the query optimizer can
	// still reject reorderings of time-reading predicates even when
	// stdout is fine.
	EffectTime
	// EffectMeta covers dynamic compilation or evaluation (eval).
	// Distinct from EffectIO so a future compile-time evaluator can
	// reject eval even when stdout is acceptable.
	EffectMeta
	// effectMax is the upper bound used to size the EffectSet bitset
	// and to drive String() ordering. Update together with the labels.
	effectMax
)

var effectNames = [...]string{
	EffectIO:   "io",
	EffectFS:   "fs",
	EffectNet:  "net",
	EffectTime: "time",
	EffectMeta: "meta",
}

// String returns the canonical lowercase identifier for the label.
func (l EffectLabel) String() string {
	if int(l) < len(effectNames) {
		return effectNames[l]
	}
	return "?"
}

// ParseEffectLabel turns a lowercase label identifier into an
// EffectLabel. Returns ok=false for unknown labels; callers raise a
// diagnostic in that case.
func ParseEffectLabel(name string) (EffectLabel, bool) {
	for i, n := range effectNames {
		if n == name {
			return EffectLabel(i), true
		}
	}
	return 0, false
}

// EffectSet is a bitset over the closed EffectLabel vocabulary. The
// empty set is "pure". Sets compare equal by value; the canonical
// printed form sorts labels by their EffectLabel index so two equal
// sets always render identically. MEP-15 E7.
type EffectSet uint8

// NewEffectSet returns the set containing exactly the listed labels.
// Duplicates are normalized away; order is irrelevant.
func NewEffectSet(labels ...EffectLabel) EffectSet {
	var s EffectSet
	for _, l := range labels {
		s |= 1 << l
	}
	return s
}

// EmptyEffects is the canonical pure set. Provided as a named constant
// so call sites read clearly.
var EmptyEffects = EffectSet(0)

// Has reports whether l is in the set.
func (s EffectSet) Has(l EffectLabel) bool {
	return s&(1<<l) != 0
}

// Add returns a new set containing s plus l. The receiver is unchanged.
func (s EffectSet) Add(l EffectLabel) EffectSet {
	return s | (1 << l)
}

// Union returns the set-theoretic union of s and t.
func (s EffectSet) Union(t EffectSet) EffectSet {
	return s | t
}

// IsSubset reports whether every label of s is in t.
func (s EffectSet) IsSubset(t EffectSet) bool {
	return s&^t == 0
}

// IsEmpty reports whether the set has no labels. Equivalent to "pure".
func (s EffectSet) IsEmpty() bool {
	return s == 0
}

// Labels returns the labels in canonical (sorted) order.
func (s EffectSet) Labels() []EffectLabel {
	out := make([]EffectLabel, 0)
	for i := EffectLabel(0); i < effectMax; i++ {
		if s.Has(i) {
			out = append(out, i)
		}
	}
	return out
}

// String renders the set as a comma-separated list of label names in
// canonical order. The empty set renders as "pure" so error messages
// have a useful word to print.
func (s EffectSet) String() string {
	if s.IsEmpty() {
		return "pure"
	}
	parts := make([]string, 0)
	for _, l := range s.Labels() {
		parts = append(parts, l.String())
	}
	return strings.Join(parts, ", ")
}
