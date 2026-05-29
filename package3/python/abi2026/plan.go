package abi2026

import (
	"fmt"
	"sort"
	"strings"
)

// WheelCandidate is one resolver-supplied wheel the selector ranks.
// Filename is the canonical PEP 491 filename. URL is what the
// install loop downloads.
type WheelCandidate struct {
	Filename string
	URL      string
}

// SelectionResult is what the selector returns.
type SelectionResult struct {
	Chosen     *WheelCandidate
	ChosenTag  string
	ChosenClass TagClass
	Reasons    map[string]string
}

// Selector picks the best wheel from candidates under Policy.
//
// Ordering (descending preference):
//
//   - TagClassABI2026 (when policy is Abi2026 or Both)
//   - TagClassLegacyABI3 (when policy is Legacy or Both)
//   - TagClassLegacyCPython (when policy is Legacy or Both)
//   - TagClassPure (always, as the fallback)
//
// Within a class, ties break on filename for determinism.
type Selector struct {
	Policy Policy
}

// Select runs the picker.
func (s Selector) Select(candidates []WheelCandidate) (*SelectionResult, error) {
	if s.Policy == PolicyUnknown {
		return nil, fmt.Errorf("abi2026: Selector.Policy must be set")
	}
	result := &SelectionResult{Reasons: map[string]string{}}

	type ranked struct {
		cand  WheelCandidate
		abi   string
		class TagClass
	}
	var pool []ranked

	for _, c := range candidates {
		abi, err := wheelABI(c.Filename)
		if err != nil {
			result.Reasons[c.Filename] = err.Error()
			continue
		}
		class, err := ClassifyABITag(abi)
		if err != nil {
			result.Reasons[c.Filename] = err.Error()
			continue
		}
		if !s.Policy.Accepts(class) {
			result.Reasons[c.Filename] = fmt.Sprintf("class %q rejected by policy %q", class, s.Policy)
			continue
		}
		pool = append(pool, ranked{cand: c, abi: abi, class: class})
	}

	if len(pool) == 0 {
		return result, nil
	}

	sort.SliceStable(pool, func(i, j int) bool {
		ri := s.Policy.Rank(pool[i].class)
		rj := s.Policy.Rank(pool[j].class)
		if ri != rj {
			return ri > rj
		}
		return pool[i].cand.Filename < pool[j].cand.Filename
	})

	winner := pool[0]
	result.Chosen = &winner.cand
	result.ChosenTag = winner.abi
	result.ChosenClass = winner.class
	return result, nil
}

// wheelABI extracts the ABI tag (penultimate hyphen segment) from a
// PEP 491 wheel filename.
func wheelABI(name string) (string, error) {
	base := strings.TrimSuffix(name, ".whl")
	if base == name {
		return "", fmt.Errorf("abi2026: %q does not end in .whl", name)
	}
	parts := strings.Split(base, "-")
	if len(parts) < 5 {
		return "", fmt.Errorf("abi2026: wheel %q is missing fields (want dist-ver-py-abi-plat)", name)
	}
	return parts[len(parts)-2], nil
}
