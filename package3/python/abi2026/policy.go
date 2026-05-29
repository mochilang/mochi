package abi2026

import "fmt"

// Policy is the `abi-tag-policy` config knob the operator picks per
// install. The default during the 2026-Q1 migration window is
// PolicyBoth; PolicyLegacy is the pre-migration safety hatch;
// PolicyAbi2026 is the post-migration end state.
type Policy int

const (
	// PolicyUnknown is the zero value; ParsePolicy + Accepts both
	// reject it so an unset Policy never silently downgrades.
	PolicyUnknown Policy = iota

	// PolicyLegacy accepts TagClassPure + TagClassLegacyCPython +
	// TagClassLegacyABI3 only. Rejects TagClassABI2026 (so an
	// operator that wants to delay the migration can pin to it).
	PolicyLegacy

	// PolicyAbi2026 accepts TagClassPure + TagClassABI2026 only.
	// Used once a vendor has fully migrated and wants the build
	// to fail loudly if anyone ships them a legacy wheel.
	PolicyAbi2026

	// PolicyBoth accepts all 4 classes. Selector ranks them with
	// TagClassABI2026 > TagClassLegacyABI3 > TagClassLegacyCPython,
	// and TagClassPure is always accepted as a fallback wheel.
	PolicyBoth
)

// String renders the policy for diagnostics + lockfile emit.
func (p Policy) String() string {
	switch p {
	case PolicyLegacy:
		return "legacy"
	case PolicyAbi2026:
		return "abi2026"
	case PolicyBoth:
		return "both"
	default:
		return "unknown"
	}
}

// ParsePolicy accepts the same three strings the lockfile + CLI
// surface uses. Empty is an error (the operator must spell it
// explicitly); unknown is an error.
func ParsePolicy(s string) (Policy, error) {
	switch s {
	case "legacy":
		return PolicyLegacy, nil
	case "abi2026":
		return PolicyAbi2026, nil
	case "both":
		return PolicyBoth, nil
	case "":
		return PolicyUnknown, fmt.Errorf("abi2026: policy must be specified")
	default:
		return PolicyUnknown, fmt.Errorf("abi2026: unknown policy %q (want legacy|abi2026|both)", s)
	}
}

// Accepts reports whether class is admissible under p. Pure
// (no-extension) wheels are always accepted; PolicyUnknown is never
// (the Selector returns an error before reaching Accepts).
func (p Policy) Accepts(class TagClass) bool {
	if class == TagClassPure {
		return p != PolicyUnknown
	}
	switch p {
	case PolicyLegacy:
		return class == TagClassLegacyCPython || class == TagClassLegacyABI3
	case PolicyAbi2026:
		return class == TagClassABI2026
	case PolicyBoth:
		return class == TagClassLegacyCPython || class == TagClassLegacyABI3 || class == TagClassABI2026
	default:
		return false
	}
}

// Rank returns the per-class preference under p. Higher wins; the
// Selector uses it to pick the best wheel when multiple classes
// remain after Accepts filtering. Pure is the lowest non-zero rank
// because the resolver should prefer a compiled wheel when one is
// available and admissible.
func (p Policy) Rank(class TagClass) int {
	if !p.Accepts(class) {
		return 0
	}
	switch class {
	case TagClassABI2026:
		return 40
	case TagClassLegacyABI3:
		return 30
	case TagClassLegacyCPython:
		return 20
	case TagClassPure:
		return 10
	default:
		return 0
	}
}
