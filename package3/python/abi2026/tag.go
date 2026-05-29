package abi2026

import (
	"fmt"
	"strings"
)

// TagClass is the coarse-grained ABI bucket the resolver branches on.
// PyPI today only emits the first three classes; the abi2026 tag
// ships as part of CPython 3.14's 2026-Q1 stable-ABI rollout.
type TagClass int

const (
	// TagClassUnknown is the zero value; Classify returns it only
	// when raw is the empty string. A non-empty raw that does not
	// match any pattern is TagClassUnrecognised + an error.
	TagClassUnknown TagClass = iota

	// TagClassPure is the `none` ABI tag emitted by pure-Python
	// wheels (e.g. `attrs-23.2.0-py3-none-any.whl`). Always accepted
	// regardless of policy.
	TagClassPure

	// TagClassLegacyCPython is the per-minor-version tag
	// `cp3XY` (or PEP 703's `cp3XYt`). Sunset target of the
	// PolicyAbi2026 end state.
	TagClassLegacyCPython

	// TagClassLegacyABI3 is the pre-2026 stable-ABI tag
	// `abi3` (paired with `cp3XY` interpreter tag at the
	// wheel-tag level). Phase 13's slimmer emits these today.
	TagClassLegacyABI3

	// TagClassABI2026 is the new tag the 2026-Q1 transition
	// introduces. The wheel-tag triple looks like
	// `cp314-abi2026-<platform>`; phase 18.2 emits these once
	// CPython 3.14 lands.
	TagClassABI2026

	// TagClassUnrecognised covers tags ClassifyABITag returns an
	// error for. The resolver surfaces this through a SkipReason.
	TagClassUnrecognised
)

// String renders the class for diagnostics. The strings are stable;
// CLI + telemetry switch on them.
func (c TagClass) String() string {
	switch c {
	case TagClassPure:
		return "pure"
	case TagClassLegacyCPython:
		return "legacy-cpython"
	case TagClassLegacyABI3:
		return "legacy-abi3"
	case TagClassABI2026:
		return "abi2026"
	case TagClassUnrecognised:
		return "unrecognised"
	default:
		return "unknown"
	}
}

// ClassifyABITag pigeonholes a raw ABI tag into its class.
// Recognised shapes:
//
//   - "none" -> TagClassPure
//   - "abi3" -> TagClassLegacyABI3
//   - "abi2026" -> TagClassABI2026
//   - "cp<major><minor>" / "cp<major><minor>t" -> TagClassLegacyCPython
//
// Anything else returns TagClassUnrecognised + an error so the
// resolver can attach a SkipReason at install time.
func ClassifyABITag(raw string) (TagClass, error) {
	switch raw {
	case "":
		return TagClassUnknown, fmt.Errorf("abi2026: empty ABI tag")
	case "none":
		return TagClassPure, nil
	case "abi3":
		return TagClassLegacyABI3, nil
	case "abi2026":
		return TagClassABI2026, nil
	}
	if strings.HasPrefix(raw, "cp") && isCPythonABI(raw) {
		return TagClassLegacyCPython, nil
	}
	return TagClassUnrecognised, fmt.Errorf("abi2026: unrecognised ABI tag %q", raw)
}

func isCPythonABI(s string) bool {
	rest := strings.TrimPrefix(s, "cp")
	if strings.HasSuffix(rest, "t") {
		rest = strings.TrimSuffix(rest, "t")
	}
	if len(rest) < 2 {
		return false
	}
	for _, r := range rest {
		if r < '0' || r > '9' {
			return false
		}
	}
	return true
}
