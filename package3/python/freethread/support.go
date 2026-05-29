package freethread

import "fmt"

// SupportLevel categorises a wheel's compatibility with a
// free-threaded interpreter. The CLI surfaces this as a single
// per-package badge ("safe" / "untested" / "unsafe").
type SupportLevel int

const (
	// SupportUnknown is the zero value; functions never return it,
	// it is the sentinel the resolver uses before any check has run.
	SupportUnknown SupportLevel = iota

	// SupportFull means the wheel either ships a `cp3XYt`-tagged
	// variant or is pure-Python (no compiled extensions). The
	// install proceeds without warnings.
	SupportFull

	// SupportUntested means the wheel only ships a legacy
	// `cp3XY`-tagged variant. The install proceeds with a warning
	// when `--allow-untested-freethread` is on, and fails otherwise.
	SupportUntested

	// SupportIncompatible means the wheel is in the deny-list
	// (known to deadlock or corrupt state under free-threaded).
	// The install always fails.
	SupportIncompatible
)

// String renders the level for diagnostics. The strings are stable
// and the CLI + telemetry switch on them.
func (s SupportLevel) String() string {
	switch s {
	case SupportFull:
		return "full"
	case SupportUntested:
		return "untested"
	case SupportIncompatible:
		return "incompatible"
	default:
		return "unknown"
	}
}

// WheelClass is the category the WheelCompat checker assigns when
// scoring one resolved package. PureFile / ExtensionFile cover the
// two physical shapes; the checker uses them to short-circuit.
type WheelClass int

const (
	WheelClassUnknown WheelClass = iota

	// WheelClassPure is a no-extension wheel (`*-none-any.whl`).
	// Always SupportFull under free-threaded.
	WheelClassPure

	// WheelClassFreeThreaded is a wheel whose ABI tag is `cp3XYt`.
	// Always SupportFull when minor matches the target.
	WheelClassFreeThreaded

	// WheelClassLegacy is a wheel whose ABI tag is `cp3XY` and is
	// not in the deny-list. SupportUntested.
	WheelClassLegacy

	// WheelClassDenied is a wheel whose package name is in the
	// deny-list. SupportIncompatible regardless of tag.
	WheelClassDenied
)

// String renders the class for diagnostics.
func (c WheelClass) String() string {
	switch c {
	case WheelClassPure:
		return "pure"
	case WheelClassFreeThreaded:
		return "free-threaded"
	case WheelClassLegacy:
		return "legacy"
	case WheelClassDenied:
		return "denied"
	default:
		return "unknown"
	}
}

// WheelCompat scores one wheel against a target interpreter. It is
// the contract sub-phase 17.3's CLI verb consumes.
type WheelCompat struct {
	// Target is the interpreter ABI the install is for. Major + Minor
	// + FreeThreaded must be set; Interpreter is ignored ("cp" is
	// the only value phase 17 ships).
	Target ABITag

	// DenyList is a set of package names the operator has declared
	// known-unsafe even when a free-threaded build is offered. The
	// MEP-71 spec maintains a small bootstrap list (numpy <2.1,
	// scipy <1.15, lxml <6.0); operators extend it via config.
	DenyList map[string]struct{}
}

// ResolvedWheel is the resolver-supplied minimum information the
// scorer needs.
type ResolvedWheel struct {
	Distribution string  // canonical name, lower-case
	Filename     string  // canonical PEP 491 filename
	ABI          string  // ABI tag from the filename
	Pure         bool    // true iff the wheel is *-none-any.whl
}

// Score returns the wheel's classification + support level. ABI parse
// errors short-circuit to Incompatible; the resolver should surface
// the underlying error through a SkipReason.
func (c WheelCompat) Score(rw ResolvedWheel) (WheelClass, SupportLevel, error) {
	if _, denied := c.DenyList[rw.Distribution]; denied {
		return WheelClassDenied, SupportIncompatible, nil
	}
	if rw.Pure {
		return WheelClassPure, SupportFull, nil
	}
	tag, err := ParseABITag(rw.ABI)
	if err != nil {
		return WheelClassUnknown, SupportIncompatible, fmt.Errorf("freethread: score %s: %w", rw.Distribution, err)
	}
	if tag.Major != c.Target.Major || tag.Minor != c.Target.Minor {
		// The resolver should not have handed us a mismatched
		// minor; flag it so the bug surfaces in the umbrella test.
		return WheelClassUnknown, SupportIncompatible, fmt.Errorf("freethread: score %s: minor %d.%d != target %d.%d", rw.Distribution, tag.Major, tag.Minor, c.Target.Major, c.Target.Minor)
	}
	if tag.FreeThreaded {
		return WheelClassFreeThreaded, SupportFull, nil
	}
	if c.Target.FreeThreaded {
		return WheelClassLegacy, SupportUntested, nil
	}
	return WheelClassLegacy, SupportFull, nil
}
