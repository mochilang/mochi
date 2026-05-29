package asyncbridge

import "fmt"

// Mode selects how the synthesised sync shim drives the underlying Python
// coroutine. The zero value is PerCall, which matches the `mochi.toml`
// default and is the safe choice for one-shot scripts.
type Mode int

const (
	// PerCall constructs a fresh asyncio event loop for every shim call
	// via `asyncio.run`. Loop teardown reclaims selectors + signal
	// handlers between calls; the trade-off is ~200μs of loop init cost.
	PerCall Mode = iota

	// Persistent caches one process-global event loop at the first shim
	// call and reuses it via `loop.run_until_complete`. Avoids the
	// per-call teardown but the loop leaks into the host process; the
	// user opts in explicitly via `mochi.toml`.
	Persistent
)

// ParseMode normalises the string form used in `mochi.toml`'s
// `[python] runtime.event-loop` knob. Empty input maps to PerCall so the
// caller can pass a missing value through unchanged.
func ParseMode(s string) (Mode, error) {
	switch s {
	case "", "per-call", "percall":
		return PerCall, nil
	case "persistent":
		return Persistent, nil
	default:
		return PerCall, fmt.Errorf("asyncbridge: unknown event-loop mode %q (want \"per-call\" or \"persistent\")", s)
	}
}

// String returns the canonical token round-trippable through ParseMode.
func (m Mode) String() string {
	switch m {
	case PerCall:
		return "per-call"
	case Persistent:
		return "persistent"
	default:
		return fmt.Sprintf("Mode(%d)", int(m))
	}
}
