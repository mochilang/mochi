// Package time is the Mochi-semantics time runtime for the Go target.
// Mochi's `now()` returns Unix nanoseconds as an int (see VM op OpNow
// in runtime/vm/vm.go). The function here matches that contract so
// the emitter can lower `now()` to a single call. We also expose
// `Format` and `Parse` for the RFC-3339 form Mochi uses in its query
// fixtures (date columns are stored as strings in `tests/vm/valid`).
package time

import gotime "time"

// Now returns the current time as Unix nanoseconds.
func Now() int64 { return gotime.Now().UnixNano() }

// NowMono returns a monotonic-clock counter in nanoseconds. Useful
// for tight benchmark code that the emitter lowers from Mochi's
// `now()` inside a Bench block.
func NowMono() int64 { return gotime.Since(refEpoch).Nanoseconds() }

var refEpoch = gotime.Now()

// FormatRFC3339 formats a Unix-nanos value as RFC-3339 in UTC.
func FormatRFC3339(unixNanos int64) string {
	return gotime.Unix(0, unixNanos).UTC().Format(gotime.RFC3339)
}

// ParseRFC3339 parses an RFC-3339 timestamp and returns Unix nanos.
func ParseRFC3339(s string) (int64, error) {
	t, err := gotime.Parse(gotime.RFC3339, s)
	if err != nil {
		return 0, err
	}
	return t.UnixNano(), nil
}

// Sleep blocks for the given number of nanoseconds. Mochi's existing
// VM does not expose sleep as a builtin; we provide it here for Go
// users importing this library directly.
func Sleep(nanos int64) { gotime.Sleep(gotime.Duration(nanos)) }
