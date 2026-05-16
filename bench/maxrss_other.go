//go:build slow && !darwin && !linux && !freebsd && !netbsd && !openbsd && !dragonfly

package bench

// Fallback for hosts we have not validated. The harness will still
// record the raw maxrss value, just without unit normalization.
const maxrssBytesMultiplier = 1
