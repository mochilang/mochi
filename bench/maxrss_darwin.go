//go:build slow && darwin

package bench

// macOS getrusage reports ru_maxrss in bytes.
const maxrssBytesMultiplier = 1
