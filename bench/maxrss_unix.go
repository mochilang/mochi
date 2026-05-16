//go:build slow && (linux || freebsd || netbsd || openbsd || dragonfly)

package bench

// Linux/BSD getrusage reports ru_maxrss in kilobytes.
const maxrssBytesMultiplier = 1024
