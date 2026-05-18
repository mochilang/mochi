//go:build !(darwin && arm64) && !(linux && amd64)

package trampoline

import "unsafe"

// Call panics on platforms without a JIT backend.
func Call(_ unsafe.Pointer, _ unsafe.Pointer) uint64 {
	panic("trampoline: not yet implemented on this platform")
}

// CallStatus panics on platforms without a JIT backend.
func CallStatus(_ unsafe.Pointer, _ unsafe.Pointer, _ unsafe.Pointer) uint64 {
	panic("trampoline: not yet implemented on this platform")
}

// CallStatusFF panics on platforms without a JIT backend.
func CallStatusFF(_ unsafe.Pointer, _ unsafe.Pointer, _ unsafe.Pointer, _ unsafe.Pointer) uint64 {
	panic("trampoline: not yet implemented on this platform")
}

// CallStatusM panics on platforms without a JIT backend.
func CallStatusM(_ unsafe.Pointer, _ unsafe.Pointer, _ unsafe.Pointer, _ unsafe.Pointer, _ unsafe.Pointer, _ unsafe.Pointer) uint64 {
	panic("trampoline: not yet implemented on this platform")
}
