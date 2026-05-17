package vm2jit

import (
	"encoding/binary"
	"unsafe"
)

// pageAlloc allocates a readable+writable (not yet executable) page large
// enough to hold nBytes of native code. Rounds up to the OS page size.
func pageAlloc(nBytes int) ([]byte, error) {
	return pageAllocOS(nBytes)
}

// pageWrite copies native instruction words into page and makes it executable,
// flipping W^X per the host OS policy (pthread_jit_write_protect on macOS,
// mprotect on Linux).
func pageWrite(page []byte, words []uint32) error {
	buf := make([]byte, len(words)*4)
	for i, w := range words {
		binary.LittleEndian.PutUint32(buf[i*4:], w)
	}
	return pageMakeExecOS(page, buf)
}

// pageFree releases a page previously obtained from pageAlloc.
func pageFree(page []byte) error {
	return pageFreeOS(page)
}

// pageEntry returns the raw function-pointer address for the start of page.
// Stored in Function.JITCode and read by the trampoline.
func pageEntry(page []byte) unsafe.Pointer {
	return unsafe.Pointer(&page[0])
}
