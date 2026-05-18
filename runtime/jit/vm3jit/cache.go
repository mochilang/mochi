package vm3jit

import (
	"encoding/binary"
	"unsafe"
)

// pageAlloc reserves an OS page large enough for nBytes of native
// code. The page is RW but not yet executable.
func pageAlloc(nBytes int) ([]byte, error) {
	return pageAllocOS(nBytes)
}

// pageWrite copies words into page and flips it to RX. words is a
// little-endian-packed AArch64 instruction stream.
func pageWrite(page []byte, words []uint32) error {
	buf := make([]byte, len(words)*4)
	for i, w := range words {
		binary.LittleEndian.PutUint32(buf[i*4:], w)
	}
	return pageMakeExecOS(page, buf)
}

// pageFree releases a page from pageAlloc.
func pageFree(page []byte) error { return pageFreeOS(page) }

// pageEntry returns the entry address for the trampoline.
func pageEntry(page []byte) unsafe.Pointer { return unsafe.Pointer(&page[0]) }
