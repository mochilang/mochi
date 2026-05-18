package vm3jit

import "unsafe"

// pageAlloc reserves an OS page large enough for nBytes of native
// code. The page is RW but not yet executable.
func pageAlloc(nBytes int) ([]byte, error) {
	return pageAllocOS(nBytes)
}

// pageWrite copies raw native code into page and flips it to RX. Each
// backend is responsible for producing the platform's byte stream;
// pageWrite is intentionally byte-oriented so it can carry both
// fixed-width (AArch64: little-endian-packed 32-bit words) and
// variable-width (x86_64) instruction encodings.
func pageWrite(page []byte, raw []byte) error {
	return pageMakeExecOS(page, raw)
}

// pageFree releases a page from pageAlloc.
func pageFree(page []byte) error { return pageFreeOS(page) }

// pageEntry returns the entry address for the trampoline.
func pageEntry(page []byte) unsafe.Pointer { return unsafe.Pointer(&page[0]) }
