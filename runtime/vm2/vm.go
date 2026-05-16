package vm2

import (
	"io"
	"math/bits"
	"sync"
)

// VM is the vm2 interpreter. It carries a *frame pool plus a set of
// per-size []Value pools (one bucket per power of two from 4 to
// 2048 inclusive) so recursion and deep query plans recycle register
// slabs instead of churning the allocator.
type VM struct {
	Prog    *Program
	Writer  io.Writer
	Globals []Value

	framePool sync.Pool
	regsPools [regsBuckets]sync.Pool
}

const (
	regsMinBucket = 2  // 1 << 2 == 4
	regsMaxBucket = 11 // 1 << 11 == 2048
	regsBuckets   = regsMaxBucket - regsMinBucket + 1
)

type frame struct {
	Fn         *Function
	Regs       []Value
	regsPtr    *[]Value // identity of the pooled slab; nil if unpooled
	IP         int
	RegsBucket int8 // bucket index used to allocate Regs; -1 if unpooled
}

// New constructs a VM for the given program. The pools are initialized
// with constructors that allocate the appropriate size; misses on a
// cold pool pay one allocation, all subsequent calls reuse.
func New(prog *Program, w io.Writer) *VM {
	m := &VM{
		Prog:    prog,
		Writer:  w,
		Globals: make([]Value, prog.NumGlobals),
	}
	m.framePool.New = func() any { return &frame{} }
	for i := range m.regsPools {
		size := 1 << (regsMinBucket + i)
		sz := size
		m.regsPools[i].New = func() any {
			s := make([]Value, sz)
			return &s
		}
	}
	return m
}

// bucketFor returns the regsPools index for a register slab of size n.
// Slabs larger than the top bucket are allocated unpooled (RegsBucket
// = -1) so they don't pin a huge slab in the pool.
func bucketFor(n int) int {
	if n <= (1 << regsMinBucket) {
		return 0
	}
	// ceil(log2(n)) for n > 0
	b := bits.Len(uint(n - 1)) // 1<<b >= n
	idx := b - regsMinBucket
	if idx >= regsBuckets {
		return -1
	}
	return idx
}

// acquireFrame returns a *frame with a regs slab of at least n cells.
// Regs are zeroed for safety: the GC must not see stale pointer cells
// when the slab is reused for a different function.
func (m *VM) acquireFrame(fn *Function) *frame {
	f := m.framePool.Get().(*frame)
	f.Fn = fn
	f.IP = 0
	n := fn.NumRegs
	b := bucketFor(n)
	if b < 0 {
		f.Regs = make([]Value, n)
		f.regsPtr = nil
		f.RegsBucket = -1
		return f
	}
	sp := m.regsPools[b].Get().(*[]Value)
	regs := *sp
	if cap(regs) < n {
		regs = make([]Value, n)
	} else {
		regs = regs[:n]
		clearValues(regs)
	}
	*sp = regs
	f.Regs = regs
	f.regsPtr = sp
	f.RegsBucket = int8(b)
	return f
}

func (m *VM) releaseFrame(f *frame) {
	if f.RegsBucket >= 0 && f.regsPtr != nil {
		// Zero before recycling so the GC drops captured references.
		clearValues(f.Regs)
		*f.regsPtr = f.Regs
		m.regsPools[f.RegsBucket].Put(f.regsPtr)
	}
	f.Fn = nil
	f.Regs = nil
	f.regsPtr = nil
	f.IP = 0
	f.RegsBucket = 0
	m.framePool.Put(f)
}

// clearValues zeros a slice of Value. Avoids the runtime memclr
// dance for non-pointer payloads since most VM allocs are Nums.
func clearValues(v []Value) {
	clear(v)
}
