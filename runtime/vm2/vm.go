package vm2

import "sync"

// VM holds the program plus the per-run Objects table for ref-tagged
// Cells (boxed wide ints, strings, lists, maps, closures). The table
// is reset on each Run so pure-numeric programs never grow it.
type VM struct {
	Program *Program
	Objects []any
}

// New constructs a VM bound to a program.
func New(p *Program) *VM { return &VM{Program: p} }

// AddObject appends to the Objects table and returns the index suitable
// for CPtr. Compilers and FFI use this to hand a ref-typed value to
// running code.
func (vm *VM) AddObject(o any) uint64 {
	vm.Objects = append(vm.Objects, o)
	return uint64(len(vm.Objects) - 1)
}

// frame is one activation record. Regs is a slab borrowed from the
// per-size pool; regsPtr holds the cached *[]Cell so releaseFrame
// returns the same allocation without a fresh address-of (which would
// escape).
type frame struct {
	Fn         *Function
	Regs       []Cell
	regsPtr    *[]Cell
	IP         int
	RetReg     int32
	RegsBucket int8
	Parent     *frame
}

var framePool = sync.Pool{New: func() any { return &frame{} }}

const (
	regsMinBucket = 2  // 2^2 = 4
	regsMaxBucket = 11 // 2^11 = 2048
)

var regsPools [regsMaxBucket - regsMinBucket + 1]sync.Pool

func init() {
	for i := range regsPools {
		size := 1 << (uint(i) + regsMinBucket)
		regsPools[i].New = func() any {
			r := make([]Cell, size)
			return &r
		}
	}
}

func bucketFor(n int) int {
	b := regsMinBucket
	for (1 << b) < n {
		b++
	}
	if b > regsMaxBucket {
		return -1
	}
	return b - regsMinBucket
}

func (vm *VM) acquireFrame(fn *Function) *frame {
	fr := framePool.Get().(*frame)
	fr.Fn = fn
	fr.IP = 0
	fr.Parent = nil
	bi := bucketFor(fn.NumRegs)
	if bi < 0 {
		regs := make([]Cell, fn.NumRegs)
		fr.Regs = regs
		fr.regsPtr = nil
		fr.RegsBucket = -1
		return fr
	}
	p := regsPools[bi].Get().(*[]Cell)
	fr.Regs = (*p)[:fn.NumRegs]
	fr.regsPtr = p
	fr.RegsBucket = int8(bi)
	return fr
}

func (vm *VM) releaseFrame(fr *frame) {
	if fr.regsPtr != nil {
		// zero the live window so stale ptr indices do not pin
		// Objects entries beyond their useful life.
		for i := range fr.Regs {
			fr.Regs[i] = 0
		}
		regsPools[fr.RegsBucket].Put(fr.regsPtr)
	}
	fr.Fn = nil
	fr.Regs = nil
	fr.regsPtr = nil
	fr.Parent = nil
	framePool.Put(fr)
}
