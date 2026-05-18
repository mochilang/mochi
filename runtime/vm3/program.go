package vm3

// Program is one compilation unit. Functions are indexed by Funcs[i],
// matching OpCall*.C. Entry is the index of the function the
// interpreter starts on when Run is called with no explicit fn.
type Program struct {
	Funcs []*Function
	Entry uint32
}

// JITCallFn is installed by runtime/jit/vm3jit on package init when the
// host architecture has a JIT backend (currently darwin/arm64 and
// linux/amd64). vm3 treats the JIT as opaque: OpCallI64 sites route
// through JITCallFn whenever the callee carries a non-nil JITCode entry
// pointer in its Function record.
//
// argsI64 holds the NumI64Params() preloaded slots. argsF64 is reserved
// for a future Phase 6.2c+ slice that wires f64 parameter passing; it
// is currently always nil on this path. The return is the raw uint64
// from the JIT trampoline; for i64-returning fns the caller reads it
// as int64 directly. deopt=true means the JIT bailed and the caller
// must restart this callee through the interpreter.
var JITCallFn func(vm *VM, fn *Function, argsI64 []int64, argsF64 []float64) (resultBits uint64, deopt bool, err error)
