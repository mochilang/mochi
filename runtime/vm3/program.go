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
// linux/amd64). vm3 treats the JIT as opaque: OpCallI64 and OpCallMixed
// sites route through JITCallFn whenever the callee carries a non-nil
// JITCode entry pointer in its Function record.
//
// Args slices are indexed by parameter-bank position, matching
// fn.ParamBanks: argsI64[k] / argsF64[k] / argsCell[k] are read iff
// fn.ParamBanks[k] selects that bank. Unused slots may be zero. The
// return is the raw uint64 from the JIT trampoline; for i64-returning
// fns the caller reads it as int64 directly, for f64 returns it
// decodes via math.Float64frombits. deopt=true means the JIT bailed
// and the caller must restart this callee through the interpreter.
//
// Phase 6.2c wired the simple i64-only path (argsCell always nil);
// Phase 6.2d.2.b adds Cell-bank parameter passing for the lists_fill_sum
// "sum" kernel (and successors), so callers now hand the JIT a populated
// argsCell slice when fn.ParamBanks contains BankCell.
var JITCallFn func(vm *VM, fn *Function, argsI64 []int64, argsF64 []float64, argsCell []Cell) (resultBits uint64, deopt bool, err error)
