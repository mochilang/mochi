package tracejit

import "mochi/runtime/jit/tmpljit"

// Engine wires the interpreter to the recorder and the compiled
// trace cache. Each Engine is single-goroutine; concurrent callers
// must use one Engine per goroutine (matching the MEP-31 spec for
// per-goroutine profiling).
type Engine struct {
	prog tmpljit.Program

	// Per-back-edge-target hit counts. Indexed by the bytecode PC
	// of the loop header (i.e. the Jnz target).
	hits map[int]int

	// Compiled traces indexed by header PC. A nil entry means
	// "tried to record and failed"; the engine will not retry,
	// which matches the spec's permanent-blacklist behavior at
	// the very simplest level (no retry budget).
	traces      map[int]*CompiledTrace
	blacklisted map[int]bool
}

// New returns an Engine ready to run prog. The Engine takes
// ownership of any compiled traces and will Free() them on Close.
func New(prog tmpljit.Program) *Engine {
	return &Engine{
		prog:        prog,
		hits:        make(map[int]int),
		traces:      make(map[int]*CompiledTrace),
		blacklisted: make(map[int]bool),
	}
}

// Close releases every compiled trace's executable page.
func (e *Engine) Close() {
	for _, ct := range e.traces {
		_ = ct.Free()
	}
	e.traces = nil
}

// Run executes the program with arg in r0 and returns whatever
// OpRet selects. It interprets every opcode itself but, on each
// taken back-edge, either records a trace, jumps into a compiled
// trace, or just bumps the hit counter. The interpreter loop is
// otherwise identical to tmpljit.Interp.
func (e *Engine) Run(arg int64) int64 {
	var regs [tmpljit.NumRegs]int64
	regs[0] = arg
	pc := 0
	for {
		ins := e.prog[pc]
		nextPC := pc + 1
		switch ins.Op {
		case tmpljit.OpMovImm:
			regs[ins.Dst] = int64(ins.Imm)
		case tmpljit.OpAdd:
			regs[ins.Dst] = regs[ins.A] + regs[ins.B]
		case tmpljit.OpMul:
			regs[ins.Dst] = regs[ins.A] * regs[ins.B]
		case tmpljit.OpLt:
			if regs[ins.A] < regs[ins.B] {
				regs[ins.Dst] = 1
			} else {
				regs[ins.Dst] = 0
			}
		case tmpljit.OpJnz:
			if regs[ins.A] == 0 {
				// Fall through (loop exit on the very first
				// iteration). Nothing to record.
				break
			}
			target := nextPC + int(ins.Imm)
			if target < nextPC {
				// Backward branch == back-edge. Check the trace
				// cache; if hot but uncompiled, record now.
				if ct, ok := e.traces[target]; ok {
					ct.run(&regs)
					// Trace ran to exhaustion; resume at exit PC.
					nextPC = ct.trace.ExitPC
					break
				}
				if !e.blacklisted[target] {
					e.hits[target]++
					if e.hits[target] >= TraceThreshold {
						// Snapshot registers, then record from
						// the header. The recorder mutates the
						// snapshot, so we restore from a copy
						// once recording finishes.
						snap := regs
						trace, err := recordIteration(e.prog, &snap, target)
						if err != nil {
							e.blacklisted[target] = true
						} else if ct, cerr := Compile(trace); cerr == nil {
							e.traces[target] = ct
							// Replay the iteration we just used
							// for recording by jumping into the
							// freshly compiled trace, starting
							// from the current (un-snapshotted)
							// register state. The trace will
							// run from this iteration onward.
							ct.run(&regs)
							nextPC = ct.trace.ExitPC
							break
						} else {
							e.blacklisted[target] = true
						}
					}
				}
				nextPC = target
			} else {
				// Forward conditional (not used by FillSumProgram
				// but supported for completeness).
				nextPC = target
			}
		case tmpljit.OpRet:
			return regs[ins.A]
		}
		pc = nextPC
	}
}

// MustCompile is a test/bench convenience: builds an Engine, forces
// trace recording on the first hot loop, and returns the Engine
// ready for steady-state Run calls. Used by benchmarks to exclude
// recording cost from the steady-state measurement.
func MustCompile(prog tmpljit.Program, warmupArg int64) *Engine {
	e := New(prog)
	for i := 0; i < TraceThreshold+2; i++ {
		e.Run(warmupArg)
	}
	return e
}
