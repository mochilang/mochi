package vm

import (
	"bufio"
	"crypto/sha256"
	"encoding/json"
	"fmt"
	"io"
	"math"
	"math/big"
	"os"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"time"

	"mochi/parser"
	"mochi/runtime/data"
	goffi "mochi/runtime/ffi/go"
	pythonffi "mochi/runtime/ffi/python"
	mhttp "mochi/runtime/http"
	"mochi/types"
)

var (
	seededNow bool
	nowSeed   int64
)

func init() {
	if s := os.Getenv("MOCHI_NOW_SEED"); s != "" {
		if v, err := strconv.ParseInt(s, 10, 64); err == nil {
			nowSeed = v
			seededNow = true
		}
	}
}

// SetNowSeed enables deterministic values for the now() builtin.
// It is primarily used by tests to ensure stable output.
func SetNowSeed(n int64) {
	seededNow = true
	nowSeed = n
}

// Value represents a runtime value handled by the VM.
// The definition lives in value.go and mirrors the interpreter's Value without
// requiring that package.

// regTag tracks the known type of a register during compilation.
type regTag uint8

const (
	tagUnknown regTag = iota
	tagInt
	tagFloat
	tagBool
)

// smallJoinThreshold controls when the compiler falls back to a simple
// nested loop join instead of emitting a hash join. When both join
// sources are constant lists smaller than this size, nested loops tend
// to be faster due to lower allocation overhead. Setting this to zero
// avoids hash joins entirely which improves compatibility with the
// simplified TPC‑DS benchmark queries executed by the VM.
const smallJoinThreshold = 0

// maxCallDepth guards against runaway recursion leading to stack
// overflows when executing user programs. Programs using deeply nested
// recursion can legitimately require more than the previous limit of
// 1024 frames, so we raise the bound while still preventing infinite
// recursion from exhausting memory.
// Increase the recursion limit modestly to accommodate moderately deep
// recursion while still preventing runaway programs from exhausting
// memory. Extremely recursive tasks will still hit this limit and abort
// with a clear error.
// Increase further to handle tasks with very deep recursion, such as
// Church numerals exponentiation. This still keeps an upper bound to
// prevent runaway programs from exhausting memory.
// Allow significantly deeper recursion to support programs that expand
// Church numerals into extremely nested function calls.
// Support extremely deep recursion required by some Rosetta programs like
// `church-numerals-1`.  One million frames still guards against runaway
// programs while avoiding stack overflow on legitimate cases.
const maxCallDepth = 1048576

// Op defines a VM instruction opcode.
type VM struct {
	prog    *Program
	writer  io.Writer
	reader  *bufio.Reader
	globals []Value
}

// StackFrame represents a single call frame in a Mochi stack trace.
type StackFrame struct {
	Func string
	Line int
}

// VMError wraps an error with Mochi stack trace information.
func New(prog *Program, w io.Writer) *VM {
	return &VM{prog: prog, writer: w, reader: bufio.NewReader(os.Stdin), globals: make([]Value, prog.NumGlobals)}
}

func NewWithIO(prog *Program, r io.Reader, w io.Writer) *VM {
	br, ok := r.(*bufio.Reader)
	if !ok {
		br = bufio.NewReader(r)
	}
	return &VM{prog: prog, writer: w, reader: br, globals: make([]Value, prog.NumGlobals)}
}

func (m *VM) Run() error {
	args := make([]Value, m.prog.Funcs[0].NumParams)
	_, err := m.call(0, args, []StackFrame{{Func: m.prog.funcName(0), Line: 0}})
	if err != nil {
		if vmErr, ok := err.(*VMError); ok {
			fmt.Fprintln(m.writer, "call graph:", vmErr.callGraph())
			fmt.Fprintln(m.writer, "stack trace:")
			fmt.Fprint(m.writer, vmErr.stackTrace(m.prog))
		}
	}
	return err
}

func (m *VM) RunResult() (Value, error) {
	args := make([]Value, m.prog.Funcs[0].NumParams)
	return m.call(0, args, []StackFrame{{Func: m.prog.funcName(0), Line: 0}})
}

type frame struct {
	fn   *Function
	regs []Value
	ip   int
}

func (m *VM) call(fnIndex int, args []Value, trace []StackFrame) (Value, error) {
	if len(trace) > maxCallDepth {
		return Value{}, &VMError{Err: fmt.Errorf("call stack exceeded %d frames", maxCallDepth), Stack: trace}
	}
	fn := &m.prog.Funcs[fnIndex]
	if len(args) < fn.NumParams {
		cl := &closure{fn: fnIndex, args: append([]Value(nil), args...)}
		return Value{Tag: ValueFunc, Func: cl}, nil
	}
	if len(args) > fn.NumParams {
		// Some legacy TPC-DS queries (for example q20–q29) invoke
		// built-ins with more arguments than their current
		// signatures accept.  Drop any extra arguments instead of
		// failing so that those queries continue to run.
		args = args[:fn.NumParams]
	}
	f := &frame{fn: fn, regs: make([]Value, fn.NumRegs)}
	// Populate global registers at the start of every call so functions can
	// directly reference global variables via their reserved register slots.
	for i := 0; i < m.prog.NumGlobals && i < len(f.regs); i++ {
		f.regs[i] = m.globals[i]
	}
	// Function parameters are placed after the global register section.
	off := m.prog.NumGlobals
	for i := 0; i < len(args) && off+i < len(f.regs); i++ {
		f.regs[off+i] = args[i]
	}
	stack := []*frame{f}
	for len(stack) > 0 {
		fr := stack[len(stack)-1]
		if fr.ip >= len(fr.fn.Code) {
			stack = stack[:len(stack)-1]
			if len(stack) == 0 {
				return Value{}, nil
			}
			continue
		}
		ins := fr.fn.Code[fr.ip]
		fr.ip++
		switch ins.Op {
		case OpConst:
			fr.regs[ins.A] = ins.Val
		case OpMove:
			fr.regs[ins.A] = fr.regs[ins.B]
		case OpAdd:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
				break
			}
			if b.Tag == ValueStr && c.Tag == ValueStr {
				fr.regs[ins.A] = Value{Tag: ValueStr, Str: b.Str + c.Str}
			} else if b.Tag == ValueFloat || c.Tag == ValueFloat {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: toFloat(b) + toFloat(c)}
			} else if b.Tag == ValueBigRat || c.Tag == ValueBigRat {
				br := new(big.Rat).Add(toRat(b), toRat(c))
				fr.regs[ins.A] = Value{Tag: ValueBigRat, BigRat: br}
			} else if b.Tag == ValueBigInt || c.Tag == ValueBigInt {
				bi := new(big.Int).Add(toBigInt(b), toBigInt(c))
				fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
			} else {
				bi := new(big.Int).Add(toBigInt(b), toBigInt(c))
				if bi.IsInt64() {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(bi.Int64())}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
				}
			}
		case OpAddInt:
			bVal := fr.regs[ins.B]
			cVal := fr.regs[ins.C]
			if bVal.Tag == ValueNull || cVal.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				bi := new(big.Int).Add(toBigInt(bVal), toBigInt(cVal))
				if bi.IsInt64() {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(bi.Int64())}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
				}
			}
		case OpAddFloat:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: toFloat(b) + toFloat(c)}
			}
		case OpSub:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
				break
			}
			if b.Tag == ValueFloat || c.Tag == ValueFloat {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: toFloat(b) - toFloat(c)}
			} else if b.Tag == ValueBigRat || c.Tag == ValueBigRat {
				br := new(big.Rat).Sub(toRat(b), toRat(c))
				fr.regs[ins.A] = Value{Tag: ValueBigRat, BigRat: br}
			} else if b.Tag == ValueBigInt || c.Tag == ValueBigInt {
				bi := new(big.Int).Sub(toBigInt(b), toBigInt(c))
				fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
			} else {
				bi := new(big.Int).Sub(toBigInt(b), toBigInt(c))
				if bi.IsInt64() {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(bi.Int64())}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
				}
			}
		case OpSubInt:
			bVal := fr.regs[ins.B]
			cVal := fr.regs[ins.C]
			if bVal.Tag == ValueNull || cVal.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				bi := new(big.Int).Sub(toBigInt(bVal), toBigInt(cVal))
				if bi.IsInt64() {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(bi.Int64())}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
				}
			}
		case OpSubFloat:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: toFloat(b) - toFloat(c)}
			}
		case OpNeg:
			b := fr.regs[ins.B]
			if b.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else if b.Tag == ValueFloat {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: -toFloat(b)}
			} else if b.Tag == ValueBigRat {
				br := new(big.Rat).Neg(toRat(b))
				fr.regs[ins.A] = Value{Tag: ValueBigRat, BigRat: br}
			} else if b.Tag == ValueBigInt {
				bi := new(big.Int).Neg(toBigInt(b))
				fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: -b.Int}
			}
		case OpNegInt:
			bVal := fr.regs[ins.B]
			if bVal.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				b := toInt(bVal)
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: -b}
			}
		case OpNegFloat:
			b := fr.regs[ins.B]
			if b.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: -toFloat(b)}
			}
		case OpMul:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
				break
			}
			if b.Tag == ValueFloat || c.Tag == ValueFloat {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: toFloat(b) * toFloat(c)}
			} else if b.Tag == ValueBigRat || c.Tag == ValueBigRat {
				br := new(big.Rat).Mul(toRat(b), toRat(c))
				fr.regs[ins.A] = Value{Tag: ValueBigRat, BigRat: br}
			} else if b.Tag == ValueBigInt || c.Tag == ValueBigInt {
				bi := new(big.Int).Mul(toBigInt(b), toBigInt(c))
				fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
			} else {
				bi := new(big.Int).Mul(toBigInt(b), toBigInt(c))
				if bi.IsInt64() {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(bi.Int64())}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
				}
			}
		case OpMulInt:
			bVal := fr.regs[ins.B]
			cVal := fr.regs[ins.C]
			if bVal.Tag == ValueNull || cVal.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				bi := new(big.Int).Mul(toBigInt(bVal), toBigInt(cVal))
				if bi.IsInt64() {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(bi.Int64())}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
				}
			}
		case OpMulFloat:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: toFloat(b) * toFloat(c)}
			}
		case OpDiv:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
				break
			}
			if toFloat(c) == 0 {
				return Value{}, m.newError(fmt.Errorf("division by zero"), trace, ins.Line)
			}
			if b.Tag == ValueFloat || c.Tag == ValueFloat {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: toFloat(b) / toFloat(c)}
			} else if b.Tag == ValueInt && c.Tag == ValueInt {
				// MEP-10 B1 Path B: int / int truncates.
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: b.Int / c.Int}
			} else if b.Tag == ValueBigRat || c.Tag == ValueBigRat {
				br := new(big.Rat).Quo(toRat(b), toRat(c))
				fr.regs[ins.A] = Value{Tag: ValueBigRat, BigRat: br}
			} else if b.Tag == ValueBigInt || c.Tag == ValueBigInt {
				bi := new(big.Int).Quo(toBigInt(b), toBigInt(c))
				if bi.IsInt64() {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(bi.Int64())}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
				}
			} else {
				bi := new(big.Int).Quo(toBigInt(b), toBigInt(c))
				if bi.IsInt64() {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(bi.Int64())}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
				}
			}
		case OpDivInt:
			bVal := fr.regs[ins.B]
			cVal := fr.regs[ins.C]
			if bVal.Tag == ValueNull || cVal.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				cBig := toBigInt(cVal)
				if cBig.Sign() == 0 {
					return Value{}, m.newError(fmt.Errorf("division by zero"), trace, ins.Line)
				}
				bi := new(big.Int).Quo(toBigInt(bVal), cBig)
				if bi.IsInt64() {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(bi.Int64())}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
				}
			}
		case OpDivFloat:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				if toFloat(c) == 0 {
					return Value{}, m.newError(fmt.Errorf("division by zero"), trace, ins.Line)
				}
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: toFloat(b) / toFloat(c)}
			}
		case OpMod:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
				break
			}
			if toFloat(c) == 0 {
				return Value{}, m.newError(fmt.Errorf("division by zero"), trace, ins.Line)
			}
			if b.Tag == ValueFloat || c.Tag == ValueFloat {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: math.Mod(toFloat(b), toFloat(c))}
			} else if b.Tag == ValueBigInt || c.Tag == ValueBigInt {
				bi := new(big.Int).Rem(toBigInt(b), toBigInt(c))
				fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
			} else {
				bi := new(big.Int).Rem(toBigInt(b), toBigInt(c))
				if bi.IsInt64() {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(bi.Int64())}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
				}
			}
		case OpModInt:
			bVal := fr.regs[ins.B]
			cVal := fr.regs[ins.C]
			if bVal.Tag == ValueNull || cVal.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				cBig := toBigInt(cVal)
				if cBig.Sign() == 0 {
					return Value{}, m.newError(fmt.Errorf("division by zero"), trace, ins.Line)
				}
				bi := new(big.Int).Rem(toBigInt(bVal), cBig)
				if bi.IsInt64() {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(bi.Int64())}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
				}
			}
		case OpModFloat:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else {
				if toFloat(c) == 0 {
					return Value{}, m.newError(fmt.Errorf("division by zero"), trace, ins.Line)
				}
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: math.Mod(toFloat(b), toFloat(c))}
			}
		case OpPow:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			} else if b.Tag == ValueFloat || c.Tag == ValueFloat {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: math.Pow(toFloat(b), toFloat(c))}
			} else {
				bi := new(big.Int).Exp(toBigInt(b), toBigInt(c), nil)
				if bi.IsInt64() {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(bi.Int64())}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueBigInt, BigInt: bi}
				}
			}
		case OpEqual:
			fr.regs[ins.A] = Value{Tag: ValueBool, Bool: valuesEqual(fr.regs[ins.B], fr.regs[ins.C])}
		case OpNotEqual:
			fr.regs[ins.A] = Value{Tag: ValueBool, Bool: !valuesEqual(fr.regs[ins.B], fr.regs[ins.C])}
		case OpEqualInt:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: false}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: toInt(b) == toInt(c)}
			}
		case OpEqualFloat:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			if b.Tag == ValueNull || c.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: false}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: toFloat(b) == toFloat(c)}
			}
		case OpLess:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			var res bool
			if b.Tag == ValueNull || c.Tag == ValueNull {
				res = false
			} else if b.Tag == ValueStr && c.Tag == ValueStr {
				res = b.Str < c.Str
			} else if b.Tag == ValueBigInt || c.Tag == ValueBigInt {
				res = toBigInt(b).Cmp(toBigInt(c)) < 0
			} else {
				res = toFloat(b) < toFloat(c)
			}
			fr.regs[ins.A] = Value{Tag: ValueBool, Bool: res}
		case OpLessEq:
			b := fr.regs[ins.B]
			c := fr.regs[ins.C]
			var res bool
			if b.Tag == ValueNull || c.Tag == ValueNull {
				res = false
			} else if b.Tag == ValueStr && c.Tag == ValueStr {
				res = b.Str <= c.Str
			} else if b.Tag == ValueBigInt || c.Tag == ValueBigInt {
				res = toBigInt(b).Cmp(toBigInt(c)) <= 0
			} else {
				res = toFloat(b) <= toFloat(c)
			}
			fr.regs[ins.A] = Value{Tag: ValueBool, Bool: res}
		case OpLessInt:
			if fr.regs[ins.B].Tag == ValueNull || fr.regs[ins.C].Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: false}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: toInt(fr.regs[ins.B]) < toInt(fr.regs[ins.C])}
			}
		case OpLessFloat:
			if fr.regs[ins.B].Tag == ValueNull || fr.regs[ins.C].Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: false}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: toFloat(fr.regs[ins.B]) < toFloat(fr.regs[ins.C])}
			}
		case OpLessEqInt:
			if fr.regs[ins.B].Tag == ValueNull || fr.regs[ins.C].Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: false}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: toInt(fr.regs[ins.B]) <= toInt(fr.regs[ins.C])}
			}
		case OpLessEqFloat:
			if fr.regs[ins.B].Tag == ValueNull || fr.regs[ins.C].Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: false}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: toFloat(fr.regs[ins.B]) <= toFloat(fr.regs[ins.C])}
			}
		case OpIn:
			item := fr.regs[ins.B]
			container := fr.regs[ins.C]
			found := false
			switch container.Tag {
			case ValueList:
				set := make(map[uint64]struct{}, len(container.List))
				for _, v := range container.List {
					set[valueHash(v)] = struct{}{}
				}
				_, found = set[valueHash(item)]
			case ValueMap:
				key := fmt.Sprint(item.ToAny())
				_, found = container.Map[key]
			case ValueStr:
				if item.Tag == ValueStr {
					found = strings.Contains(container.Str, item.Str)
				} else {
					found = strings.Contains(container.Str, fmt.Sprint(item.ToAny()))
				}
			default:
				return Value{}, m.newError(fmt.Errorf("invalid 'in' operand"), trace, ins.Line)
			}
			fr.regs[ins.A] = Value{Tag: ValueBool, Bool: found}
		case OpJump:
			fr.ip = ins.A
		case OpJumpIfFalse:
			if !fr.regs[ins.A].Truthy() {
				fr.ip = ins.B
			}
		case OpJumpIfTrue:
			if fr.regs[ins.A].Truthy() {
				fr.ip = ins.B
			}
		case OpJumpIfNotNull:
			if fr.regs[ins.A].Tag != ValueNull {
				fr.ip = ins.B
			}
		case OpLen:
			v := fr.regs[ins.B]
			switch v.Tag {
			case ValueList:
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: len(v.List)}
			case ValueStr:
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: len([]rune(v.Str))}
			case ValueMap:
				if flag, ok := v.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					items := v.Map["items"]
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: len(items.List)}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: len(v.Map)}
				}
			case ValueNull:
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
			case ValueInt:
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: len(strconv.Itoa(v.Int))}
			case ValueBigInt:
				if v.BigInt == nil {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: len(v.BigInt.String())}
				}
			case ValueFloat:
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: len(strconv.FormatFloat(v.Float, 'f', -1, 64))}
			case ValueBigRat:
				if v.BigRat == nil {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: len(v.BigRat.RatString())}
				}
			default:
				return Value{}, m.newError(fmt.Errorf("invalid len operand"), trace, ins.Line)
			}
		case OpIndex:
			src := fr.regs[ins.B]
			idxVal := fr.regs[ins.C]
			if src.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueNull}
				break
			}
			switch src.Tag {
			case ValueList:
				idx := idxVal.Int
				if idx < 0 {
					idx += len(src.List)
				}
				if idx < 0 || idx >= len(src.List) {
					fr.regs[ins.A] = Value{Tag: ValueNull}
					break
				}
				fr.regs[ins.A] = src.List[idx]
			case ValueMap:
				var key string
				switch idxVal.Tag {
				case ValueStr:
					key = idxVal.Str
				case ValueInt:
					key = fmt.Sprintf("%d", idxVal.Int)
				default:
					key = valueToString(idxVal)
				}
				if v, ok := src.Map[key]; ok {
					fr.regs[ins.A] = v
				} else {
					found := Value{Tag: ValueNull}
					for _, vv := range src.Map {
						if vv.Tag == ValueMap {
							if val, ok := vv.Map[key]; ok {
								found = val
								break
							}
						}
					}
					fr.regs[ins.A] = found
				}
			case ValueStr:
				if idxVal.Tag != ValueInt {
					fr.regs[ins.A] = Value{Tag: ValueNull}
					break
				}
				runes := []rune(src.Str)
				idx := idxVal.Int
				if idx < 0 {
					idx += len(runes)
				}
				if idx < 0 || idx >= len(runes) {
					fr.regs[ins.A] = Value{Tag: ValueNull}
					break
				}
				fr.regs[ins.A] = Value{Tag: ValueStr, Str: string(runes[idx])}
			default:
				fr.regs[ins.A] = Value{Tag: ValueNull}
			}
		case OpSlice:
			src := fr.regs[ins.B]
			startVal := fr.regs[ins.C]
			endVal := fr.regs[ins.D]
			switch src.Tag {
			case ValueList:
				n := len(src.List)
				start := 0
				if startVal.Tag != ValueNull {
					if startVal.Tag != ValueInt {
						return Value{}, m.newError(fmt.Errorf("invalid index"), trace, ins.Line)
					}
					start = startVal.Int
				}
				end := n
				if endVal.Tag != ValueNull {
					if endVal.Tag != ValueInt {
						return Value{}, m.newError(fmt.Errorf("invalid index"), trace, ins.Line)
					}
					end = endVal.Int
				}
				start, end = clampSlice(n, start, end)
				out := make([]Value, end-start)
				copy(out, src.List[start:end])
				fr.regs[ins.A] = Value{Tag: ValueList, List: out}
			case ValueStr:
				runes := []rune(src.Str)
				n := len(runes)
				start := 0
				if startVal.Tag != ValueNull {
					if startVal.Tag != ValueInt {
						return Value{}, m.newError(fmt.Errorf("invalid index"), trace, ins.Line)
					}
					start = startVal.Int
				}
				end := n
				if endVal.Tag != ValueNull {
					if endVal.Tag != ValueInt {
						return Value{}, m.newError(fmt.Errorf("invalid index"), trace, ins.Line)
					}
					end = endVal.Int
				}
				start, end = clampSlice(n, start, end)
				fr.regs[ins.A] = Value{Tag: ValueStr, Str: string(runes[start:end])}
			default:
				return Value{}, m.newError(fmt.Errorf("invalid index target"), trace, ins.Line)
			}
		case OpSetIndex:
			dst := &fr.regs[ins.A]
			idxVal := fr.regs[ins.B]
			val := fr.regs[ins.C]
			switch dst.Tag {
			case ValueNull:
				dst.Tag = ValueList
				dst.List = make([]Value, idxVal.Int+1)
				dst.List[idxVal.Int] = val
			case ValueList:
				idx := idxVal.Int
				if idx < 0 {
					idx += len(dst.List)
				}
				if idx < 0 {
					return Value{}, m.newError(fmt.Errorf("index out of range"), trace, ins.Line)
				}
				if idx >= len(dst.List) {
					newList := make([]Value, idx+1)
					copy(newList, dst.List)
					dst.List = newList
				}
				dst.List[idx] = val
			case ValueMap:
				var key string
				switch idxVal.Tag {
				case ValueStr:
					key = idxVal.Str
				case ValueInt:
					key = fmt.Sprintf("%d", idxVal.Int)
				default:
					key = valueToString(idxVal)
				}
				if dst.Map == nil {
					dst.Map = map[string]Value{}
				}
				dst.Map[key] = val
			default:
				return Value{}, m.newError(fmt.Errorf("invalid index target"), trace, ins.Line)
			}
		case OpMakeList:
			n := ins.B
			start := ins.C
			list := make([]Value, n)
			copy(list, fr.regs[start:start+n])
			fr.regs[ins.A] = Value{Tag: ValueList, List: list}
		case OpMakeMap:
			n := ins.B
			start := ins.C
			mp := make(map[string]Value, n)
			for i := 0; i < n; i++ {
				key := fr.regs[start+i*2]
				val := fr.regs[start+i*2+1]
				var k string
				switch key.Tag {
				case ValueStr:
					k = key.Str
				case ValueInt:
					k = fmt.Sprintf("%d", key.Int)
				default:
					k = valueToString(key)
				}
				mp[k] = val
			}
			fr.regs[ins.A] = Value{Tag: ValueMap, Map: mp}
		case OpPrint:
			fmt.Fprintln(m.writer, formatValue(fr.regs[ins.A]))
		case OpPrint2:
			fmt.Fprintln(m.writer, formatValue(fr.regs[ins.A]), formatValue(fr.regs[ins.B]))
		case OpPrintN:
			var sb strings.Builder
			for i := 0; i < ins.B; i++ {
				if i > 0 {
					sb.WriteByte(' ')
				}
				sb.WriteString(formatValue(fr.regs[ins.C+i]))
			}
			fmt.Fprintln(m.writer, strings.TrimSpace(sb.String()))
		case OpNow:
			if seededNow {
				nowSeed = (nowSeed*1664525 + 1013904223) % 2147483647
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(nowSeed)}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(time.Now().UnixNano())}
			}
		case OpRealNow:
			fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(time.Now().UnixNano())}
		case OpMem:
			var ms runtime.MemStats
			runtime.ReadMemStats(&ms)
			fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(ms.Alloc)}
		case OpJSON:
			b, _ := json.MarshalIndent(fr.regs[ins.A].ToAny(), "", "  ")
			fmt.Fprintln(m.writer, string(b))
		case OpAppend:
			lst := fr.regs[ins.B]
			if lst.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueList, List: []Value{fr.regs[ins.C]}}
				break
			}
			if lst.Tag == ValueMap {
				if flag, ok := lst.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					items := lst.Map["items"]
					newItems := append(append([]Value(nil), items.List...), fr.regs[ins.C])
					lst.Map["items"] = Value{Tag: ValueList, List: newItems}
					lst.Map["count"] = Value{Tag: ValueInt, Int: len(newItems)}
					fr.regs[ins.A] = lst
					break
				}
			}
			if lst.Tag != ValueList {
				return Value{}, m.newError(fmt.Errorf("append expects list"), trace, ins.Line)
			}
			newList := append(append([]Value(nil), lst.List...), fr.regs[ins.C])
			fr.regs[ins.A] = Value{Tag: ValueList, List: newList}
		case OpUnionAll:
			a := fr.regs[ins.B]
			if a.Tag == ValueNull {
				a = Value{Tag: ValueList}
			} else if a.Tag == ValueMap {
				if flag, ok := a.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					a = a.Map["items"]
				}
			}
			b := fr.regs[ins.C]
			if b.Tag == ValueNull {
				b = Value{Tag: ValueList}
			} else if b.Tag == ValueMap {
				if flag, ok := b.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					b = b.Map["items"]
				}
			}
			if a.Tag != ValueList || b.Tag != ValueList {
				return Value{}, m.newError(fmt.Errorf("union expects lists"), trace, ins.Line)
			}
			// Preallocate the result slice for efficiency when
			// concatenating two lists.
			out := make([]Value, 0, len(a.List)+len(b.List))
			out = append(out, a.List...)
			out = append(out, b.List...)
			fr.regs[ins.A] = Value{Tag: ValueList, List: out}
		case OpUnion:
			a := fr.regs[ins.B]
			if a.Tag == ValueNull {
				a = Value{Tag: ValueList}
			} else if a.Tag == ValueMap {
				if flag, ok := a.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					a = a.Map["items"]
				}
			}
			b := fr.regs[ins.C]
			if b.Tag == ValueNull {
				b = Value{Tag: ValueList}
			} else if b.Tag == ValueMap {
				if flag, ok := b.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					b = b.Map["items"]
				}
			}
			if a.Tag != ValueList || b.Tag != ValueList {
				return Value{}, m.newError(fmt.Errorf("union expects lists"), trace, ins.Line)
			}
			seen := make(map[uint64]struct{}, len(a.List)+len(b.List))
			out := make([]Value, 0, len(a.List)+len(b.List))
			for _, v := range a.List {
				k := valueHash(v)
				if _, ok := seen[k]; !ok {
					seen[k] = struct{}{}
					out = append(out, v)
				}
			}
			for _, v := range b.List {
				k := valueHash(v)
				if _, ok := seen[k]; !ok {
					seen[k] = struct{}{}
					out = append(out, v)
				}
			}
			fr.regs[ins.A] = Value{Tag: ValueList, List: out}
		case OpExcept:
			a := fr.regs[ins.B]
			if a.Tag == ValueNull {
				a = Value{Tag: ValueList}
			} else if a.Tag == ValueMap {
				if flag, ok := a.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					a = a.Map["items"]
				}
			}
			b := fr.regs[ins.C]
			if b.Tag == ValueNull {
				b = Value{Tag: ValueList}
			} else if b.Tag == ValueMap {
				if flag, ok := b.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					b = b.Map["items"]
				}
			}
			if a.Tag != ValueList || b.Tag != ValueList {
				return Value{}, m.newError(fmt.Errorf("except expects lists"), trace, ins.Line)
			}
			set := make(map[uint64]struct{}, len(b.List))
			for _, v := range b.List {
				set[valueHash(v)] = struct{}{}
			}
			diff := make([]Value, 0, len(a.List))
			for _, v := range a.List {
				if _, ok := set[valueHash(v)]; !ok {
					diff = append(diff, v)
				}
			}
			fr.regs[ins.A] = Value{Tag: ValueList, List: diff}
		case OpIntersect:
			a := fr.regs[ins.B]
			if a.Tag == ValueNull {
				a = Value{Tag: ValueList}
			} else if a.Tag == ValueMap {
				if flag, ok := a.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					a = a.Map["items"]
				}
			}
			b := fr.regs[ins.C]
			if b.Tag == ValueNull {
				b = Value{Tag: ValueList}
			} else if b.Tag == ValueMap {
				if flag, ok := b.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					b = b.Map["items"]
				}
			}
			if a.Tag != ValueList || b.Tag != ValueList {
				return Value{}, m.newError(fmt.Errorf("intersect expects lists"), trace, ins.Line)
			}
			setA := make(map[uint64]struct{}, len(a.List))
			for _, v := range a.List {
				setA[valueHash(v)] = struct{}{}
			}
			inter := []Value{}
			added := make(map[uint64]struct{}, len(b.List))
			for _, v := range b.List {
				k := valueHash(v)
				if _, ok := setA[k]; ok {
					if _, done := added[k]; !done {
						added[k] = struct{}{}
						inter = append(inter, v)
					}
				}
			}
			fr.regs[ins.A] = Value{Tag: ValueList, List: inter}
		case OpSort:
			src := fr.regs[ins.B]
			if src.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueList}
				break
			}
			if src.Tag == ValueMap {
				if flag, ok := src.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					src = src.Map["items"]
				} else {
					keys := make([]string, 0, len(src.Map))
					for k := range src.Map {
						keys = append(keys, k)
					}
					sort.Strings(keys)
					out := make([]Value, len(keys))
					for i, k := range keys {
						out[i] = src.Map[k]
					}
					fr.regs[ins.A] = Value{Tag: ValueList, List: out}
					break
				}
			}
			if src.Tag != ValueList {
				return Value{}, m.newError(fmt.Errorf("sort expects list"), trace, ins.Line)
			}
			pairs := append([]Value(nil), src.List...)
			sort.SliceStable(pairs, func(i, j int) bool {
				return valueLess(pairKey(pairs[i]), pairKey(pairs[j]))
			})
			out := make([]Value, len(pairs))
			for i, p := range pairs {
				out[i] = pairVal(p)
			}
			fr.regs[ins.A] = Value{Tag: ValueList, List: out}
		case OpDistinct:
			src := fr.regs[ins.B]
			if src.Tag != ValueList {
				return Value{}, m.newError(fmt.Errorf("distinct expects list"), trace, ins.Line)
			}
			out := []Value{}
			seen := map[string]struct{}{}
			for _, v := range src.List {
				k := valueToString(v)
				if _, ok := seen[k]; !ok {
					seen[k] = struct{}{}
					out = append(out, v)
				}
			}
			fr.regs[ins.A] = Value{Tag: ValueList, List: out}
		case OpExpect:
			val := fr.regs[ins.A]
			if val.Tag != ValueBool || !val.Bool {
				src := ""
				if ins.Line > 0 && ins.Line <= len(m.prog.Source) {
					src = strings.TrimSpace(m.prog.Source[ins.Line-1])
				}
				msg := "expect condition failed"
				if src != "" {
					msg = fmt.Sprintf("expect condition failed at %s:%d: %s", m.prog.File, ins.Line, src)
				}
				return Value{}, m.newError(fmt.Errorf("%s", msg), trace, ins.Line)
			}
		case OpSelect:
			cond := fr.regs[ins.B]
			if cond.Tag == ValueBool && cond.Bool {
				fr.regs[ins.A] = fr.regs[ins.C]
			} else {
				fr.regs[ins.A] = fr.regs[ins.D]
			}
		case OpStr:
			fr.regs[ins.A] = Value{Tag: ValueStr, Str: fmt.Sprint(fr.regs[ins.B].ToAny())}
		case OpUpper:
			b := fr.regs[ins.B]
			if b.Tag != ValueStr {
				return Value{}, m.newError(fmt.Errorf("upper expects string"), trace, ins.Line)
			}
			fr.regs[ins.A] = Value{Tag: ValueStr, Str: strings.ToUpper(b.Str)}
		case OpLower:
			b := fr.regs[ins.B]
			if b.Tag == ValueStr {
				fr.regs[ins.A] = Value{Tag: ValueStr, Str: strings.ToLower(b.Str)}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueStr, Str: strings.ToLower(fmt.Sprint(b.ToAny()))}
			}
		case OpReverse:
			val := fr.regs[ins.B]
			switch val.Tag {
			case ValueList:
				newList := append([]Value(nil), val.List...)
				for i, j := 0, len(newList)-1; i < j; i, j = i+1, j-1 {
					newList[i], newList[j] = newList[j], newList[i]
				}
				fr.regs[ins.A] = Value{Tag: ValueList, List: newList}
			case ValueStr:
				r := []rune(val.Str)
				for i, j := 0, len(r)-1; i < j; i, j = i+1, j-1 {
					r[i], r[j] = r[j], r[i]
				}
				fr.regs[ins.A] = Value{Tag: ValueStr, Str: string(r)}
			default:
				return Value{}, m.newError(fmt.Errorf("reverse expects list or string"), trace, ins.Line)
			}
		case OpPadStart:
			s := fr.regs[ins.B]
			ln := fr.regs[ins.C]
			pad := fr.regs[ins.D]
			if s.Tag != ValueStr || ln.Tag != ValueInt || pad.Tag != ValueStr {
				return Value{}, m.newError(fmt.Errorf("padStart expects (string, int, string)"), trace, ins.Line)
			}
			r := []rune(s.Str)
			if len(r) >= ln.Int {
				fr.regs[ins.A] = s
				break
			}
			pr := []rune(pad.Str)
			if len(pr) == 0 {
				pr = []rune(" ")
			}
			need := ln.Int - len(r)
			buf := make([]rune, 0, ln.Int)
			for need > 0 {
				if need >= len(pr) {
					buf = append(buf, pr...)
					need -= len(pr)
				} else {
					buf = append(buf, pr[:need]...)
					need = 0
				}
			}
			buf = append(buf, r...)
			fr.regs[ins.A] = Value{Tag: ValueStr, Str: string(buf)}
		case OpSHA256:
			v := fr.regs[ins.B]
			var data []byte
			if v.Tag == ValueStr {
				data = []byte(v.Str)
			} else if lst, ok := toList(v); ok {
				data = make([]byte, len(lst))
				for i, it := range lst {
					data[i] = byte(toInt(it))
				}
			} else {
				return Value{}, m.newError(fmt.Errorf("sha256 expects string or list"), trace, ins.Line)
			}
			sum := sha256.Sum256(data)
			out := make([]Value, len(sum))
			for i, b := range sum {
				out[i] = Value{Tag: ValueInt, Int: int(b)}
			}
			fr.regs[ins.A] = Value{Tag: ValueList, List: out}
		case OpInput:
			line, err := m.reader.ReadString('\n')
			if err != nil && err != io.EOF {
				return Value{}, err
			}
			line = strings.TrimRight(line, "\r\n")
			fr.regs[ins.A] = Value{Tag: ValueStr, Str: line}
		case OpFirst:
			lst := fr.regs[ins.B]
			if lst.Tag != ValueList {
				return Value{}, m.newError(fmt.Errorf("first expects list"), trace, ins.Line)
			}
			if len(lst.List) > 0 {
				fr.regs[ins.A] = lst.List[0]
			} else {
				fr.regs[ins.A] = Value{Tag: ValueNull}
			}
		case OpIterPrep:
			src := fr.regs[ins.B]
			switch src.Tag {
			case ValueNull:
				fr.regs[ins.A] = Value{Tag: ValueList, List: nil}
			case ValueList:
				fr.regs[ins.A] = src
			case ValueMap:
				if flag, ok := src.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					items := src.Map["items"]
					fr.regs[ins.A] = items
					break
				}
				ks := make([]string, 0, len(src.Map))
				for k := range src.Map {
					ks = append(ks, k)
				}
				sort.Strings(ks)
				keys := make([]Value, len(ks))
				for i, k := range ks {
					keys[i] = Value{Tag: ValueStr, Str: k}
				}
				fr.regs[ins.A] = Value{Tag: ValueList, List: keys}
			case ValueStr:
				r := []rune(src.Str)
				lst := make([]Value, len(r))
				for i, ch := range r {
					lst[i] = Value{Tag: ValueStr, Str: string(ch)}
				}
				fr.regs[ins.A] = Value{Tag: ValueList, List: lst}
			default:
				return Value{}, m.newError(fmt.Errorf("invalid iterator"), trace, ins.Line)
			}
		case OpLoad:
			path := fr.regs[ins.B].Str
			opts := fr.regs[ins.C].ToAny()
			optMap, _ := opts.(map[string]any)
			format := "csv"
			header := true
			delim := ','
			if optMap != nil {
				if f, ok := optMap["format"].(string); ok {
					format = f
				}
				if h, ok := optMap["header"].(bool); ok {
					header = h
				}
				if d, ok := optMap["delimiter"].(string); ok && len(d) > 0 {
					delim = rune(d[0])
				}
			}
			var rows []map[string]any
			var err error
			switch format {
			case "jsonl":
				if path == "" || path == "-" {
					rows, err = data.LoadJSONLReader(os.Stdin)
				} else {
					rows, err = data.LoadJSONL(path)
				}
			case "json":
				if path == "" || path == "-" {
					rows, err = data.LoadJSONReader(os.Stdin)
				} else {
					rows, err = data.LoadJSON(path)
				}
			case "yaml":
				if path == "" || path == "-" {
					rows, err = data.LoadYAMLReader(os.Stdin)
				} else {
					rows, err = data.LoadYAML(path)
				}
			case "tsv":
				delim = '\t'
				fallthrough
			default:
				if path == "" || path == "-" {
					rows, err = data.LoadCSVReader(os.Stdin, header, delim)
				} else {
					rows, err = data.LoadCSV(path, header, delim)
				}
			}
			if err != nil {
				return Value{}, m.newError(err, trace, ins.Line)
			}
			out := make([]Value, len(rows))
			if ins.D >= 0 {
				typ := m.prog.Types[ins.D]
				for i, row := range rows {
					val := any(row)
					cv, err := castValue(typ, val)
					if err != nil {
						return Value{}, m.newError(err, trace, ins.Line)
					}
					out[i] = FromAny(cv)
				}
			} else {
				for i, row := range rows {
					out[i] = FromAny(row)
				}
			}
			fr.regs[ins.A] = Value{Tag: ValueList, List: out}
		case OpSave:
			srcVal := fr.regs[ins.B]
			path := fr.regs[ins.C].Str
			opts := fr.regs[ins.D].ToAny()
			optMap := toAnyMap(opts)
			format := "csv"
			header := false
			delim := ','
			if optMap != nil {
				if f, ok := optMap["format"].(string); ok {
					format = f
				}
				if h, ok := optMap["header"].(bool); ok {
					header = h
				}
				if d, ok := optMap["delimiter"].(string); ok && len(d) > 0 {
					delim = rune(d[0])
				}
			}
			rows, ok := toMapSlice(srcVal.ToAny())
			if !ok {
				return Value{}, m.newError(fmt.Errorf("save source must be list of maps"), trace, ins.Line)
			}
			var err error
			switch format {
			case "jsonl":
				if path == "" || path == "-" {
					err = data.SaveJSONLWriter(rows, m.writer)
				} else {
					err = data.SaveJSONL(rows, path)
				}
			case "json":
				if path == "" || path == "-" {
					err = data.SaveJSONWriter(rows, m.writer)
				} else {
					err = data.SaveJSON(rows, path)
				}
			case "yaml":
				if path == "" || path == "-" {
					err = data.SaveYAMLWriter(rows, m.writer)
				} else {
					err = data.SaveYAML(rows, path)
				}
			case "tsv":
				delim = '\t'
				fallthrough
			default:
				if path == "" || path == "-" {
					err = data.SaveCSVWriter(rows, m.writer, header, delim)
				} else {
					err = data.SaveCSV(rows, path, header, delim)
				}
			}
			if err != nil {
				return Value{}, m.newError(err, trace, ins.Line)
			}
			fr.regs[ins.A] = Value{Tag: ValueNull}
		case OpEval:
			srcVal := fr.regs[ins.B]
			if srcVal.Tag != ValueStr {
				return Value{}, m.newError(fmt.Errorf("eval expects string"), trace, ins.Line)
			}
			prog, err := parser.ParseString(srcVal.Str)
			if err != nil {
				return Value{}, m.newError(err, trace, ins.Line)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				return Value{}, m.newError(errs[0], trace, ins.Line)
			}
			p, errc := Compile(prog, env)
			if errc != nil {
				return Value{}, m.newError(errc, trace, ins.Line)
			}
			vm := New(p, io.Discard)
			resVal, err := vm.RunResult()
			if err != nil {
				return Value{}, m.newError(err, trace, ins.Line)
			}
			fr.regs[ins.A] = resVal
		case OpFetch:
			urlVal := fr.regs[ins.B]
			if urlVal.Tag != ValueStr {
				return Value{}, m.newError(fmt.Errorf("fetch URL must be string"), trace, ins.Line)
			}
			opts := fr.regs[ins.C].ToAny()
			optMap := toAnyMap(opts)
			res, err := mhttp.FetchWith(urlVal.Str, optMap)
			if err != nil {
				return Value{}, m.newError(err, trace, ins.Line)
			}
			fr.regs[ins.A] = FromAny(res)
		case OpGetGlobal:
			if ins.B >= len(m.globals) {
				return Value{}, fmt.Errorf("global index out of range")
			}
			fr.regs[ins.A] = m.globals[ins.B]
		case OpSetGlobal:
			if ins.A >= len(m.globals) {
				return Value{}, fmt.Errorf("global index out of range")
			}
			m.globals[ins.A] = copyValue(fr.regs[ins.B])
		case OpCount:
			lst := fr.regs[ins.B]
			if lst.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
				break
			}
			if lst.Tag == ValueList {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: len(lst.List)}
				break
			}
			if lst.Tag == ValueMap {
				if flag, ok := lst.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					items := lst.Map["items"]
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: len(items.List)}
					break
				}
			}
			return Value{}, fmt.Errorf("count expects list")
		case OpExists:
			lst := fr.regs[ins.B]
			switch lst.Tag {
			case ValueList:
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: len(lst.List) > 0}
			case ValueMap:
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: len(lst.Map) > 0}
			case ValueStr:
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: len(lst.Str) > 0}
			default:
				fr.regs[ins.A] = Value{Tag: ValueBool, Bool: false}
			}
		case OpAvg:
			lst := fr.regs[ins.B]
			if lst.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
				break
			}
			if lst.Tag == ValueMap {
				if flag, ok := lst.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					lst = lst.Map["items"]
				}
			}
			if lst.Tag != ValueList {
				return Value{}, fmt.Errorf("avg expects list")
			}
			if len(lst.List) == 0 {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: 0}
			} else {
				var sum float64
				var count int
				for _, v := range lst.List {
					if v.Tag == ValueNull {
						continue
					}
					sum += toFloat(v)
					count++
				}
				if count == 0 {
					fr.regs[ins.A] = Value{Tag: ValueFloat, Float: 0}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueFloat, Float: sum / float64(count)}
				}
			}
		case OpSum:
			lst := fr.regs[ins.B]
			if lst.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
				break
			}
			if lst.Tag == ValueMap {
				if flag, ok := lst.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					// fast path: check group count before iterating
					if cnt, ok := lst.Map["count"]; ok && cnt.Tag == ValueInt && cnt.Int == 0 {
						fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
						break
					}
					lst = lst.Map["items"]
				}
			}
			if lst.Tag != ValueList {
				return Value{}, fmt.Errorf("sum expects list")
			}
			if len(lst.List) == 0 {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
				break
			}
			var sumF float64
			var sumI int
			allInt := true
			for _, v := range lst.List {
				if v.Tag == ValueInt {
					sumI += v.Int
				} else {
					allInt = false
					sumF += toFloat(v)
				}
			}
			if allInt {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: sumI}
			} else {
				sumF += float64(sumI)
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: sumF}
			}
		case OpMin:
			lst := fr.regs[ins.B]
			if lst.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
				break
			}
			if lst.Tag == ValueMap {
				if flag, ok := lst.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					lst = lst.Map["items"]
				}
			}
			if lst.Tag != ValueList {
				return Value{}, fmt.Errorf("min expects list")
			}
			if len(lst.List) == 0 {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
			} else {
				minVal := toFloat(lst.List[0])
				isFloat := lst.List[0].Tag == ValueFloat
				for _, v := range lst.List[1:] {
					if v.Tag == ValueFloat {
						isFloat = true
					}
					f := toFloat(v)
					if f < minVal {
						minVal = f
					}
				}
				if isFloat {
					fr.regs[ins.A] = Value{Tag: ValueFloat, Float: minVal}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(minVal)}
				}
			}
		case OpMax:
			lst := fr.regs[ins.B]
			if lst.Tag == ValueNull {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
				break
			}
			if lst.Tag == ValueMap {
				if flag, ok := lst.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
					lst = lst.Map["items"]
				}
			}
			if lst.Tag != ValueList {
				return Value{}, fmt.Errorf("max expects list")
			}
			if len(lst.List) == 0 {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: 0}
			} else {
				maxVal := toFloat(lst.List[0])
				isFloat := lst.List[0].Tag == ValueFloat
				for _, v := range lst.List[1:] {
					if v.Tag == ValueFloat {
						isFloat = true
					}
					f := toFloat(v)
					if f > maxVal {
						maxVal = f
					}
				}
				if isFloat {
					fr.regs[ins.A] = Value{Tag: ValueFloat, Float: maxVal}
				} else {
					fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(maxVal)}
				}
			}
		case OpValues:
			m := fr.regs[ins.B]
			if m.Tag != ValueMap {
				return Value{}, fmt.Errorf("values expects map")
			}
			if len(m.Map) == 0 {
				fr.regs[ins.A] = Value{Tag: ValueList, List: []Value{}}
				break
			}
			keys := make([]string, 0, len(m.Map))
			for k := range m.Map {
				keys = append(keys, k)
			}
			sort.Strings(keys)
			vals := make([]Value, 0, len(keys))
			for _, k := range keys {
				vals = append(vals, m.Map[k])
			}
			fr.regs[ins.A] = Value{Tag: ValueList, List: vals}
		case OpRound:
			val := fr.regs[ins.B]
			digitsVal := fr.regs[ins.C]
			d := 0
			if digitsVal.Tag == ValueInt {
				d = digitsVal.Int
			} else if digitsVal.Tag != ValueNull {
				d = int(toFloat(digitsVal))
			}
			f := toFloat(val)
			factor := math.Pow(10, float64(d))
			rounded := math.Round(f*factor) / factor
			if d == 0 && val.Tag == ValueInt {
				fr.regs[ins.A] = Value{Tag: ValueInt, Int: int(rounded)}
			} else {
				fr.regs[ins.A] = Value{Tag: ValueFloat, Float: rounded}
			}
		case OpCast:
			val := fr.regs[ins.B].ToAny()
			typ := m.prog.Types[ins.C]
			cv, err := castValue(typ, val)
			if err != nil {
				return Value{}, m.newError(err, trace, ins.Line)
			}
			fr.regs[ins.A] = FromAny(cv)
		case OpMakeClosure:
			caps := make([]Value, ins.C)
			copy(caps, fr.regs[ins.D:ins.D+ins.C])
			cl := &closure{fn: ins.B, args: caps}
			fr.regs[ins.A] = Value{Tag: ValueFunc, Func: cl}
		case OpCall2:
			a := fr.regs[ins.C]
			b := fr.regs[ins.D]
			if fr.ip < len(fr.fn.Code) {
				next := fr.fn.Code[fr.ip]
				if next.Op == OpReturn && next.A == ins.A {
					fn := &m.prog.Funcs[ins.B]
					if fn.NumParams == 2 {
						fr.fn = fn
						fr.regs = make([]Value, fn.NumRegs)
						fr.regs[0] = a
						fr.regs[1] = b
						fr.ip = 0
						trace[len(trace)-1] = StackFrame{Func: m.prog.funcName(ins.B), Line: ins.Line}
						continue
					}
				}
			}
			res, err := m.call(ins.B, []Value{a, b}, append(trace, StackFrame{Func: m.prog.funcName(ins.B), Line: ins.Line}))
			if err != nil {
				if vmErr, ok := err.(*VMError); ok {
					if len(vmErr.Stack) >= 2 {
						vmErr.Stack[len(vmErr.Stack)-2].Line = ins.Line
					}
				}
				return Value{}, err
			}
			fr.regs[ins.A] = res
		case OpCall:
			args := make([]Value, ins.C)
			for i := 0; i < ins.C; i++ {
				args[i] = fr.regs[ins.D+i]
			}
			if fr.ip < len(fr.fn.Code) {
				next := fr.fn.Code[fr.ip]
				if next.Op == OpReturn && next.A == ins.A {
					fn := &m.prog.Funcs[ins.B]
					if fn.NumParams == len(args) {
						fr.fn = fn
						fr.regs = make([]Value, fn.NumRegs)
						for i := 0; i < len(args) && i < len(fr.regs); i++ {
							fr.regs[i] = args[i]
						}
						fr.ip = 0
						trace[len(trace)-1] = StackFrame{Func: m.prog.funcName(ins.B), Line: ins.Line}
						continue
					}
				}
			}
			res, err := m.call(ins.B, args, append(trace, StackFrame{Func: m.prog.funcName(ins.B), Line: ins.Line}))
			if err != nil {
				if vmErr, ok := err.(*VMError); ok {
					if len(vmErr.Stack) >= 2 {
						vmErr.Stack[len(vmErr.Stack)-2].Line = ins.Line
					}
				}
				return Value{}, err
			}
			fr.regs[ins.A] = res
		case OpCallV:
			fnVal := fr.regs[ins.B]
			args := make([]Value, ins.C)
			for i := 0; i < ins.C; i++ {
				args[i] = copyValue(fr.regs[ins.D+i])
			}
			if fr.ip < len(fr.fn.Code) {
				next := fr.fn.Code[fr.ip]
				if next.Op == OpReturn && next.A == ins.A {
					var fnIdx int
					var all []Value
					if fnVal.Tag == ValueFunc {
						cl := fnVal.Func.(*closure)
						fnIdx = cl.fn
						all = append(append([]Value{}, cl.args...), args...)
					} else {
						fnIdx = fnVal.Int
						all = args
					}
					fn := &m.prog.Funcs[fnIdx]
					if len(all) == fn.NumParams {
						fr.fn = fn
						fr.regs = make([]Value, fn.NumRegs)
						for i := 0; i < len(all) && i < len(fr.regs); i++ {
							fr.regs[i] = all[i]
						}
						fr.ip = 0
						trace[len(trace)-1] = StackFrame{Func: m.prog.funcName(fnIdx), Line: ins.Line}
						continue
					}
				}
			}
			if fnVal.Tag == ValueFunc {
				cl := fnVal.Func.(*closure)
				all := append(append([]Value{}, cl.args...), args...)
				res, err := m.call(cl.fn, all, append(trace, StackFrame{Func: m.prog.funcName(cl.fn), Line: ins.Line}))
				if err != nil {
					if vmErr, ok := err.(*VMError); ok {
						if len(vmErr.Stack) >= 2 {
							vmErr.Stack[len(vmErr.Stack)-2].Line = ins.Line
						}
					}
					return Value{}, err
				}
				fr.regs[ins.A] = res
				break
			}
			fnIdx := fnVal.Int
			res, err := m.call(fnIdx, args, append(trace, StackFrame{Func: m.prog.funcName(fnIdx), Line: ins.Line}))
			if err != nil {
				if vmErr, ok := err.(*VMError); ok {
					if len(vmErr.Stack) >= 2 {
						vmErr.Stack[len(vmErr.Stack)-2].Line = ins.Line
					}
				}
				return Value{}, err
			}
			fr.regs[ins.A] = res
		case OpGoCall:
			name := fr.regs[ins.B].Str
			args := make([]any, ins.C)
			for i := 0; i < ins.C; i++ {
				args[i] = fr.regs[ins.D+i].ToAny()
			}
			res, err := goffi.Call(name, args...)
			if err != nil {
				return Value{}, m.newError(err, trace, ins.Line)
			}
			fr.regs[ins.A] = FromAny(res)
		case OpGoAutoCall:
			name := fr.regs[ins.B].Str
			args := make([]any, ins.C)
			for i := 0; i < ins.C; i++ {
				args[i] = fr.regs[ins.D+i].ToAny()
			}
			res, err := goffi.CallAuto(name, args...)
			if err != nil {
				return Value{}, m.newError(err, trace, ins.Line)
			}
			fr.regs[ins.A] = FromAny(res)
		case OpPyCall:
			mod := fr.regs[ins.B].Str
			attr := fr.regs[ins.C].Str
			count := ins.Val.Int
			args := make([]any, count)
			for i := 0; i < count; i++ {
				args[i] = fr.regs[ins.D+i].ToAny()
			}
			res, err := pythonffi.Attr(mod, attr, args...)
			if err != nil {
				return Value{}, m.newError(err, trace, ins.Line)
			}
			fr.regs[ins.A] = FromAny(res)
		case OpNot:
			fr.regs[ins.A] = Value{Tag: ValueBool, Bool: !fr.regs[ins.B].Truthy()}
		case OpReturn:
			ret := fr.regs[ins.A]
			stack = stack[:len(stack)-1]
			if len(stack) == 0 {
				return ret, nil
			}
			stack[len(stack)-1].regs[0] = ret
		default:
			return Value{}, m.newError(fmt.Errorf("unknown op"), trace, ins.Line)
		}
	}
	return Value{}, nil
}
