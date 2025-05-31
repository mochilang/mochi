package vm

import (
	"encoding/json"
	"fmt"
	"io"
	"mochi/compiler"
	"strings"
	"time"
)

const maxFrames = 100

type Frame struct {
	Fn      *compiler.Function
	IP      int
	Locals  map[string]any
	BasePtr int
	Closure *Closure // ← Add this line
}

type Closure struct {
	Fn       *compiler.Function
	Captured map[string]any
}

type VM struct {
	stack    []any
	sp       int
	frames   []*Frame
	frame    *Frame
	globals  map[string]any
	writer   io.Writer
	builtins map[string]Builtin
	Debug    bool // ✅ Add this line
}

func NewVM(w io.Writer) *VM {
	vm := &VM{
		stack:   make([]any, 0, 1024),
		globals: make(map[string]any),
		writer:  w,
	}
	vm.initBuiltins()
	return vm
}

func (vm *VM) debugf(format string, args ...any) {
	if !vm.Debug {
		return
	}
	fmt.Fprintf(vm.writer, "[vm] "+format+"\n", args...)
}

func (vm *VM) debugStack() {
	if !vm.Debug {
		return
	}
	printStack(vm, vm.stack, vm.sp)
}

func (vm *VM) push(val any) {
	vm.stack = append(vm.stack, val)
	vm.debugf("⇡ push: %-10T %v  → sp=%d", val, val, vm.sp)
	vm.sp++
}

func (vm *VM) pop() any {
	if vm.sp <= 0 {
		panic("stack underflow")
	}
	vm.sp--
	val := vm.stack[vm.sp]
	vm.debugf("⇣ pop : %-10T %v  ← sp=%d", val, val, vm.sp)
	vm.stack = vm.stack[:vm.sp]
	return val
}

func (vm *VM) current() *compiler.Instruction {
	return &vm.frame.Fn.Body[vm.frame.IP]
}

func (vm *VM) Run(chunk *compiler.Chunk) error {
	// fmt.Printf("Chunk code: %v\n", chunk.Code)
	mainFn := &compiler.Function{
		Name:   "__main__",
		Params: nil,
		Body:   chunk.Code,
	}
	mainFrame := &Frame{
		Fn:     mainFn,
		Locals: make(map[string]any),
	}
	vm.frames = append(vm.frames, mainFrame)
	vm.frame = mainFrame

	for len(vm.frames) > 0 {
		frame := vm.frame
		code := frame.Fn.Body

		vm.debugf("Running function: %s", frame.Fn.Name)
		vm.debugf("Code: %v", code)

		for frame.IP < len(code) {
			inst := code[frame.IP]
			vm.debugf("fn=%s ip=%d/%d op=%v arg=%v", frame.Fn.Name, frame.IP, len(code), inst.Op, inst.Arg)

			vm.debugStack()
			vm.debugf("   → locals: %v", frame.Locals)

			frame.IP++

			switch inst.Op {
			case compiler.OpConst:
				vm.push(inst.Arg)

			case compiler.OpPop:
				vm.pop()

			case compiler.OpAdd:
				b := vm.pop()
				a := vm.pop()
				v, err := arith("+", a, b)
				if err != nil {
					return err
				}
				vm.push(v)

			case compiler.OpSub:
				b := vm.pop()
				a := vm.pop()
				v, err := arith("-", a, b)
				if err != nil {
					return err
				}
				vm.push(v)

			case compiler.OpMul:
				b := vm.pop()
				a := vm.pop()
				v, err := arith("*", a, b)
				if err != nil {
					return err
				}
				vm.push(v)

			case compiler.OpDiv:
				b := vm.pop()
				a := vm.pop()
				v, err := arith("/", a, b)
				if err != nil {
					return err
				}
				vm.push(v)

			case compiler.OpMod:
				b := vm.pop()
				a := vm.pop()
				v, err := arith("%", a, b)
				if err != nil {
					return err
				}
				vm.push(v)

			case compiler.OpNeg:
				a := vm.pop()
				v, err := negate(a)
				if err != nil {
					return err
				}
				vm.push(v)

			case compiler.OpEq:
				b := vm.pop()
				a := vm.pop()
				v, err := compare("==", a, b)
				if err != nil {
					return err
				}
				vm.push(v)

			case compiler.OpNeq:
				b := vm.pop()
				a := vm.pop()
				v, err := compare("!=", a, b)
				if err != nil {
					return err
				}
				vm.push(v)

			case compiler.OpLt:
				b := vm.pop()
				a := vm.pop()
				v, err := compare("<", a, b)
				if err != nil {
					return err
				}
				vm.push(v)

			case compiler.OpLe:
				b := vm.pop()
				a := vm.pop()
				v, err := compare("<=", a, b)
				if err != nil {
					return err
				}
				vm.push(v)

			case compiler.OpGt:
				b := vm.pop()
				a := vm.pop()
				v, err := compare(">", a, b)
				if err != nil {
					return err
				}
				vm.push(v)

			case compiler.OpGe:
				b := vm.pop()
				a := vm.pop()
				v, err := compare(">=", a, b)
				if err != nil {
					return err
				}
				vm.push(v)

			case compiler.OpStore:
				name := inst.Arg.(string)
				val := vm.pop()

				if vm.frame != nil {
					vm.frame.Locals[name] = val
					vm.debugf("STORE local %s = %T %v", name, val, val)
				} else {
					vm.globals[name] = val
					vm.debugf("STORE global %s = %T %v", name, val, val)
				}

			case compiler.OpLoad:
				name := inst.Arg.(string)

				// ✅ 1. Try current frame locals
				if val, ok := vm.frame.Locals[name]; ok {
					vm.push(val)
					vm.debugf("LOAD local %s → %T %v\n", name, val, val)
					break
				}

				// ✅ 2. Try captured closure variables
				if vm.frame.Closure != nil {
					if val, ok := vm.frame.Closure.Captured[name]; ok {
						vm.push(val)
						vm.debugf("LOAD captured %s → %T %v\n", name, val, val)
						break
					}
				}

				// ✅ 3. Fallback to global environment
				if val, ok := vm.globals[name]; ok {
					vm.push(val)
					vm.debugf("LOAD global %s → %T %v\n", name, val, val)
					break
				}

				// ❌ Not found anywhere
				return fmt.Errorf("undefined variable: %s", name)

			case compiler.OpDefineFun:
				fn := inst.Arg.(*compiler.Function)
				closure := &Closure{
					Fn:       fn,
					Captured: make(map[string]any),
				}

				// 👇 Auto-capture the function's own name if next instruction is `OpStore`
				if frame.IP < len(code) {
					next := code[frame.IP] // ← This is safe because we've already incremented IP before the switch
					if next.Op == compiler.OpStore {
						name := next.Arg.(string)
						closure.Captured[name] = closure
					}
				}

				vm.push(closure)

			case compiler.OpCall:
				argc := inst.Arg.(int)

				if vm.sp < argc+1 {
					return fmt.Errorf("OpCall: not enough values on stack: sp=%d, want %d args + fn", vm.sp, argc)
				}

				// ✅ Pop function first (top of stack)
				fn := vm.pop()

				// ✅ Pop args in reverse order: last arg is top of stack
				args := make([]any, argc)
				for i := argc - 1; i >= 0; i-- {
					args[i] = vm.pop()
				}

				// NOW calculate base pointer after popping
				basePtr := vm.sp

				if len(vm.frames) >= maxFrames {
					return fmt.Errorf("stack overflow: exceeded %d frames", maxFrames)
				}

				// DEBUG function type and args
				vm.logf("OpCall → fn=%T, args=%v, basePtr=%d, sp=%d", fn, args, basePtr, vm.sp)

				switch f := fn.(type) {

				case *Closure:
					vm.logf("⇨ call Closure %s with args %v", f.Fn.Name, args)
					// ✅ Create a new frame with the function and captured locals
					newFrame := &Frame{
						Fn:      f.Fn,
						IP:      0,
						Locals:  make(map[string]any),
						BasePtr: basePtr, // vm.sp, // 📌 Marks current top of stack (caller frame’s top)
						Closure: f,       // ← Track the calling closure
					}

					// ✅ Assign arguments to parameters
					for i, name := range f.Fn.Params {
						newFrame.Locals[name] = args[i]
					}

					// ✅ Push new frame and switch
					vm.frames = append(vm.frames, newFrame)
					vm.frame = newFrame

				case string:
					vm.logf("⇨ call Builtin %s with args %v", f, args)
					// ✅ Built-in function call by name
					builtin, ok := vm.builtins[f]
					if !ok {
						return fmt.Errorf("unknown builtin: %s", f)
					}
					ret, err := builtin(vm, args)
					if err != nil {
						return err
					}
					vm.push(ret)

				default:
					return fmt.Errorf("cannot call type: %T", fn)
				}

			case compiler.OpReturn:
				var retVal any
				if vm.sp > vm.frame.BasePtr {
					retVal = vm.pop()
					vm.debugf("Return value: %T %v\n", retVal, retVal)
				} else {
					vm.debugf("Return without value (sp=%d base=%d)\n", vm.sp, vm.frame.BasePtr)
				}

				// Pop current frame
				prevBase := vm.frame.BasePtr
				vm.frames = vm.frames[:len(vm.frames)-1]

				if len(vm.frames) > 0 {
					vm.frame = vm.frames[len(vm.frames)-1]

					// DEBUG: detect and report leaks
					if vm.sp != prevBase {
						vm.debugf("⚠️ stack pointer reset: sp=%d → BasePtr=%d (frame=%s)\n", vm.sp, prevBase, vm.frame.Fn.Name)
						if delta := vm.sp - prevBase; delta > 0 {
							leaked := make([]any, delta)
							copy(leaked, vm.stack[prevBase:vm.sp])
							vm.debugf("⚠️ stack delta: %d values leaked\n", delta)
							vm.debugf("⚠️ leaked stack values: %v\n", leaked)
						}
					}

					// Truncate stack to base pointer of caller
					vm.stack = vm.stack[:prevBase]
					vm.sp = prevBase

					if retVal != nil {
						vm.push(retVal)
					}
				} else {
					// Last frame → exit
					vm.frame = nil
					vm.debugf("program exited")
				}

			case compiler.OpExpect:
				val := vm.pop()
				if val != true {
					return fmt.Errorf("test failed: expected true, got %v", val)
				}
			case compiler.OpMakeList:
				n := inst.Arg.(int)
				list := make([]any, n)
				for i := n - 1; i >= 0; i-- {
					list[i] = vm.pop()
				}
				vm.push(list)

			case compiler.OpMakeMap:
				n := inst.Arg.(int)
				m := make(map[any]any)
				for i := 0; i < n; i++ {
					val := vm.pop()
					key := vm.pop()
					m[key] = val
				}
				vm.push(m)

			case compiler.OpGetAttr:
				key := vm.pop()
				obj := vm.pop()
				switch o := obj.(type) {
				case map[any]any:
					vm.push(o[key])
				default:
					return fmt.Errorf("unsupported GetAttr on %T", obj)
				}

			case compiler.OpJump:
				frame.IP = inst.Arg.(int)

			case compiler.OpJumpIfFalse:
				cond := vm.pop()
				if !truthy(cond) {
					frame.IP = inst.Arg.(int)
				}

			case compiler.OpIndex:
				index := vm.pop()
				target := vm.pop()
				switch t := target.(type) {
				case []any:
					i := int(toFloat(index))
					if i < 0 || i >= len(t) {
						return fmt.Errorf("index out of range")
					}
					vm.push(t[i])
				case map[any]any:
					vm.push(t[index])
				default:
					return fmt.Errorf("cannot index into type: %T", target)
				}

			default:
				return fmt.Errorf("unknown opcode: %d", inst.Op)
			}
			vm.stackPreview()
			// vm.logf("   → top = %-10T %v", vm.peek(), vm.peek())

			// If frame changed (via OpCall or OpReturn), break inner loop
			if vm.frame != frame {
				break
			}
		}
	}
	return nil
}

func negate(val any) (any, error) {
	switch v := val.(type) {
	case int:
		return -v, nil
	case float64:
		return -v, nil
	default:
		return nil, fmt.Errorf("cannot negate type: %T", val)
	}
}

func compare(op string, a, b any) (bool, error) {
	switch a := a.(type) {
	case int:
		bi, ok := b.(int)
		if !ok {
			return false, fmt.Errorf("type mismatch: %T and %T", a, b)
		}
		switch op {
		case "==":
			return a == bi, nil
		case "!=":
			return a != bi, nil
		case "<":
			return a < bi, nil
		case "<=":
			return a <= bi, nil
		case ">":
			return a > bi, nil
		case ">=":
			return a >= bi, nil
		}
	case float64:
		bf, ok := b.(float64)
		if !ok {
			return false, fmt.Errorf("type mismatch: %T and %T", a, b)
		}
		switch op {
		case "==":
			return a == bf, nil
		case "!=":
			return a != bf, nil
		case "<":
			return a < bf, nil
		case "<=":
			return a <= bf, nil
		case ">":
			return a > bf, nil
		case ">=":
			return a >= bf, nil
		}
	case string:
		bs, ok := b.(string)
		if !ok {
			return false, fmt.Errorf("type mismatch: %T and %T", a, b)
		}
		switch op {
		case "==":
			return a == bs, nil
		case "!=":
			return a != bs, nil
		case "<":
			return a < bs, nil
		case "<=":
			return a <= bs, nil
		case ">":
			return a > bs, nil
		case ">=":
			return a >= bs, nil
		}
	case bool:
		bb, ok := b.(bool)
		if !ok {
			return false, fmt.Errorf("type mismatch: %T and %T", a, b)
		}
		switch op {
		case "==":
			return a == bb, nil
		case "!=":
			return a != bb, nil
		}
	}
	return false, fmt.Errorf("unsupported comparison: %T %s %T", a, op, b)
}

func arith(op string, a, b any) (any, error) {
	switch a := a.(type) {
	case int:
		bi, ok := b.(int)
		if !ok {
			return nil, fmt.Errorf("type mismatch: %T and %T", a, b)
		}
		switch op {
		case "+":
			return a + bi, nil
		case "-":
			return a - bi, nil
		case "*":
			return a * bi, nil
		case "/":
			if bi == 0 {
				return nil, fmt.Errorf("division by zero")
			}
			return a / bi, nil
		case "%":
			if bi == 0 {
				return nil, fmt.Errorf("modulo by zero")
			}
			return a % bi, nil
		}

	case int64:
		switch b := b.(type) {
		case int64:
			switch op {
			case "+":
				return a + b, nil
			case "-":
				return a - b, nil
			case "*":
				return a * b, nil
			case "/":
				if b == 0 {
					return nil, fmt.Errorf("division by zero")
				}
				return a / b, nil
			case "%":
				if b == 0 {
					return nil, fmt.Errorf("modulo by zero")
				}
				return a % b, nil
			}
		case int:
			return arith(op, a, int64(b)) // promote int to int64
		}

	case float64:
		bf, ok := b.(float64)
		if !ok {
			return nil, fmt.Errorf("type mismatch: %T and %T", a, b)
		}
		switch op {
		case "+":
			return a + bf, nil
		case "-":
			return a - bf, nil
		case "*":
			return a * bf, nil
		case "/":
			if bf == 0.0 {
				return nil, fmt.Errorf("division by zero")
			}
			return a / bf, nil
		}
	case []any:
		bList, ok := b.([]any)
		if !ok {
			return nil, fmt.Errorf("type mismatch: %T and %T", a, b)
		}
		switch op {
		case "+":
			return append(a, bList...), nil
		default:
			// return nil, fmt.Errorf("unsupported operation '%s' on lists: %#v and %#v", op, a, b)
			return nil, fmt.Errorf("unsupported operation '%s' on lists", op)
		}
	}

	return nil, fmt.Errorf("unsupported operand '%s' types: %T and %T", op, a, b)
}

func (vm *VM) stackPreview() string {
	if !vm.Debug || vm.sp <= 0 || vm.sp > len(vm.stack) {
		return "(empty)"
	}
	start := max(vm.sp-5, 0)
	preview := vm.stack[start:vm.sp]
	parts := make([]string, len(preview))
	for i, v := range preview {
		parts[i] = fmt.Sprintf("%-8T %v", v, v)
	}
	return strings.Join(parts, " | ")
}

func (vm *VM) logf(format string, args ...any) {
	if !vm.Debug {
		return
	}
	msg := fmt.Sprintf(format, args...)
	fn := "(nil)"
	if vm.frame != nil && vm.frame.Fn != nil {
		fn = vm.frame.Fn.Name
	}

	stackPreview := ""
	if vm.sp > 0 {
		stackPreview = fmt.Sprintf("%v", vm.stack[max(vm.sp-5, 0):vm.sp])
	}
	vm.debugf("fn=%s frame=%d ip=%d sp=%d\n      %s\n      stack top: %v\n\n",
		fn, len(vm.frames), vm.frame.IP, vm.sp, msg, stackPreview)
}

func truthy(v any) bool {
	switch x := v.(type) {
	case bool:
		return x
	case nil:
		return false
	case float64:
		return x != 0
	case string:
		return x != ""
	default:
		return true
	}
}

func toFloat(v any) float64 {
	switch v := v.(type) {
	case int:
		return float64(v)
	case float64:
		return v
	default:
		panic(fmt.Sprintf("cannot convert to float: %T", v))
	}
}

func equal(a, b any) bool {
	switch a := a.(type) {
	case int:
		if b, ok := b.(int); ok {
			return a == b
		}
	case float64:
		if b, ok := b.(float64); ok {
			return a == b
		}
	case string:
		if b, ok := b.(string); ok {
			return a == b
		}
	case bool:
		if b, ok := b.(bool); ok {
			return a == b
		}
	}
	return false
}

type Builtin func(vm *VM, args []any) (any, error)

func (vm *VM) initBuiltins() {
	vm.builtins = map[string]Builtin{
		"print": builtinPrint,
		"len":   builtinLen,
		"now":   builtinNow,
		"json":  builtinJSON,
	}
}

func builtinPrint(vm *VM, args []any) (any, error) {
	var sb strings.Builder
	for _, arg := range args {
		fmt.Fprintf(&sb, "%v ", arg)
	}
	fmt.Fprintln(vm.writer, strings.TrimSpace(sb.String()))
	return nil, nil
}

func builtinLen(vm *VM, args []any) (any, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("len() expects 1 argument, got %d", len(args))
	}
	switch v := args[0].(type) {
	case []any:
		return len(v), nil
	case string:
		return len([]rune(v)), nil
	case map[any]any:
		return len(v), nil
	default:
		return nil, fmt.Errorf("invalid operand to len(): %T", args[0])
	}
}

func builtinNow(vm *VM, args []any) (any, error) {
	if len(args) != 0 {
		return nil, fmt.Errorf("now() takes no arguments")
	}
	return time.Now().UnixNano(), nil
}

func builtinJSON(vm *VM, args []any) (any, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("json(x) takes exactly one argument")
	}
	normalized, err := normalizeValue(args[0])
	if err != nil {
		return nil, err
	}

	data, err := json.MarshalIndent(normalized, "", "  ")
	if err != nil {
		return nil, err
	}
	fmt.Fprintln(vm.writer, string(data))
	return nil, nil
}

func printStack(vm *VM, stack []any, sp int) {
	if !vm.Debug {
		return
	}
	if sp <= 0 || sp > len(stack) {
		fmt.Println("   → stack: (empty or invalid)")
		return
	}
	const previewLimit = 10
	const repeatCheckLimit = 20

	// If small, just print
	if sp <= previewLimit {
		vm.debugf("   → stack (%d): %s\n", sp, formatSlice(stack[:sp]))
		return
	}

	// Check for repeated values at top
	start := max(sp-repeatCheckLimit, 0)
	top := stack[start:sp]
	same := true
	for i := 1; i < len(top); i++ {
		if top[i] != top[0] {
			same = false
			break
		}
	}
	if same {
		vm.debugf("   → stack (%d): [%T] %v × %d\n", sp, top[0], top[0], sp)
		return
	}

	// Show head + tail
	head := formatSlice(stack[:5])
	tail := formatSlice(stack[sp-5:])
	vm.debugf("   → stack (%d): %s ... %s\n", sp, head, tail)
}

func formatSlice(slice []any) string {
	parts := make([]string, len(slice))
	for i, v := range slice {
		switch val := v.(type) {
		case *Closure:
			parts[i] = fmt.Sprintf("&%s", val.Fn.Name)
		case float64, int, string, bool:
			parts[i] = fmt.Sprintf("%v", val)
		default:
			parts[i] = fmt.Sprintf("0x%p", val)
		}
	}
	return "[" + strings.Join(parts, " ") + "]"
}

func normalizeValue(v any) (any, error) {
	switch v := v.(type) {
	case map[any]any:
		out := make(map[string]any)
		for k, val := range v {
			ks, ok := k.(string)
			if !ok {
				return nil, fmt.Errorf("json: map key must be string, got %T", k)
			}
			nval, err := normalizeValue(val)
			if err != nil {
				return nil, err
			}
			out[ks] = nval
		}
		return out, nil
	case []any:
		for i, elem := range v {
			nval, err := normalizeValue(elem)
			if err != nil {
				return nil, err
			}
			v[i] = nval
		}
		return v, nil
	default:
		return v, nil
	}
}
