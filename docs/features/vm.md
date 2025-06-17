## Virtual Machine Design

Mochi's VM executes a simple register-based SSA (static single assignment) instruction set. Each function
is compiled to a list of instructions that operate on numbered registers. Registers hold untyped Go `any`
values for now, keeping the interpreter straightforward.

### Registers and Instructions

Every operation reads its operands from registers and writes the result to a new register. Because values are
single assignment, a register is written at most once. Instructions are represented as:

```go
// runtime/vm/instruction.go
 type Instr struct {
     Op   OpCode
     A, B int   // input registers
     C    int   // destination register
     Arg  any   // optional literal or jump target
 }
```

The interpreter maintains a slice of registers for the current frame. Opcodes cover arithmetic, branching,
loading constants and calling other functions.

### Program Structure

A compiled program contains a list of `Func` objects, each with its own instruction slice and arity. Calls push a
new frame and return the value stored in register 0 of the callee.

### Example

The following pseudo bytecode computes `add(1, 2)`:

```
load_const r0, 1
load_const r1, 2
add       r2, r0, r1
ret       r2
```

Running the VM returns `3`.

This minimal design leaves plenty of room for optimisation and future compilation from Mochi source.
