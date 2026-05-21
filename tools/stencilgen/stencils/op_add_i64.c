// op_add_i64.c is the MEP-42 Phase 1.1 stencil for ir.OpAddI64.
//
// ABI summary (x86_64 SysV variant adapted for Mochi):
//
//   R12 = pointer to the current vm3 Frame
//   R13 = pointer to the typed-arena base table (Arenas)
//   R14 = pointer to the per-VM context (PC stash, deopt slot)
//   RAX = left operand (i64)
//   RDI = right operand (i64)
//   RSI = scratch
//
// Result: RAX = RAX + RDI. The next-op stencil is appended directly
// in the code cache (no inter-stencil call); the function returns
// nothing because copypatch concatenates stencils into a linear code
// stream.
//
// The function is declared with __attribute__((naked)) so Clang does
// not emit a prologue or epilogue; the result must be left in RAX so
// the next stencil sees it on entry.
//
// To check what Clang emits:
//
//   clang -O2 -fno-asynchronous-unwind-tables -fno-stack-protector \
//         -mno-red-zone -fpic -fno-pie -S op_add_i64.c -o -
//
// Expected x86_64 output (one of):
//
//   addq %rdi, %rax           ; 48 01 F8
//
// The Phase 1 hand-written stencil table in
// compiler3/emit/copypatch/stencils_amd64.go mirrors this shape
// verbatim until stencilgen takes over.

typedef long long int64_t;

__attribute__((naked))
void mochi_op_add_i64(void) {
	__asm__("addq %rdi, %rax\n\t");
}
