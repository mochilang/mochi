.section .data
msg:
    .asciz "hello\n"
.section .text
.globl _start
_start:
    mov $1, %rax
    mov $1, %rdi
    lea msg(%rip), %rsi
    mov $6, %rdx
    syscall
    mov $60, %rax
    xor %rdi, %rdi
    syscall
