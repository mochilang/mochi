// typed_var.c - manual translation of tests/vm/valid/typed_var.mochi
#include <stdio.h>

int main() {
    int x = 0; // uninitialized int defaults to 0
    printf("%d\n", x);
    return 0;
}
