// fun_expr_in_let.c - manual translation of tests/vm/valid/fun_expr_in_let.mochi
#include <stdio.h>

static int square(int x) {
    return x * x;
}

int main() {
    printf("%d\n", square(6));
    return 0;
}
