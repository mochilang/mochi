// fun_three_args.c - manual translation of tests/vm/valid/fun_three_args.mochi
#include <stdio.h>

static int sum3(int a, int b, int c) {
    return a + b + c;
}

int main() {
    printf("%d\n", sum3(1, 2, 3));
    return 0;
}
