// fun_call.c - manual translation of tests/vm/valid/fun_call.mochi
#include <stdio.h>

static int add(int a, int b) {
    return a + b;
}

int main() {
    printf("%d\n", add(2, 3));
    return 0;
}
