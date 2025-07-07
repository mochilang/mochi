// partial_application.c - manual translation of tests/vm/valid/partial_application.mochi
#include <stdio.h>

static int add(int a, int b) { return a + b; }
static int add5(int b) { return add(5, b); }

int main() {
    printf("%d\n", add5(3));
    return 0;
}
