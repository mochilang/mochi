// closure.c - manual translation of tests/vm/valid/closure.mochi
#include <stdio.h>

typedef struct { int n; } Adder;

static int add_fn(void *ctx, int x) {
    Adder *a = (Adder*)ctx;
    return a->n + x;
}

static int makeAdder_apply(Adder *a, int x) {
    return add_fn(a, x);
}

int main() {
    Adder add10 = {10};
    int result = makeAdder_apply(&add10, 7);
    printf("%d\n", result);
    return 0;
}
