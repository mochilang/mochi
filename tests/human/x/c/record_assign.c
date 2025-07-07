// record_assign.c - manual translation of tests/vm/valid/record_assign.mochi
#include <stdio.h>

typedef struct { int n; } Counter;

static void inc(Counter *c) {
    c->n = c->n + 1;
}

int main() {
    Counter c = {0};
    inc(&c);
    printf("%d\n", c.n);
    return 0;
}
