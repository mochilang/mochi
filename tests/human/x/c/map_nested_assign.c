// map_nested_assign.c - manual translation of tests/vm/valid/map_nested_assign.mochi
#include <stdio.h>

typedef struct { int inner; } Inner;
typedef struct { Inner outer; } Data;

int main() {
    Data data = { .outer = { .inner = 1 } };
    data.outer.inner = 2;
    printf("%d\n", data.outer.inner);
    return 0;
}
