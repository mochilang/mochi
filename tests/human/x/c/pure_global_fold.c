// pure_global_fold.c - manual translation of tests/vm/valid/pure_global_fold.mochi
#include <stdio.h>

static int k = 2;
static int inc(int x) { return x + k; }

int main() {
    printf("%d\n", inc(3));
    return 0;
}
