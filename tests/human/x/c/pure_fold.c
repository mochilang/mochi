// pure_fold.c - manual translation of tests/vm/valid/pure_fold.mochi
#include <stdio.h>

static int triple(int x) { return x * 3; }

int main() {
    printf("%d\n", triple(1 + 2));
    return 0;
}
