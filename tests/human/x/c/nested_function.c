// nested_function.c - manual translation of tests/vm/valid/nested_function.mochi
#include <stdio.h>

static int inner(int x, int y) {
    return x + y;
}

int outer(int x) {
    return inner(x, 5);
}

int main() {
    printf("%d\n", outer(3));
    return 0;
}
