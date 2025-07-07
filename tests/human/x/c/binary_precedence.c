// binary_precedence.c - manual translation of tests/vm/valid/binary_precedence.mochi
#include <stdio.h>

int main() {
    printf("%d\n", 1 + 2 * 3);
    printf("%d\n", (1 + 2) * 3);
    printf("%d\n", 2 * 3 + 1);
    printf("%d\n", 2 * (3 + 1));
    return 0;
}
