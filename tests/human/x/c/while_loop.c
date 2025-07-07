// while_loop.c - manual translation of tests/vm/valid/while_loop.mochi
#include <stdio.h>

int main() {
    int i = 0;
    while (i < 3) {
        printf("%d\n", i);
        i = i + 1;
    }
    return 0;
}
