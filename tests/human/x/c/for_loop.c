// for_loop.c - manual translation of tests/vm/valid/for_loop.mochi
#include <stdio.h>

int main() {
    for (int i = 1; i < 4; i++) {
        printf("%d\n", i);
    }
    return 0;
}
