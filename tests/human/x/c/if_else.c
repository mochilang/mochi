// if_else.c - manual translation of tests/vm/valid/if_else.mochi
#include <stdio.h>

int main() {
    int x = 5;
    if (x > 3) {
        printf("big\n");
    } else {
        printf("small\n");
    }
    return 0;
}
