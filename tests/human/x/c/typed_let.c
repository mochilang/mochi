// typed_let.c - manual translation of tests/vm/valid/typed_let.mochi
#include <stdio.h>

int main() {
    int y = 0; // uninitialized let variable defaults to 0
    printf("%d\n", y);
    return 0;
}
