// test_block.c - manual translation of tests/vm/valid/test_block.mochi
#include <stdio.h>
#include <assert.h>

int main() {
    int x = 1 + 2;
    assert(x == 3);
    printf("ok\n");
    return 0;
}
