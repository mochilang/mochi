// if_then_else_nested.c - manual translation of tests/vm/valid/if_then_else_nested.mochi
#include <stdio.h>

int main() {
    int x = 8;
    const char *msg = (x > 10) ? "big" : ((x > 5) ? "medium" : "small");
    printf("%s\n", msg);
    return 0;
}
