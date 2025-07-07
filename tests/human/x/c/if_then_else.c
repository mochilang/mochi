// if_then_else.c - manual translation of tests/vm/valid/if_then_else.mochi
#include <stdio.h>

int main() {
    int x = 12;
    const char *msg = (x > 10) ? "yes" : "no";
    printf("%s\n", msg);
    return 0;
}
