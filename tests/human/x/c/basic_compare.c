// basic_compare.c - manual translation of tests/vm/valid/basic_compare.mochi
#include <stdio.h>
#include <stdbool.h>

int main() {
    int a = 10 - 3;
    int b = 2 + 2;
    printf("%d\n", a);
    printf("%s\n", (a == 7) ? "true" : "false");
    printf("%s\n", (b < 5) ? "true" : "false");
    return 0;
}
