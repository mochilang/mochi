// match_expr.c - manual translation of tests/vm/valid/match_expr.mochi
#include <stdio.h>

int main() {
    int x = 2;
    const char *label;
    switch (x) {
        case 1: label = "one"; break;
        case 2: label = "two"; break;
        case 3: label = "three"; break;
        default: label = "unknown"; break;
    }
    printf("%s\n", label);
    return 0;
}
