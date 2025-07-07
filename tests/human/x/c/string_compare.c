// string_compare.c - manual translation of tests/vm/valid/string_compare.mochi
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

int main() {
    printf("%s\n", strcmp("a", "b") < 0 ? "true" : "false");
    printf("%s\n", strcmp("a", "a") <= 0 ? "true" : "false");
    printf("%s\n", strcmp("b", "a") > 0 ? "true" : "false");
    printf("%s\n", strcmp("b", "b") >= 0 ? "true" : "false");
    return 0;
}
