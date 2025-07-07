// string_in_operator.c - manual translation of tests/vm/valid/string_in_operator.mochi
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

int main() {
    const char *s = "catch";
    printf("%s\n", strstr(s, "cat") ? "true" : "false");
    printf("%s\n", strstr(s, "dog") ? "true" : "false");
    return 0;
}
