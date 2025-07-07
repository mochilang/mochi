// string_concat.c - manual translation of tests/vm/valid/string_concat.mochi
#include <stdio.h>
#include <string.h>

int main() {
    char result[32];
    strcpy(result, "hello ");
    strcat(result, "world");
    printf("%s\n", result);
    return 0;
}
