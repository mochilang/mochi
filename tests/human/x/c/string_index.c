// string_index.c - manual translation of tests/vm/valid/string_index.mochi
#include <stdio.h>

int main() {
    const char *s = "mochi";
    printf("%c\n", s[1]);
    return 0;
}
