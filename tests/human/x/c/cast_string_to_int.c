// cast_string_to_int.c - manual translation of tests/vm/valid/cast_string_to_int.mochi
#include <stdio.h>
#include <stdlib.h>

int main() {
    const char *s = "1995";
    int v = atoi(s);
    printf("%d\n", v);
    return 0;
}
