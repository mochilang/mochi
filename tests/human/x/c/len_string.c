// len_string.c - manual translation of tests/vm/valid/len_string.mochi
#include <stdio.h>
#include <string.h>

int main() {
    const char *s = "mochi";
    printf("%zu\n", strlen(s));
    return 0;
}
