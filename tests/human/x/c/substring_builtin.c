// substring_builtin.c - manual translation of tests/vm/valid/substring_builtin.mochi
#include <stdio.h>
#include <string.h>

int main() {
    const char *s = "mochi";
    char buf[5];
    strncpy(buf, s + 1, 3);
    buf[3] = '\0';
    printf("%s\n", buf);
    return 0;
}
