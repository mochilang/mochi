// string_prefix_slice.c - manual translation of tests/vm/valid/string_prefix_slice.mochi
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

static bool has_prefix(const char *s, const char *prefix) {
    return strncmp(s, prefix, strlen(prefix)) == 0;
}

int main() {
    const char *prefix = "fore";
    const char *s1 = "forest";
    printf("%s\n", has_prefix(s1, prefix) ? "true" : "false");
    const char *s2 = "desert";
    printf("%s\n", has_prefix(s2, prefix) ? "true" : "false");
    return 0;
}
