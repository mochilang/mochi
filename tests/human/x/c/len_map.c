// len_map.c - manual translation of tests/vm/valid/len_map.mochi
#include <stdio.h>

int main() {
    const char *keys[] = {"a", "b"};
    int values[] = {1, 2};
    (void)values;
    printf("%lu\n", (unsigned long)(sizeof(keys) / sizeof(keys[0])));
    return 0;
}
