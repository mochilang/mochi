// for_map_collection.c - manual translation of tests/vm/valid/for_map_collection.mochi
#include <stdio.h>

int main() {
    const char *keys[] = {"a", "b"};
    int values[] = {1, 2};
    for (int i = 0; i < 2; i++) {
        (void)values[i]; // values unused
        printf("%s\n", keys[i]);
    }
    return 0;
}
