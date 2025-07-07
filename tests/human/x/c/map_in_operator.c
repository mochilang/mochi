// map_in_operator.c - manual translation of tests/vm/valid/map_in_operator.mochi
#include <stdio.h>
#include <stdbool.h>

typedef struct { int key; const char *value; } Entry;

static bool contains_key(Entry *entries, int n, int key) {
    for (int i = 0; i < n; i++) {
        if (entries[i].key == key) return true;
    }
    return false;
}

int main() {
    Entry m[] = { {1, "a"}, {2, "b"} };
    printf("%s\n", contains_key(m, 2, 1) ? "true" : "false");
    printf("%s\n", contains_key(m, 2, 3) ? "true" : "false");
    return 0;
}
