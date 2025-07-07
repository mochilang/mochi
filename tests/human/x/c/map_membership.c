// map_membership.c - manual translation of tests/vm/valid/map_membership.mochi
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

typedef struct { const char *key; int value; } Entry;

static bool contains(Entry *entries, int n, const char *key) {
    for (int i = 0; i < n; i++) {
        if (strcmp(entries[i].key, key) == 0) return true;
    }
    return false;
}

int main() {
    Entry m[] = {{"a", 1}, {"b", 2}};
    printf("%s\n", contains(m, 2, "a") ? "true" : "false");
    printf("%s\n", contains(m, 2, "c") ? "true" : "false");
    return 0;
}
