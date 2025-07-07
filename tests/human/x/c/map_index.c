// map_index.c - manual translation of tests/vm/valid/map_index.mochi
#include <stdio.h>
#include <string.h>

typedef struct { const char *key; int value; } Entry;

static int get_value(Entry *entries, int n, const char *key) {
    for (int i = 0; i < n; i++) {
        if (strcmp(entries[i].key, key) == 0) {
            return entries[i].value;
        }
    }
    return 0;
}

int main() {
    Entry m[] = {{"a", 1}, {"b", 2}};
    printf("%d\n", get_value(m, 2, "b"));
    return 0;
}
