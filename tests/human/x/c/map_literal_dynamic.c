// map_literal_dynamic.c - manual translation of tests/vm/valid/map_literal_dynamic.mochi
#include <stdio.h>
#include <string.h>

typedef struct { const char *key; int value; } Entry;

static int get_value(Entry *entries, int n, const char *key) {
    for (int i = 0; i < n; i++) {
        if (strcmp(entries[i].key, key) == 0) return entries[i].value;
    }
    return 0;
}

int main() {
    int x = 3;
    int y = 4;
    Entry m[] = { {"a", x}, {"b", y} };
    printf("%d %d\n", get_value(m, 2, "a"), get_value(m, 2, "b"));
    return 0;
}
