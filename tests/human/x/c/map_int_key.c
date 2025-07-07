// map_int_key.c - manual translation of tests/vm/valid/map_int_key.mochi
#include <stdio.h>

typedef struct { int key; const char *value; } Entry;

static const char* get_value(Entry *entries, int n, int key) {
    for (int i = 0; i < n; i++) {
        if (entries[i].key == key) return entries[i].value;
    }
    return "";
}

int main() {
    Entry m[] = { {1, "a"}, {2, "b"} };
    printf("%s\n", get_value(m, 2, 1));
    return 0;
}
