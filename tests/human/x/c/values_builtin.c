// values_builtin.c - manual translation of tests/vm/valid/values_builtin.mochi
#include <stdio.h>

typedef struct { const char *key; int value; } Entry;

int main() {
    Entry m[] = { {"a",1}, {"b",2}, {"c",3} };
    printf("[%d, %d, %d]\n", m[0].value, m[1].value, m[2].value);
    return 0;
}
