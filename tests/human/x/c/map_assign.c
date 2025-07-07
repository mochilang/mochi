// map_assign.c - manual translation of tests/vm/valid/map_assign.mochi
#include <stdio.h>
#include <string.h>

typedef struct { const char *key; int value; } Entry;

int main() {
    Entry scores[2];
    scores[0].key = "alice";
    scores[0].value = 1;

    // add new entry
    scores[1].key = "bob";
    scores[1].value = 2;

    // print value for key "bob"
    printf("%d\n", scores[1].value);
    return 0;
}
