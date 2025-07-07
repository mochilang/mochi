// group_items_iteration.c - manual translation of tests/vm/valid/group_items_iteration.mochi
#include <stdio.h>
#include <string.h>

struct Item { const char *tag; int val; };
struct Stat { const char *tag; int total; };

int main() {
    struct Item data[] = {
        {"a", 1},
        {"a", 2},
        {"b", 3}
    };
    int n = sizeof(data)/sizeof(data[0]);

    struct Stat stats[10];
    int scount = 0;

    for (int i = 0; i < n; i++) {
        int j;
        for (j = 0; j < scount; j++)
            if (strcmp(stats[j].tag, data[i].tag) == 0) break;
        if (j == scount) { stats[j].tag = data[i].tag; stats[j].total = 0; scount++; }
        stats[j].total += data[i].val;
    }

    // sort by tag
    for (int i = 0; i < scount-1; i++)
        for (int j = i+1; j < scount; j++)
            if (strcmp(stats[i].tag, stats[j].tag) > 0) {
                struct Stat t = stats[i]; stats[i] = stats[j]; stats[j] = t;
            }

    for (int i = 0; i < scount; i++)
        printf("map[tag:%s total:%d]%s", stats[i].tag, stats[i].total,
               i == scount - 1 ? "" : " ");
    printf("\n");
    return 0;
}
