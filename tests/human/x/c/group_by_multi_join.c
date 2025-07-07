// group_by_multi_join.c - manual translation of tests/vm/valid/group_by_multi_join.mochi
#include <stdio.h>
#include <string.h>

struct Nation { int id; const char *name; };
struct Supplier { int id; int nation; };
struct PartSupp { int part; int supplier; double cost; int qty; };
struct Stat { int part; double total; };

int main() {
    struct Nation nations[] = {
        {1, "A"},
        {2, "B"}
    };
    struct Supplier suppliers[] = {
        {1, 1},
        {2, 2}
    };
    struct PartSupp ps[] = {
        {100, 1, 10.0, 2},
        {100, 2, 20.0, 1},
        {200, 1, 5.0, 3}
    };
    int nNations = sizeof(nations)/sizeof(nations[0]);
    int nSuppliers = sizeof(suppliers)/sizeof(suppliers[0]);
    int nPS = sizeof(ps)/sizeof(ps[0]);

    struct Stat stats[10];
    int scount = 0;

    for (int i = 0; i < nPS; i++) {
        struct Supplier *s = NULL;
        for (int j = 0; j < nSuppliers; j++)
            if (suppliers[j].id == ps[i].supplier) { s = &suppliers[j]; break; }
        if (!s) continue;
        struct Nation *n = NULL;
        for (int j = 0; j < nNations; j++)
            if (nations[j].id == s->nation) { n = &nations[j]; break; }
        if (!n || strcmp(n->name, "A") != 0) continue;

        double value = ps[i].cost * ps[i].qty;
        int k;
        for (k = 0; k < scount; k++)
            if (stats[k].part == ps[i].part) break;
        if (k == scount) { stats[k].part = ps[i].part; stats[k].total = 0; scount++; }
        stats[k].total += value;
    }

    for (int i = 0; i < scount; i++) {
        printf("map[part:%d total:%.0f]%s", stats[i].part, stats[i].total,
               i == scount - 1 ? "" : " ");
    }
    printf("\n");
    return 0;
}
