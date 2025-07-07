// group_by_join.c - manual translation of tests/vm/valid/group_by_join.mochi
#include <stdio.h>
#include <string.h>

struct Customer { int id; const char *name; };
struct Order { int id; int customerId; };
struct Stat { const char *name; int count; };

int main() {
    struct Customer customers[] = {
        {1, "Alice"},
        {2, "Bob"}
    };
    struct Order orders[] = {
        {100, 1},
        {101, 1},
        {102, 2}
    };
    int nCustomers = sizeof(customers)/sizeof(customers[0]);
    int nOrders = sizeof(orders)/sizeof(orders[0]);

    struct Stat stats[10];
    int statCount = 0;

    for (int i = 0; i < nOrders; i++) {
        const char *name = NULL;
        for (int j = 0; j < nCustomers; j++) {
            if (orders[i].customerId == customers[j].id) {
                name = customers[j].name;
                break;
            }
        }
        if (name) {
            int k;
            for (k = 0; k < statCount; k++) {
                if (strcmp(stats[k].name, name) == 0) {
                    stats[k].count++;
                    break;
                }
            }
            if (k == statCount) {
                stats[k].name = name;
                stats[k].count = 1;
                statCount++;
            }
        }
    }

    printf("--- Orders per customer ---\n");
    for (int i = 0; i < statCount; i++) {
        printf("%s orders: %d\n", stats[i].name, stats[i].count);
    }
    return 0;
}
