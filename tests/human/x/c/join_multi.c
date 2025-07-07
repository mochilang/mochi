// join_multi.c - manual translation of tests/vm/valid/join_multi.mochi
#include <stdio.h>

struct Customer { int id; const char *name; };
struct Order { int id; int customerId; };
struct Item { int orderId; const char *sku; };

int main() {
    struct Customer customers[] = {
        {1, "Alice"},
        {2, "Bob"}
    };
    struct Order orders[] = {
        {100, 1},
        {101, 2}
    };
    struct Item items[] = {
        {100, "a"},
        {101, "b"}
    };
    int nCustomers = sizeof(customers)/sizeof(customers[0]);
    int nOrders = sizeof(orders)/sizeof(orders[0]);
    int nItems = sizeof(items)/sizeof(items[0]);

    printf("--- Multi Join ---\n");
    for (int i = 0; i < nOrders; i++) {
        for (int j = 0; j < nCustomers; j++) {
            if (orders[i].customerId == customers[j].id) {
                for (int k = 0; k < nItems; k++) {
                    if (items[k].orderId == orders[i].id) {
                        printf("%s bought item %s\n", customers[j].name, items[k].sku);
                    }
                }
            }
        }
    }
    return 0;
}
