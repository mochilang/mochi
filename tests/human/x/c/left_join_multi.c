// left_join_multi.c - manual translation of tests/vm/valid/left_join_multi.mochi
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
        {100, "a"}
    };
    int nCustomers = sizeof(customers)/sizeof(customers[0]);
    int nOrders = sizeof(orders)/sizeof(orders[0]);
    int nItems = sizeof(items)/sizeof(items[0]);

    printf("--- Left Join Multi ---\n");
    for (int i = 0; i < nOrders; i++) {
        const char *name = NULL;
        for (int j = 0; j < nCustomers; j++)
            if (orders[i].customerId == customers[j].id) { name = customers[j].name; break; }
        const char *item = NULL;
        for (int k = 0; k < nItems; k++)
            if (items[k].orderId == orders[i].id) { item = items[k].sku; break; }
        if (item)
            printf("%d %s map[orderId:%d sku:%s]\n", orders[i].id, name, orders[i].id, item);
        else
            printf("%d %s <nil>\n", orders[i].id, name);
    }
    return 0;
}
