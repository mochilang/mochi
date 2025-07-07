// cross_join.c - manual translation of tests/vm/valid/cross_join.mochi
#include <stdio.h>

struct Customer { int id; const char *name; };
struct Order { int id; int customerId; int total; };

int main() {
    struct Customer customers[] = {
        {1, "Alice"},
        {2, "Bob"},
        {3, "Charlie"}
    };
    struct Order orders[] = {
        {100, 1, 250},
        {101, 2, 125},
        {102, 1, 300}
    };
    int numCustomers = sizeof(customers)/sizeof(customers[0]);
    int numOrders = sizeof(orders)/sizeof(orders[0]);
    printf("--- Cross Join: All order-customer pairs ---\n");
    for (int i = 0; i < numOrders; i++) {
        for (int j = 0; j < numCustomers; j++) {
            printf("Order %d (customerId: %d, total: $%d) paired with %s\n",
                   orders[i].id, orders[i].customerId,
                   orders[i].total, customers[j].name);
        }
    }
    return 0;
}
