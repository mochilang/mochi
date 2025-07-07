// inner_join.c - manual translation of tests/vm/valid/inner_join.mochi
#include <stdio.h>
#include <stdbool.h>

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
        {102, 1, 300},
        {103, 4, 80}
    };
    int numCustomers = sizeof(customers)/sizeof(customers[0]);
    int numOrders = sizeof(orders)/sizeof(orders[0]);

    printf("--- Orders with customer info ---\n");
    for (int i = 0; i < numOrders; i++) {
        for (int j = 0; j < numCustomers; j++) {
            if (orders[i].customerId == customers[j].id) {
                printf("Order %d by %s - $%d\n", orders[i].id, customers[j].name, orders[i].total);
            }
        }
    }
    return 0;
}
