// right_join.c - manual translation of tests/vm/valid/right_join.mochi
#include <stdio.h>
#include <stdbool.h>

struct Customer { int id; const char *name; };
struct Order { int id; int customerId; int total; };

int main() {
    struct Customer customers[] = {
        {1, "Alice"},
        {2, "Bob"},
        {3, "Charlie"},
        {4, "Diana"}
    };
    struct Order orders[] = {
        {100, 1, 250},
        {101, 2, 125},
        {102, 1, 300}
    };
    int nCustomers = sizeof(customers)/sizeof(customers[0]);
    int nOrders = sizeof(orders)/sizeof(orders[0]);

    printf("--- Right Join using syntax ---\n");
    for (int i = 0; i < nOrders; i++) {
        const char *custName = NULL;
        for (int j = 0; j < nCustomers; j++) {
            if (orders[i].customerId == customers[j].id) {
                custName = customers[j].name;
                break;
            }
        }
        if (custName) {
            printf("Customer %s has order %d - $%d\n", custName, orders[i].id, orders[i].total);
        } else {
            printf("Customer Unknown has order %d - $%d\n", orders[i].id, orders[i].total);
        }
    }
    // also print customers with no orders
    for (int j = 0; j < nCustomers; j++) {
        bool found = false;
        for (int i = 0; i < nOrders; i++) {
            if (customers[j].id == orders[i].customerId) {
                found = true;
                break;
            }
        }
        if (!found) {
            printf("Customer %s has no orders\n", customers[j].name);
        }
    }
    return 0;
}
