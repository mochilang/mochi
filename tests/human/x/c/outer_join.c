// outer_join.c - manual translation of tests/vm/valid/outer_join.mochi
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
        {102, 1, 300},
        {103, 5, 80}
    };
    int nCustomers = sizeof(customers)/sizeof(customers[0]);
    int nOrders = sizeof(orders)/sizeof(orders[0]);

    bool orderMatched[nOrders];
    for (int i = 0; i < nOrders; i++) orderMatched[i] = false;

    printf("--- Outer Join using syntax ---\n");
    for (int i = 0; i < nOrders; i++) {
        bool found = false;
        for (int j = 0; j < nCustomers; j++) {
            if (orders[i].customerId == customers[j].id) {
                printf("Order %d by %s - $%d\n", orders[i].id, customers[j].name, orders[i].total);
                found = true;
                break;
            }
        }
        if (!found) {
            printf("Order %d by Unknown - $%d\n", orders[i].id, orders[i].total);
        }
        orderMatched[i] = true;
    }
    for (int j = 0; j < nCustomers; j++) {
        bool hasOrder = false;
        for (int i = 0; i < nOrders; i++) {
            if (customers[j].id == orders[i].customerId) {
                hasOrder = true;
                break;
            }
        }
        if (!hasOrder) {
            printf("Customer %s has no orders\n", customers[j].name);
        }
    }
    return 0;
}
