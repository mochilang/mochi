// left_join.c - manual translation of tests/vm/valid/left_join.mochi
#include <stdio.h>
#include <stdbool.h>

struct Customer { int id; const char *name; };
struct Order { int id; int customerId; int total; };

int main() {
    struct Customer customers[] = {
        {1, "Alice"},
        {2, "Bob"}
    };
    struct Order orders[] = {
        {100, 1, 250},
        {101, 3, 80}
    };
    int nCustomers = sizeof(customers)/sizeof(customers[0]);
    int nOrders = sizeof(orders)/sizeof(orders[0]);

    printf("--- Left Join ---\n");
    for (int i = 0; i < nOrders; i++) {
        const char *name = NULL;
        for (int j = 0; j < nCustomers; j++) {
            if (orders[i].customerId == customers[j].id) {
                name = customers[j].name;
                break;
            }
        }
        printf("Order %d customer %s total %d\n", orders[i].id,
               name ? name : "NULL", orders[i].total);
    }
    return 0;
}
