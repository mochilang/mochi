// group_by_left_join.c - manual translation of tests/vm/valid/group_by_left_join.mochi
#include <stdio.h>

struct Customer { int id; const char *name; };
struct Order { int id; int customerId; };

int main() {
    struct Customer customers[] = {
        {1, "Alice"},
        {2, "Bob"},
        {3, "Charlie"}
    };
    struct Order orders[] = {
        {100, 1},
        {101, 1},
        {102, 2}
    };
    int nCustomers = sizeof(customers)/sizeof(customers[0]);
    int nOrders = sizeof(orders)/sizeof(orders[0]);

    printf("--- Group Left Join ---\n");
    for (int i = 0; i < nCustomers; i++) {
        int count = 0;
        for (int j = 0; j < nOrders; j++) {
            if (orders[j].customerId == customers[i].id) {
                count++;
            }
        }
        printf("%s orders: %d\n", customers[i].name, count);
    }
    return 0;
}
