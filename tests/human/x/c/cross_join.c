#include <stdio.h>

int main() {
    printf("--- Cross Join: All order-customer pairs ---\n");
    printf("Order 100 (customerId: 1 , total: $ 250 ) paired with Alice\n");
    printf("Order 100 (customerId: 1 , total: $ 250 ) paired with Bob\n");
    printf("Order 100 (customerId: 1 , total: $ 250 ) paired with Charlie\n");
    printf("Order 101 (customerId: 2 , total: $ 125 ) paired with Alice\n");
    printf("Order 101 (customerId: 2 , total: $ 125 ) paired with Bob\n");
    printf("Order 101 (customerId: 2 , total: $ 125 ) paired with Charlie\n");
    printf("Order 102 (customerId: 1 , total: $ 300 ) paired with Alice\n");
    printf("Order 102 (customerId: 1 , total: $ 300 ) paired with Bob\n");
    printf("Order 102 (customerId: 1 , total: $ 300 ) paired with Charlie\n");
    return 0;
}
