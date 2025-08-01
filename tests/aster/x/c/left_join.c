// Generated by Mochi 0.10.32 on 2025-07-22 13:40 +0700
#include <stdio.h>
#include <string.h>
typedef struct Anon5 Anon5;
typedef struct Anon6 Anon6;
typedef struct Anon7 Anon7;
typedef struct Customers Customers;
typedef struct Data2 Data2;
typedef struct Data4 Data4;
typedef struct Orders Orders;
struct Anon5 {
    int customerId;
    int id;
    int total;
};
struct Anon6 {
    int id;
    const char *name;
};
struct Anon7 {
    const char *customer;
    int orderId;
    int total;
};
struct Customers {
    int id;
    const char *name;
};
struct Data2 {
    int id;
    const char *name;
};
struct Data4 {
    int id;
    int customerId;
    int total;
};
struct Orders {
    int id;
    int customerId;
    int total;
};
Customers customers[] = {(Customers){.id = 1,.name = "Alice"},(Customers){.id = 2,.name = "Bob"}};
Orders orders[] = {(Orders){.id = 100,.customerId = 1,.total = 250},(Orders){.id = 101,.customerId = 3,.total = 80}};
Anon7 result[] = {(Anon7){.customer = "{'id': 1, 'name': 'Alice'}",.orderId = 100,.total = 250},(Anon7){.customer = "None",.orderId = 101,.total = 80}};
int main(void) {
    puts("--- Left Join ---");
    {
        Anon7 entry_arr[] = {(Anon7){.customer = "{'id': 1, 'name': 'Alice'}",.orderId = 100,.total = 250},(Anon7){.customer = "None",.orderId = 101,.total = 80}};
        size_t entry_len = sizeof((entry_arr)) / sizeof((entry_arr[0]));
        for (size_t i = 0; i < entry_len; i++) {
            Anon7 entry = entry_arr[i];
            printf("%s %d %s %s %s %d\n", "Order", entry.orderId, "customer", entry.customer, "total", entry.total);
        }
    }
    return 0;
}
