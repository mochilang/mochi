// Generated by Mochi 0.10.32 on 2025-07-22 13:55 +0700
#include <stdio.h>
#include <string.h>
typedef struct Anon10 Anon10;
typedef struct Anon7 Anon7;
typedef struct Anon8 Anon8;
typedef struct Anon9 Anon9;
typedef struct Customers Customers;
typedef struct Data2 Data2;
typedef struct Data4 Data4;
typedef struct Data6 Data6;
typedef struct Items Items;
typedef struct Orders Orders;
struct Anon10 {
    const char *item;
    const char *name;
    int orderId;
};
struct Anon7 {
    int customerId;
    int id;
};
struct Anon8 {
    int id;
    const char *name;
};
struct Anon9 {
    int orderId;
    const char *sku;
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
};
struct Data6 {
    int orderId;
    const char *sku;
};
struct Items {
    int orderId;
    const char *sku;
};
struct Orders {
    int id;
    int customerId;
};
Customers customers[] = {(Customers){.id = 1,.name = "Alice"},(Customers){.id = 2,.name = "Bob"}};
Orders orders[] = {(Orders){.id = 100,.customerId = 1},(Orders){.id = 101,.customerId = 2}};
Items items[] = {(Items){.orderId = 100,.sku = "a"}};
Anon10 result[] = {(Anon10){.item = "{'orderId': 100, 'sku': 'a'}",.name = "Alice",.orderId = 100},(Anon10){.item = "None",.name = "Bob",.orderId = 101}};
int main(void) {
    puts("--- Left Join Multi ---");
    {
        Anon10 r_arr[] = {(Anon10){.item = "{'orderId': 100, 'sku': 'a'}",.name = "Alice",.orderId = 100},(Anon10){.item = "None",.name = "Bob",.orderId = 101}};
        size_t r_len = sizeof((r_arr)) / sizeof((r_arr[0]));
        for (size_t i = 0; i < r_len; i++) {
            Anon10 r = r_arr[i];
            printf("%d %s %s\n", r.orderId, r.name, r.item);
        }
    }
    return 0;
}
