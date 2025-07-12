#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int id;
  char *name;
} CustomersItem;
typedef struct {
  int len;
  CustomersItem *data;
} list_CustomersItem;
static list_CustomersItem list_CustomersItem_create(int len) {
  list_CustomersItem l;
  l.len = len;
  l.data = calloc(len, sizeof(CustomersItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  int customerId;
} OrdersItem;
typedef struct {
  int len;
  OrdersItem *data;
} list_OrdersItem;
static list_OrdersItem list_OrdersItem_create(int len) {
  list_OrdersItem l;
  l.len = len;
  l.data = calloc(len, sizeof(OrdersItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  char *name;
  int count;
} StatsItem;
typedef struct {
  int len;
  StatsItem *data;
} list_StatsItem;
static list_StatsItem list_StatsItem_create(int len) {
  list_StatsItem l;
  l.len = len;
  l.data = calloc(len, sizeof(StatsItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  CustomersItem tmp1_data[] = {(CustomersItem){.id = 1, .name = "Alice"},
                               (CustomersItem){.id = 2, .name = "Bob"},
                               (CustomersItem){.id = 3, .name = "Charlie"}};
  list_CustomersItem tmp1 = {3, tmp1_data};
  list_CustomersItem customers = tmp1;
  OrdersItem tmp2_data[] = {(OrdersItem){.id = 100, .customerId = 1},
                            (OrdersItem){.id = 101, .customerId = 1},
                            (OrdersItem){.id = 102, .customerId = 2}};
  list_OrdersItem tmp2 = {3, tmp2_data};
  list_OrdersItem orders = tmp2;
  list_StatsItem stats = 0;
  printf("%s\n", "--- Group Left Join ---");
  // unsupported dynamic list iteration
  for (;;) {
    break;
  }
  return 0;
}
