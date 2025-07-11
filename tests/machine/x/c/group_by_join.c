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
static void _print_list_customersItem(list_CustomersItem v) {
  for (int i = 0; i < v.len; i++) {
    CustomersItem s = v.data[i];
    printf("map[");
    printf("id:");
    printf("%d", s.id);
    printf(" ");
    printf("name:");
    printf("%s", s.name);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
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
static void _print_list_ordersItem(list_OrdersItem v) {
  for (int i = 0; i < v.len; i++) {
    OrdersItem s = v.data[i];
    printf("map[");
    printf("id:");
    printf("%d", s.id);
    printf(" ");
    printf("customerId:");
    printf("%d", s.customerId);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
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
static void _print_list_statsItem(list_StatsItem v) {
  for (int i = 0; i < v.len; i++) {
    StatsItem s = v.data[i];
    printf("map[");
    printf("name:");
    printf("%s", s.name);
    printf(" ");
    printf("count:");
    printf("%d", s.count);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

int main() {
  CustomersItem tmp1_data[] = {(CustomersItem){.id = 1, .name = "Alice"},
                               (CustomersItem){.id = 2, .name = "Bob"}};
  list_CustomersItem tmp1 = {2, tmp1_data};
  list_CustomersItem customers = tmp1;
  OrdersItem tmp2_data[] = {(OrdersItem){.id = 100, .customerId = 1},
                            (OrdersItem){.id = 101, .customerId = 1},
                            (OrdersItem){.id = 102, .customerId = 2}};
  list_OrdersItem tmp2 = {3, tmp2_data};
  list_OrdersItem orders = tmp2;
  list_StatsItem stats = 0;
  printf("%s\n", "--- Orders per customer ---");
  for (int tmp3 = 0; tmp3 < stats.len; tmp3++) {
    StatsItem s = stats.data[tmp3];
    printf("%s ", s.name);
    printf("%s ", "orders:");
    printf("%d\n", s.count);
  }
  return 0;
}
