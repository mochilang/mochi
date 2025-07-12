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
  int total;
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
  char *customerName;
  OrdersItem order;
} ResultItem;
typedef struct {
  int len;
  ResultItem *data;
} list_ResultItem;
static list_ResultItem list_ResultItem_create(int len) {
  list_ResultItem l;
  l.len = len;
  l.data = calloc(len, sizeof(ResultItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  CustomersItem tmp1_data[] = {(CustomersItem){.id = 1, .name = "Alice"},
                               (CustomersItem){.id = 2, .name = "Bob"},
                               (CustomersItem){.id = 3, .name = "Charlie"},
                               (CustomersItem){.id = 4, .name = "Diana"}};
  list_CustomersItem tmp1 = {4, tmp1_data};
  list_CustomersItem customers = tmp1;
  OrdersItem tmp2_data[] = {
      (OrdersItem){.id = 100, .customerId = 1, .total = 250},
      (OrdersItem){.id = 101, .customerId = 2, .total = 125},
      (OrdersItem){.id = 102, .customerId = 1, .total = 300}};
  list_OrdersItem tmp2 = {3, tmp2_data};
  list_OrdersItem orders = tmp2;
  list_ResultItem tmp3 = list_ResultItem_create(orders.len * customers.len);
  int tmp4 = 0;
  for (int tmp5 = 0; tmp5 < orders.len; tmp5++) {
    OrdersItem o = orders.data[tmp5];
    int tmp6 = 0;
    for (int tmp7 = 0; tmp7 < customers.len; tmp7++) {
      CustomersItem c = customers.data[tmp7];
      if (!(o.customerId == c.id)) {
        continue;
      }
      tmp6 = 1;
      tmp3.data[tmp4] = (ResultItem){.customerName = c.name, .order = o};
      tmp4++;
    }
    if (!tmp6) {
      CustomersItem c = (CustomersItem){0};
      tmp3.data[tmp4] = (ResultItem){.customerName = c.name, .order = o};
      tmp4++;
    }
  }
  tmp3.len = tmp4;
  list_ResultItem result = tmp3;
  printf("%s\n", "--- Right Join using syntax ---");
  // unsupported dynamic list iteration
  for (;;) {
    break;
  }
  return 0;
}
