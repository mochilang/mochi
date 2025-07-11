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
  ordersItem order;
  customersItem customer;
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
  list_customersItem customers = tmp1;
  OrdersItem tmp2_data[] = {
      (OrdersItem){.id = 100, .customerId = 1, .total = 250},
      (OrdersItem){.id = 101, .customerId = 2, .total = 125},
      (OrdersItem){.id = 102, .customerId = 1, .total = 300},
      (OrdersItem){.id = 103, .customerId = 5, .total = 80}};
  list_OrdersItem tmp2 = {4, tmp2_data};
  list_ordersItem orders = tmp2;
  list_resultItem tmp3 = list_resultItem_create(orders.len * customers.len);
  int tmp4 = 0;
  for (int tmp5 = 0; tmp5 < orders.len; tmp5++) {
    ordersItem o = orders.data[tmp5];
    for (int tmp6 = 0; tmp6 < customers.len; tmp6++) {
      customersItem c = customers.data[tmp6];
      if (!(o.customerId == c.id)) {
        continue;
      }
      tmp3.data[tmp4] = (ResultItem){.order = o, .customer = c};
      tmp4++;
    }
  }
  tmp3.len = tmp4;
  list_ResultItem result = tmp3;
  printf("%s\n", "--- Outer Join using syntax ---");
  for (int tmp7 = 0; tmp7 < result.len; tmp7++) {
    resultItem row = result.data[tmp7];
    if (row.order) {
      if (row.customer) {
        printf("%s ", "Order");
        printf("%d ", row.order.id);
        printf("%s ", "by");
        printf("%s ", row.customer.name);
        printf("%s ", "- $");
        printf("%d\n", row.order.total);
      } else {
        printf("%s ", "Order");
        printf("%d ", row.order.id);
        printf("%s ", "by");
        printf("%s ", "Unknown");
        printf("%s ", "- $");
        printf("%d\n", row.order.total);
      }
    } else {
      printf("%s ", "Customer");
      printf("%s ", row.customer.name);
      printf("%s\n", "has no orders");
    }
  }
  return 0;
}
