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
  OrdersItem order;
  CustomersItem customer;
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
      (OrdersItem){.id = 102, .customerId = 1, .total = 300},
      (OrdersItem){.id = 103, .customerId = 5, .total = 80}};
  list_OrdersItem tmp2 = {4, tmp2_data};
  list_OrdersItem orders = tmp2;
  list_ResultItem tmp3 = list_ResultItem_create(orders.len * customers.len +
                                                orders.len + customers.len);
  int tmp4 = 0;
  int *tmp5 = calloc(orders.len, sizeof(int));
  int *tmp6 = calloc(customers.len, sizeof(int));
  for (int tmp7 = 0; tmp7 < orders.len; tmp7++) {
    OrdersItem o = orders.data[tmp7];
    for (int tmp8 = 0; tmp8 < customers.len; tmp8++) {
      CustomersItem c = customers.data[tmp8];
      if (!(o.customerId == c.id)) {
        continue;
      }
      tmp5[tmp7] = 1;
      tmp6[tmp8] = 1;
      tmp3.data[tmp4] = (ResultItem){.order = o, .customer = c};
      tmp4++;
    }
  }
  for (int tmp7 = 0; tmp7 < orders.len; tmp7++) {
    if (tmp5[tmp7])
      continue;
    OrdersItem o = orders.data[tmp7];
    CustomersItem c = (CustomersItem){0};
    tmp3.data[tmp4] = (ResultItem){.order = o, .customer = c};
    tmp4++;
  }
  for (int tmp9 = 0; tmp9 < customers.len; tmp9++) {
    if (tmp6[tmp9])
      continue;
    CustomersItem c = customers.data[tmp9];
    OrdersItem o = (OrdersItem){0};
    tmp3.data[tmp4] = (ResultItem){.order = o, .customer = c};
    tmp4++;
  }
  free(tmp5);
  free(tmp6);
  tmp3.len = tmp4;
  list_ResultItem result = tmp3;
  printf("%s\n", "--- Outer Join using syntax ---");
  for (int tmp10 = 0; tmp10 < result.len; tmp10++) {
    ResultItem row = result.data[tmp10];
    if (row.order) {
      if (row.customer) {
        printf("%s ", "Order");
        printf("%.16g ", row.order.id);
        printf("%s ", "by");
        printf("%s ", row.customer.name);
        printf("%s ", "- $");
        printf("%.16g\n", row.order.total);
      } else {
        printf("%s ", "Order");
        printf("%.16g ", row.order.id);
        printf("%s ", "by");
        printf("%s ", "Unknown");
        printf("%s ", "- $");
        printf("%.16g\n", row.order.total);
      }
    } else {
      printf("%s ", "Customer");
      printf("%s ", row.customer.name);
      printf("%s\n", "has no orders");
    }
  }
  return 0;
}
