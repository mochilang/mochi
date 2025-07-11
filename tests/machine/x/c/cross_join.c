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
static void _print_list_ordersItem(list_OrdersItem v) {
  for (int i = 0; i < v.len; i++) {
    OrdersItem s = v.data[i];
    printf("map[");
    printf("id:");
    printf("%d", s.id);
    printf(" ");
    printf("customerId:");
    printf("%d", s.customerId);
    printf(" ");
    printf("total:");
    printf("%d", s.total);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

typedef struct {
  int orderId;
  int orderCustomerId;
  char *pairedCustomerName;
  int orderTotal;
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
static void _print_list_resultItem(list_ResultItem v) {
  for (int i = 0; i < v.len; i++) {
    ResultItem s = v.data[i];
    printf("map[");
    printf("orderId:");
    printf("%d", s.orderId);
    printf(" ");
    printf("orderCustomerId:");
    printf("%d", s.orderCustomerId);
    printf(" ");
    printf("pairedCustomerName:");
    printf("%s", s.pairedCustomerName);
    printf(" ");
    printf("orderTotal:");
    printf("%d", s.orderTotal);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

int main() {
  CustomersItem tmp1_data[] = {(CustomersItem){.id = 1, .name = "Alice"},
                               (CustomersItem){.id = 2, .name = "Bob"},
                               (CustomersItem){.id = 3, .name = "Charlie"}};
  list_CustomersItem tmp1 = {3, tmp1_data};
  list_CustomersItem customers = tmp1;
  OrdersItem tmp2_data[] = {
      (OrdersItem){.id = 100, .customerId = 1, .total = 250},
      (OrdersItem){.id = 101, .customerId = 2, .total = 125},
      (OrdersItem){.id = 102, .customerId = 1, .total = 300}};
  list_OrdersItem tmp2 = {3, tmp2_data};
  list_OrdersItem orders = tmp2;
  list_ResultItem tmp3 = list_ResultItem_create(orders.len * customers.len);
  int tmp4 = 0;
  for (int o_idx = 0; o_idx < orders.len; o_idx++) {
    OrdersItem o = orders.data[o_idx];
    for (int c_idx = 0; c_idx < customers.len; c_idx++) {
      CustomersItem c = customers.data[c_idx];
      tmp3.data[tmp4] = (ResultItem){.orderId = o.id,
                                     .orderCustomerId = o.customerId,
                                     .pairedCustomerName = c.name,
                                     .orderTotal = o.total};
      tmp4++;
    }
  }
  tmp3.len = tmp4;
  list_ResultItem result = tmp3;
  printf("%s\n", "--- Cross Join: All order-customer pairs ---");
  for (int tmp5 = 0; tmp5 < result.len; tmp5++) {
    ResultItem entry = result.data[tmp5];
    printf("%s ", "Order");
    printf("%d ", entry.orderId);
    printf("%s ", "(customerId:");
    printf("%d ", entry.orderCustomerId);
    printf("%s ", ", total: $");
    printf("%d ", entry.orderTotal);
    printf("%s ", ") paired with");
    printf("%s\n", entry.pairedCustomerName);
  }
  return 0;
}
