#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int id;
  char *name;
} customersItem;
typedef struct {
  int len;
  customersItem *data;
} list_customersItem;
static list_customersItem list_customersItem_create(int len) {
  list_customersItem l;
  l.len = len;
  l.data = calloc(len, sizeof(customersItem));
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
} ordersItem;
typedef struct {
  int len;
  ordersItem *data;
} list_ordersItem;
static list_ordersItem list_ordersItem_create(int len) {
  list_ordersItem l;
  l.len = len;
  l.data = calloc(len, sizeof(ordersItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int orderId;
  int orderCustomerId;
  char *pairedCustomerName;
  int orderTotal;
} resultItem;
typedef struct {
  int len;
  resultItem *data;
} list_resultItem;
static list_resultItem list_resultItem_create(int len) {
  list_resultItem l;
  l.len = len;
  l.data = calloc(len, sizeof(resultItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  customersItem tmp1_data[] = {(customersItem){.id = 1, .name = "Alice"},
                               (customersItem){.id = 2, .name = "Bob"},
                               (customersItem){.id = 3, .name = "Charlie"}};
  list_customersItem tmp1 = {3, tmp1_data};
  list_customersItem customers = tmp1;
  ordersItem tmp2_data[] = {
      (ordersItem){.id = 100, .customerId = 1, .total = 250},
      (ordersItem){.id = 101, .customerId = 2, .total = 125},
      (ordersItem){.id = 102, .customerId = 1, .total = 300}};
  list_ordersItem tmp2 = {3, tmp2_data};
  list_ordersItem orders = tmp2;
  list_resultItem tmp3 = list_resultItem_create(orders.len * customers.len);
  int tmp4 = 0;
  for (int o_idx = 0; o_idx < orders.len; o_idx++) {
    ordersItem o = orders.data[o_idx];
    for (int c_idx = 0; c_idx < customers.len; c_idx++) {
      customersItem c = customers.data[c_idx];
      tmp3.data[tmp4] = (resultItem){.orderId = o.id,
                                     .orderCustomerId = o.customerId,
                                     .pairedCustomerName = c.name,
                                     .orderTotal = o.total};
      tmp4++;
    }
  }
  tmp3.len = tmp4;
  list_resultItem result = tmp3;
  printf("%s\n", "--- Cross Join: All order-customer pairs ---");
  for (int tmp5 = 0; tmp5 < result.len; tmp5++) {
    resultItem entry = result.data[tmp5];
    printf("%s ", "Order");
    printf("%.16g ", entry.orderId);
    printf("%s ", "(customerId:");
    printf("%.16g ", entry.orderCustomerId);
    printf("%s ", ", total: $");
    printf("%.16g ", entry.orderTotal);
    printf("%s ", ") paired with");
    printf("%s\n", entry.pairedCustomerName);
  }
  return 0;
}
