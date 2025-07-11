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
  customersItem customer;
  int total;
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
                               (customersItem){.id = 2, .name = "Bob"}};
  list_customersItem tmp1 = {2, tmp1_data};
  list_customersItem customers = tmp1;
  ordersItem tmp2_data[] = {
      (ordersItem){.id = 100, .customerId = 1, .total = 250},
      (ordersItem){.id = 101, .customerId = 3, .total = 80}};
  list_ordersItem tmp2 = {2, tmp2_data};
  list_ordersItem orders = tmp2;
  list_resultItem tmp3 = list_resultItem_create(orders.len * customers.len);
  int tmp4 = 0;
  for (int tmp5 = 0; tmp5 < orders.len; tmp5++) {
    ordersItem o = orders.data[tmp5];
    int tmp7 = 0;
    for (int tmp6 = 0; tmp6 < customers.len; tmp6++) {
      customersItem c = customers.data[tmp6];
      if (!(o.customerId == c.id)) {
        continue;
      }
      tmp7 = 1;
      tmp3.data[tmp4] =
          (resultItem){.orderId = o.id, .customer = c, .total = o.total};
      tmp4++;
    }
    if (!tmp7) {
      customersItem c = (customersItem){0};
      tmp3.data[tmp4] =
          (resultItem){.orderId = o.id, .customer = c, .total = o.total};
      tmp4++;
    }
  }
  tmp3.len = tmp4;
  list_resultItem result = tmp3;
  printf("%s\n", "--- Left Join ---");
  for (int tmp8 = 0; tmp8 < result.len; tmp8++) {
    resultItem entry = result.data[tmp8];
    printf("%s ", "Order");
    printf("%.16g ", entry.orderId);
    printf("%s ", "customer");
    printf("%.16g ", entry.customer);
    printf("%s ", "total");
    printf("%.16g\n", entry.total);
  }
  return 0;
}
