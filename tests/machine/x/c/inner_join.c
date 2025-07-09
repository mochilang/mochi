#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int len;
  int cap;
  int *data;
} list_int;
static list_int list_int_create(int len) {
  list_int l;
  l.len = len;
  l.cap = len;
  l.data = len ? (int *)malloc(sizeof(int) * len) : NULL;
  return l;
}
static void list_int_free(list_int *l) {
  free(l->data);
  l->data = NULL;
  l->len = 0;
  l->cap = 0;
}
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
  l.data = (customersItem *)malloc(sizeof(customersItem) * len);
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
  l.data = (ordersItem *)malloc(sizeof(ordersItem) * len);
  return l;
}

typedef struct {
  int orderId;
  int customerName;
  int total;
} resultItem;
typedef struct {
  int len;
  resultItem *data;
} list_resultItem;
static list_resultItem list_resultItem_create(int len) {
  list_resultItem l;
  l.len = len;
  l.data = (resultItem *)malloc(sizeof(resultItem) * len);
  return l;
}

int main() {
  list_customersItem _t1 = list_customersItem_create(3);
  _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
  _t1.data[2] = (customersItem){.id = 3, .name = "Charlie"};
  list_customersItem customers = _t1;
  list_ordersItem _t2 = list_ordersItem_create(4);
  _t2.data[0] = (ordersItem){.id = 100, .customerId = 1, .total = 250};
  _t2.data[1] = (ordersItem){.id = 101, .customerId = 2, .total = 125};
  _t2.data[2] = (ordersItem){.id = 102, .customerId = 1, .total = 300};
  _t2.data[3] = (ordersItem){.id = 103, .customerId = 4, .total = 80};
  list_ordersItem orders = _t2;
  list_int _t3 = list_int_create(orders.len * customers.len);
  int _t4 = 0;
  for (int _t5 = 0; _t5 < orders.len; _t5++) {
    ordersItem o = orders.data[_t5];
    for (int _t6 = 0; _t6 < customers.len; _t6++) {
      customersItem c = customers.data[_t6];
      if (!((o.customerId == c.id))) {
        continue;
      }
      _t3.data[_t4] = (resultItem){
          .orderId = o.id, .customerName = c.name, .total = o.total};
      _t4++;
    }
  }
  _t3.len = _t4;
  list_resultItem result = _t3;
  printf("%s\n", "--- Orders with customer info ---");
  for (int _t7 = 0; _t7 < result.len; _t7++) {
    resultItem entry = result.data[_t7];
    printf("%s ", "Order");
    printf("%d ", entry.orderId);
    printf("%s ", "by");
    printf("%d ", entry.customerName);
    printf("%s ", "- $");
    printf("%d\n", entry.total);
  }
  return 0;
}
