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
  l.data = (resultItem *)malloc(sizeof(resultItem) * len);
  return l;
}

int main() {
  list_customersItem _t1 = list_customersItem_create(3);
  _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
  _t1.data[2] = (customersItem){.id = 3, .name = "Charlie"};
  __auto_type customers = _t1;
  list_ordersItem _t2 = list_ordersItem_create(3);
  _t2.data[0] = (ordersItem){.id = 100, .customerId = 1, .total = 250};
  _t2.data[1] = (ordersItem){.id = 101, .customerId = 2, .total = 125};
  _t2.data[2] = (ordersItem){.id = 102, .customerId = 1, .total = 300};
  __auto_type orders = _t2;
  list_resultItem _t3 = list_resultItem_create(orders.len * customers.len);
  int _t4 = 0;
  for (int _t5 = 0; _t5 < orders.len; _t5++) {
    ordersItem o = orders.data[_t5];
    for (int _t6 = 0; _t6 < customers.len; _t6++) {
      customersItem c = customers.data[_t6];
      _t3.data[_t4] = (resultItem){.orderId = o.id,
                                   .orderCustomerId = o.customerId,
                                   .pairedCustomerName = c.name,
                                   .orderTotal = o.total};
      _t4++;
    }
  }
  _t3.len = _t4;
  list_resultItem result = _t3;
  printf("%s\n", "--- Cross Join: All order-customer pairs ---");
  for (int _t7 = 0; _t7 < result.len; _t7++) {
    resultItem entry = result.data[_t7];
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
