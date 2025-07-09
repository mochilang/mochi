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
  char *sku;
} itemsItem;
typedef struct {
  int len;
  itemsItem *data;
} list_itemsItem;
static list_itemsItem list_itemsItem_create(int len) {
  list_itemsItem l;
  l.len = len;
  l.data = (itemsItem *)malloc(sizeof(itemsItem) * len);
  return l;
}

typedef struct {
  int name;
  int sku;
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
  list_customersItem _t1 = list_customersItem_create(2);
  _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
  list_customersItem customers = _t1;
  list_ordersItem _t2 = list_ordersItem_create(2);
  _t2.data[0] = (ordersItem){.id = 100, .customerId = 1};
  _t2.data[1] = (ordersItem){.id = 101, .customerId = 2};
  list_ordersItem orders = _t2;
  list_itemsItem _t3 = list_itemsItem_create(2);
  _t3.data[0] = (itemsItem){.orderId = 100, .sku = "a"};
  _t3.data[1] = (itemsItem){.orderId = 101, .sku = "b"};
  list_itemsItem items = _t3;
  list_int _t4 = list_int_create(orders.len * customers.len * items.len);
  int _t5 = 0;
  for (int _t6 = 0; _t6 < orders.len; _t6++) {
    ordersItem o = orders.data[_t6];
    for (int _t7 = 0; _t7 < customers.len; _t7++) {
      customersItem c = customers.data[_t7];
      if (!(o.customerId == c.id)) {
        continue;
      }
      for (int _t8 = 0; _t8 < items.len; _t8++) {
        itemsItem i = items.data[_t8];
        if (!(o.id == i.orderId)) {
          continue;
        }
        _t4.data[_t5] = (resultItem){.name = c.name, .sku = i.sku};
        _t5++;
      }
    }
  }
  _t4.len = _t5;
  list_resultItem result = _t4;
  printf("%s\n", "--- Multi Join ---");
  for (int _t9 = 0; _t9 < result.len; _t9++) {
    resultItem r = result.data[_t9];
    printf("%d ", r.name);
    printf("%s ", "bought item");
    printf("%d\n", r.sku);
  }
  return 0;
}
