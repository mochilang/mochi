#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int len;
  int *data;
} list_int;
static list_int list_int_create(int len) {
  list_int l;
  l.len = len;
  l.data = (int *)malloc(sizeof(int) * len);
  return l;
}
typedef struct {
  int key;
  int value;
} map_int_bool_item;
static map_int_bool_item *map_int_bool_item_new(int key, int value) {
  map_int_bool_item *it =
      (map_int_bool_item *)malloc(sizeof(map_int_bool_item));
  it->key = key;
  it->value = value;
  return it;
}
typedef struct {
  int len;
  int cap;
  map_int_bool_item **data;
} map_int_bool;
static map_int_bool map_int_bool_create(int cap) {
  map_int_bool m;
  m.len = 0;
  m.cap = cap;
  m.data = cap ? (map_int_bool_item **)malloc(sizeof(map_int_bool_item *) * cap)
               : NULL;
  return m;
}
static void map_int_bool_put(map_int_bool *m, int key, int value) {
  for (int i = 0; i < m->len; i++)
    if (m->data[i]->key == key) {
      m->data[i]->value = value;
      return;
    }
  if (m->len >= m->cap) {
    m->cap = m->cap ? m->cap * 2 : 4;
    m->data = (map_int_bool_item **)realloc(
        m->data, sizeof(map_int_bool_item *) * m->cap);
  }
  m->data[m->len++] = map_int_bool_item_new(key, value);
}
static int map_int_bool_contains(map_int_bool m, int key) {
  for (int i = 0; i < m.len; i++)
    if (m.data[i]->key == key)
      return 1;
  return 0;
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

int main() {
  list_customersItem _t1 = list_customersItem_create(3);
  _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
  _t1.data[2] = (customersItem){.id = 3, .name = "Charlie"};
  list_customersItem customers = _t1;
  list_ordersItem _t2 = list_ordersItem_create(3);
  _t2.data[0] = (ordersItem){.id = 100, .customerId = 1, .total = 250};
  _t2.data[1] = (ordersItem){.id = 101, .customerId = 2, .total = 125};
  _t2.data[2] = (ordersItem){.id = 102, .customerId = 1, .total = 300};
  list_ordersItem orders = _t2;
  map_int_bool _t3 = map_int_bool_create(4);
  map_int_bool_put(&_t3, "orderId", o.id);
  map_int_bool_put(&_t3, "orderCustomerId", o.customerId);
  map_int_bool_put(&_t3, "pairedCustomerName", c.name);
  map_int_bool_put(&_t3, "orderTotal", o.total);
  list_int _t4 = list_int_create(orders.len * customers.len);
  int _t5 = 0;
  for (int _t6 = 0; _t6 < orders.len; _t6++) {
    ordersItem o = orders.data[_t6];
    for (int _t7 = 0; _t7 < customers.len; _t7++) {
      customersItem c = customers.data[_t7];
      _t4.data[_t5] = _t3;
      _t5++;
    }
  }
  _t4.len = _t5;
  list_int result = _t4;
  printf("%s\n", "--- Cross Join: All order-customer pairs ---");
  for (int _t8 = 0; _t8 < result.len; _t8++) {
    int entry = result.data[_t8];
    printf("%s ", "Order");
    printf("%d ", entry.orderId);
    printf("%s ", "(customerId:");
    printf("%d ", entry.orderCustomerId);
    printf("%s ", ", total: $");
    printf("%d ", entry.orderTotal);
    printf("%s ", ") paired with");
    printf("%d\n", entry.pairedCustomerName);
  }
  return 0;
}
