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
  map_int_bool _t4 = map_int_bool_create(2);
  map_int_bool_put(&_t4, "name", c.name);
  map_int_bool_put(&_t4, "sku", i.sku);
  list_int _t5 = list_int_create(orders.len * customers.len * items.len);
  int _t6 = 0;
  for (int _t7 = 0; _t7 < orders.len; _t7++) {
    ordersItem o = orders.data[_t7];
    for (int _t8 = 0; _t8 < customers.len; _t8++) {
      customersItem c = customers.data[_t8];
      for (int _t9 = 0; _t9 < items.len; _t9++) {
        itemsItem i = items.data[_t9];
        if (!((o.customerId == c.id) && (o.id == i.orderId))) {
          continue;
        }
        _t5.data[_t6] = _t4;
        _t6++;
      }
    }
  }
  _t5.len = _t6;
  list_int result = _t5;
  printf("%s\n", "--- Multi Join ---");
  for (int _t10 = 0; _t10 < result.len; _t10++) {
    int r = result.data[_t10];
    printf("%d ", r.name);
    printf("%s ", "bought item");
    printf("%d\n", r.sku);
  }
  return 0;
}
