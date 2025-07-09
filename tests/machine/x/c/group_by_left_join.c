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
  int name;
  int count;
} statsItem;
typedef struct {
  int len;
  statsItem *data;
} list_statsItem;
static list_statsItem list_statsItem_create(int len) {
  list_statsItem l;
  l.len = len;
  l.data = (statsItem *)malloc(sizeof(statsItem) * len);
  return l;
}

int main() {
  list_customersItem _t1 = list_customersItem_create(3);
  _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
  _t1.data[2] = (customersItem){.id = 3, .name = "Charlie"};
  list_customersItem customers = _t1;
  list_ordersItem _t2 = list_ordersItem_create(3);
  _t2.data[0] = (ordersItem){.id = 100, .customerId = 1};
  _t2.data[1] = (ordersItem){.id = 101, .customerId = 1};
  _t2.data[2] = (ordersItem){.id = 102, .customerId = 2};
  list_ordersItem orders = _t2;
  list_statsItem stats = 0;
  printf("%s\n", "--- Group Left Join ---");
  for (int _t3 = 0; _t3 < stats.len; _t3++) {
    statsItem s = stats.data[_t3];
    printf("%d ", s.name);
    printf("%s ", "orders:");
    printf("%d\n", s.count);
  }
  return 0;
}
