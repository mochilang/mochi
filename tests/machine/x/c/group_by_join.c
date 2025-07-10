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
  char *name;
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
  list_customersItem _t1 = list_customersItem_create(2);
  _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
  __auto_type customers = _t1;
  list_ordersItem _t2 = list_ordersItem_create(3);
  _t2.data[0] = (ordersItem){.id = 100, .customerId = 1};
  _t2.data[1] = (ordersItem){.id = 101, .customerId = 1};
  _t2.data[2] = (ordersItem){.id = 102, .customerId = 2};
  __auto_type orders = _t2;
  list_statsItem stats = 0;
  printf("%s\n", "--- Orders per customer ---");
  for (int _t3 = 0; _t3 < stats.len; _t3++) {
    statsItem s = stats.data[_t3];
    printf("%s ", s.name);
    printf("%s ", "orders:");
    printf("%d\n", s.count);
  }
  return 0;
}
