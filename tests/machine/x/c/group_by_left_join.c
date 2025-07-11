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
  l.data = calloc(len, sizeof(statsItem));
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
  ordersItem tmp2_data[] = {(ordersItem){.id = 100, .customerId = 1},
                            (ordersItem){.id = 101, .customerId = 1},
                            (ordersItem){.id = 102, .customerId = 2}};
  list_ordersItem tmp2 = {3, tmp2_data};
  list_ordersItem orders = tmp2;
  list_statsItem stats = 0;
  printf("%s\n", "--- Group Left Join ---");
  for (int tmp3 = 0; tmp3 < stats.len; tmp3++) {
    statsItem s = stats.data[tmp3];
    printf("%s ", s.name);
    printf("%s ", "orders:");
    printf("%.16g\n", s.count);
  }
  return 0;
}
