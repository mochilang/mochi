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
  l.data = calloc(len, sizeof(itemsItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int orderId;
  char *name;
  itemsItem item;
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
  ordersItem tmp2_data[] = {(ordersItem){.id = 100, .customerId = 1},
                            (ordersItem){.id = 101, .customerId = 2}};
  list_ordersItem tmp2 = {2, tmp2_data};
  list_ordersItem orders = tmp2;
  itemsItem tmp3_data[] = {(itemsItem){.orderId = 100, .sku = "a"}};
  list_itemsItem tmp3 = {1, tmp3_data};
  list_itemsItem items = tmp3;
  list_resultItem tmp4 =
      list_resultItem_create(orders.len * customers.len * items.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < orders.len; tmp6++) {
    ordersItem o = orders.data[tmp6];
    for (int tmp7 = 0; tmp7 < customers.len; tmp7++) {
      customersItem c = customers.data[tmp7];
      if (!(o.customerId == c.id)) {
        continue;
      }
      int tmp9 = 0;
      for (int tmp8 = 0; tmp8 < items.len; tmp8++) {
        itemsItem i = items.data[tmp8];
        if (!(o.id == i.orderId)) {
          continue;
        }
        tmp9 = 1;
        tmp4.data[tmp5] =
            (resultItem){.orderId = o.id, .name = c.name, .item = i};
        tmp5++;
      }
      if (!tmp9) {
        itemsItem i = (itemsItem){0};
        tmp4.data[tmp5] =
            (resultItem){.orderId = o.id, .name = c.name, .item = i};
        tmp5++;
      }
    }
  }
  tmp4.len = tmp5;
  list_resultItem result = tmp4;
  printf("%s\n", "--- Left Join Multi ---");
  for (int tmp10 = 0; tmp10 < result.len; tmp10++) {
    resultItem r = result.data[tmp10];
    printf("%.16g ", r.orderId);
    printf("%s ", r.name);
    printf("%.16g\n", r.item);
  }
  return 0;
}
