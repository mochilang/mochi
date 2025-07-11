#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int id;
  char *name;
} CustomersItem;
typedef struct {
  int len;
  CustomersItem *data;
} list_CustomersItem;
static list_CustomersItem list_CustomersItem_create(int len) {
  list_CustomersItem l;
  l.len = len;
  l.data = calloc(len, sizeof(CustomersItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  int customerId;
} OrdersItem;
typedef struct {
  int len;
  OrdersItem *data;
} list_OrdersItem;
static list_OrdersItem list_OrdersItem_create(int len) {
  list_OrdersItem l;
  l.len = len;
  l.data = calloc(len, sizeof(OrdersItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int orderId;
  char *sku;
} ItemsItem;
typedef struct {
  int len;
  ItemsItem *data;
} list_ItemsItem;
static list_ItemsItem list_ItemsItem_create(int len) {
  list_ItemsItem l;
  l.len = len;
  l.data = calloc(len, sizeof(ItemsItem));
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
} ResultItem;
typedef struct {
  int len;
  ResultItem *data;
} list_ResultItem;
static list_ResultItem list_ResultItem_create(int len) {
  list_ResultItem l;
  l.len = len;
  l.data = calloc(len, sizeof(ResultItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  CustomersItem tmp1_data[] = {(CustomersItem){.id = 1, .name = "Alice"},
                               (CustomersItem){.id = 2, .name = "Bob"}};
  list_CustomersItem tmp1 = {2, tmp1_data};
  list_customersItem customers = tmp1;
  OrdersItem tmp2_data[] = {(OrdersItem){.id = 100, .customerId = 1},
                            (OrdersItem){.id = 101, .customerId = 2}};
  list_OrdersItem tmp2 = {2, tmp2_data};
  list_ordersItem orders = tmp2;
  ItemsItem tmp3_data[] = {(ItemsItem){.orderId = 100, .sku = "a"}};
  list_ItemsItem tmp3 = {1, tmp3_data};
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
            (ResultItem){.orderId = o.id, .name = c.name, .item = i};
        tmp5++;
      }
      if (!tmp9) {
        itemsItem i = (itemsItem){0};
        tmp4.data[tmp5] =
            (ResultItem){.orderId = o.id, .name = c.name, .item = i};
        tmp5++;
      }
    }
  }
  tmp4.len = tmp5;
  list_ResultItem result = tmp4;
  printf("%s\n", "--- Left Join Multi ---");
  for (int tmp10 = 0; tmp10 < result.len; tmp10++) {
    resultItem r = result.data[tmp10];
    printf("%d ", r.orderId);
    printf("%s ", r.name);
    printf("%d\n", r.item);
  }
  return 0;
}
