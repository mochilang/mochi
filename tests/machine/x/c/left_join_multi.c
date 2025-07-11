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
static void _print_list_customersItem(list_CustomersItem v) {
  for (int i = 0; i < v.len; i++) {
    CustomersItem s = v.data[i];
    printf("map[");
    printf("id:");
    printf("%d", s.id);
    printf(" ");
    printf("name:");
    printf("%s", s.name);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
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
static void _print_list_ordersItem(list_OrdersItem v) {
  for (int i = 0; i < v.len; i++) {
    OrdersItem s = v.data[i];
    printf("map[");
    printf("id:");
    printf("%d", s.id);
    printf(" ");
    printf("customerId:");
    printf("%d", s.customerId);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
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
static void _print_list_itemsItem(list_ItemsItem v) {
  for (int i = 0; i < v.len; i++) {
    ItemsItem s = v.data[i];
    printf("map[");
    printf("orderId:");
    printf("%d", s.orderId);
    printf(" ");
    printf("sku:");
    printf("%s", s.sku);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

typedef struct {
  int orderId;
  char *name;
  ItemsItem item;
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
static void _print_list_resultItem(list_ResultItem v) {
  for (int i = 0; i < v.len; i++) {
    ResultItem s = v.data[i];
    printf("map[");
    printf("orderId:");
    printf("%d", s.orderId);
    printf(" ");
    printf("name:");
    printf("%s", s.name);
    printf(" ");
    printf("item:");
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

int main() {
  CustomersItem tmp1_data[] = {(CustomersItem){.id = 1, .name = "Alice"},
                               (CustomersItem){.id = 2, .name = "Bob"}};
  list_CustomersItem tmp1 = {2, tmp1_data};
  list_CustomersItem customers = tmp1;
  OrdersItem tmp2_data[] = {(OrdersItem){.id = 100, .customerId = 1},
                            (OrdersItem){.id = 101, .customerId = 2}};
  list_OrdersItem tmp2 = {2, tmp2_data};
  list_OrdersItem orders = tmp2;
  ItemsItem tmp3_data[] = {(ItemsItem){.orderId = 100, .sku = "a"}};
  list_ItemsItem tmp3 = {1, tmp3_data};
  list_ItemsItem items = tmp3;
  list_ResultItem tmp4 =
      list_ResultItem_create(orders.len * customers.len * items.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < orders.len; tmp6++) {
    OrdersItem o = orders.data[tmp6];
    for (int tmp7 = 0; tmp7 < customers.len; tmp7++) {
      CustomersItem c = customers.data[tmp7];
      if (!(o.customerId == c.id)) {
        continue;
      }
      int tmp9 = 0;
      for (int tmp8 = 0; tmp8 < items.len; tmp8++) {
        ItemsItem i = items.data[tmp8];
        if (!(o.id == i.orderId)) {
          continue;
        }
        tmp9 = 1;
        tmp4.data[tmp5] =
            (ResultItem){.orderId = o.id, .name = c.name, .item = i};
        tmp5++;
      }
      if (!tmp9) {
        ItemsItem i = (ItemsItem){0};
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
    ResultItem r = result.data[tmp10];
    printf("%d ", r.orderId);
    printf("%s ", r.name);
    printf("%d\n", r.item);
  }
  return 0;
}
