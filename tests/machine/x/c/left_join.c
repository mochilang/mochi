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
  int total;
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
    printf(" ");
    printf("total:");
    printf("%d", s.total);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

typedef struct {
  int orderId;
  CustomersItem customer;
  int total;
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
    printf("customer:");
    printf(" ");
    printf("total:");
    printf("%d", s.total);
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
  OrdersItem tmp2_data[] = {
      (OrdersItem){.id = 100, .customerId = 1, .total = 250},
      (OrdersItem){.id = 101, .customerId = 3, .total = 80}};
  list_OrdersItem tmp2 = {2, tmp2_data};
  list_OrdersItem orders = tmp2;
  list_ResultItem tmp3 = list_ResultItem_create(orders.len * customers.len);
  int tmp4 = 0;
  for (int tmp5 = 0; tmp5 < orders.len; tmp5++) {
    OrdersItem o = orders.data[tmp5];
    int tmp7 = 0;
    for (int tmp6 = 0; tmp6 < customers.len; tmp6++) {
      CustomersItem c = customers.data[tmp6];
      if (!(o.customerId == c.id)) {
        continue;
      }
      tmp7 = 1;
      tmp3.data[tmp4] =
          (ResultItem){.orderId = o.id, .customer = c, .total = o.total};
      tmp4++;
    }
    if (!tmp7) {
      CustomersItem c = (CustomersItem){0};
      tmp3.data[tmp4] =
          (ResultItem){.orderId = o.id, .customer = c, .total = o.total};
      tmp4++;
    }
  }
  tmp3.len = tmp4;
  list_ResultItem result = tmp3;
  printf("%s\n", "--- Left Join ---");
  for (int tmp8 = 0; tmp8 < result.len; tmp8++) {
    ResultItem entry = result.data[tmp8];
    printf("%s ", "Order");
    printf("%d ", entry.orderId);
    printf("%s ", "customer");
    printf("%d ", entry.customer);
    printf("%s ", "total");
    printf("%d\n", entry.total);
  }
  return 0;
}
