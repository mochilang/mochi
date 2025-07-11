#include <stdio.h>
#include <stdlib.h>

static char *start_date = "1993-10-01";
static char *end_date = "1994-01-01";

typedef struct {
  int n_nationkey;
  char *n_name;
} NationItem;
typedef struct {
  int len;
  NationItem *data;
} list_NationItem;
static list_NationItem list_NationItem_create(int len) {
  list_NationItem l;
  l.len = len;
  l.data = calloc(len, sizeof(NationItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _print_list_nationItem(list_NationItem v) {
  for (int i = 0; i < v.len; i++) {
    NationItem s = v.data[i];
    printf("map[");
    printf("n_nationkey:");
    printf("%d", s.n_nationkey);
    printf(" ");
    printf("n_name:");
    printf("%s", s.n_name);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

typedef struct {
  int c_custkey;
  char *c_name;
  double c_acctbal;
  int c_nationkey;
  char *c_address;
  char *c_phone;
  char *c_comment;
} CustomerItem;
typedef struct {
  int len;
  CustomerItem *data;
} list_CustomerItem;
static list_CustomerItem list_CustomerItem_create(int len) {
  list_CustomerItem l;
  l.len = len;
  l.data = calloc(len, sizeof(CustomerItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _print_list_customerItem(list_CustomerItem v) {
  for (int i = 0; i < v.len; i++) {
    CustomerItem s = v.data[i];
    printf("map[");
    printf("c_custkey:");
    printf("%d", s.c_custkey);
    printf(" ");
    printf("c_name:");
    printf("%s", s.c_name);
    printf(" ");
    printf("c_acctbal:");
    printf("%g", s.c_acctbal);
    printf(" ");
    printf("c_nationkey:");
    printf("%d", s.c_nationkey);
    printf(" ");
    printf("c_address:");
    printf("%s", s.c_address);
    printf(" ");
    printf("c_phone:");
    printf("%s", s.c_phone);
    printf(" ");
    printf("c_comment:");
    printf("%s", s.c_comment);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

typedef struct {
  int o_orderkey;
  int o_custkey;
  char *o_orderdate;
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
    printf("o_orderkey:");
    printf("%d", s.o_orderkey);
    printf(" ");
    printf("o_custkey:");
    printf("%d", s.o_custkey);
    printf(" ");
    printf("o_orderdate:");
    printf("%s", s.o_orderdate);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

typedef struct {
  int l_orderkey;
  char *l_returnflag;
  double l_extendedprice;
  double l_discount;
} LineitemItem;
typedef struct {
  int len;
  LineitemItem *data;
} list_LineitemItem;
static list_LineitemItem list_LineitemItem_create(int len) {
  list_LineitemItem l;
  l.len = len;
  l.data = calloc(len, sizeof(LineitemItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _print_list_lineitemItem(list_LineitemItem v) {
  for (int i = 0; i < v.len; i++) {
    LineitemItem s = v.data[i];
    printf("map[");
    printf("l_orderkey:");
    printf("%d", s.l_orderkey);
    printf(" ");
    printf("l_returnflag:");
    printf("%s", s.l_returnflag);
    printf(" ");
    printf("l_extendedprice:");
    printf("%g", s.l_extendedprice);
    printf(" ");
    printf("l_discount:");
    printf("%g", s.l_discount);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

typedef struct {
  int c_custkey;
  int c_name;
  double revenue;
  int c_acctbal;
  int n_name;
  int c_address;
  int c_phone;
  int c_comment;
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
    printf("c_custkey:");
    printf("%d", s.c_custkey);
    printf(" ");
    printf("c_name:");
    printf("%d", s.c_name);
    printf(" ");
    printf("revenue:");
    printf("%g", s.revenue);
    printf(" ");
    printf("c_acctbal:");
    printf("%d", s.c_acctbal);
    printf(" ");
    printf("n_name:");
    printf("%d", s.n_name);
    printf(" ");
    printf("c_address:");
    printf("%d", s.c_address);
    printf(" ");
    printf("c_phone:");
    printf("%d", s.c_phone);
    printf(" ");
    printf("c_comment:");
    printf("%d", s.c_comment);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

int main() {
  NationItem tmp1_data[] = {(NationItem){.n_nationkey = 1, .n_name = "BRAZIL"}};
  list_NationItem tmp1 = {1, tmp1_data};
  list_NationItem nation = tmp1;
  CustomerItem tmp2_data[] = {(CustomerItem){.c_custkey = 1,
                                             .c_name = "Alice",
                                             .c_acctbal = 100.0,
                                             .c_nationkey = 1,
                                             .c_address = "123 St",
                                             .c_phone = "123-456",
                                             .c_comment = "Loyal"}};
  list_CustomerItem tmp2 = {1, tmp2_data};
  list_CustomerItem customer = tmp2;
  OrdersItem tmp3_data[] = {
      (OrdersItem){
          .o_orderkey = 1000, .o_custkey = 1, .o_orderdate = "1993-10-15"},
      (OrdersItem){
          .o_orderkey = 2000, .o_custkey = 1, .o_orderdate = "1994-01-02"}};
  list_OrdersItem tmp3 = {2, tmp3_data};
  list_OrdersItem orders = tmp3;
  LineitemItem tmp4_data[] = {(LineitemItem){.l_orderkey = 1000,
                                             .l_returnflag = "R",
                                             .l_extendedprice = 1000.0,
                                             .l_discount = 0.1},
                              (LineitemItem){.l_orderkey = 2000,
                                             .l_returnflag = "N",
                                             .l_extendedprice = 500.0,
                                             .l_discount = 0.0}};
  list_LineitemItem tmp4 = {2, tmp4_data};
  list_LineitemItem lineitem = tmp4;
  list_ResultItem result = 0;
  _print_list_resultItem(result);
  printf("\n");
  return 0;
}
