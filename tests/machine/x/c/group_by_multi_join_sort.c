#include <stdio.h>
#include <stdlib.h>

static char *start_date = "1993-10-01";
static char *end_date = "1994-01-01";

typedef struct {
  int n_nationkey;
  char *n_name;
} nationItem;
typedef struct {
  int len;
  nationItem *data;
} list_nationItem;
static list_nationItem list_nationItem_create(int len) {
  list_nationItem l;
  l.len = len;
  l.data = (nationItem *)malloc(sizeof(nationItem) * len);
  return l;
}

typedef struct {
  int c_custkey;
  char *c_name;
  double c_acctbal;
  int c_nationkey;
  char *c_address;
  char *c_phone;
  char *c_comment;
} customerItem;
typedef struct {
  int len;
  customerItem *data;
} list_customerItem;
static list_customerItem list_customerItem_create(int len) {
  list_customerItem l;
  l.len = len;
  l.data = (customerItem *)malloc(sizeof(customerItem) * len);
  return l;
}

typedef struct {
  int o_orderkey;
  int o_custkey;
  char *o_orderdate;
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
  int l_orderkey;
  char *l_returnflag;
  double l_extendedprice;
  double l_discount;
} lineitemItem;
typedef struct {
  int len;
  lineitemItem *data;
} list_lineitemItem;
static list_lineitemItem list_lineitemItem_create(int len) {
  list_lineitemItem l;
  l.len = len;
  l.data = (lineitemItem *)malloc(sizeof(lineitemItem) * len);
  return l;
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
} resultItem;
typedef struct {
  int len;
  resultItem *data;
} list_resultItem;
static list_resultItem list_resultItem_create(int len) {
  list_resultItem l;
  l.len = len;
  l.data = (resultItem *)malloc(sizeof(resultItem) * len);
  return l;
}

int main() {
  list_nationItem _t1 = list_nationItem_create(1);
  _t1.data[0] = (nationItem){.n_nationkey = 1, .n_name = "BRAZIL"};
  __auto_type nation = _t1;
  list_customerItem _t2 = list_customerItem_create(1);
  _t2.data[0] = (customerItem){.c_custkey = 1,
                               .c_name = "Alice",
                               .c_acctbal = 100.0,
                               .c_nationkey = 1,
                               .c_address = "123 St",
                               .c_phone = "123-456",
                               .c_comment = "Loyal"};
  __auto_type customer = _t2;
  list_ordersItem _t3 = list_ordersItem_create(2);
  _t3.data[0] = (ordersItem){
      .o_orderkey = 1000, .o_custkey = 1, .o_orderdate = "1993-10-15"};
  _t3.data[1] = (ordersItem){
      .o_orderkey = 2000, .o_custkey = 1, .o_orderdate = "1994-01-02"};
  __auto_type orders = _t3;
  list_lineitemItem _t4 = list_lineitemItem_create(2);
  _t4.data[0] = (lineitemItem){.l_orderkey = 1000,
                               .l_returnflag = "R",
                               .l_extendedprice = 1000.0,
                               .l_discount = 0.1};
  _t4.data[1] = (lineitemItem){.l_orderkey = 2000,
                               .l_returnflag = "N",
                               .l_extendedprice = 500.0,
                               .l_discount = 0.0};
  __auto_type lineitem = _t4;
  list_resultItem result = 0;
  printf("%d\n", result);
  return 0;
}
