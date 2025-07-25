// Generated by Mochi compiler v0.10.26 on 2006-01-02T15:04:05Z
// Generated by Mochi compiler v0.10.26 on 2006-01-02T15:04:05Z
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  int len;
  int *data;
} list_int;
static list_int list_int_create(int len) {
  list_int l;
  l.len = len;
  l.data = calloc(len, sizeof(int));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
typedef struct {
  int len;
  double *data;
} list_float;
static list_float list_float_create(int len) {
  list_float l;
  l.len = len;
  l.data = calloc(len, sizeof(double));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
typedef struct {
  int len;
  char **data;
} list_string;
static list_string list_string_create(int len) {
  list_string l;
  l.len = len;
  l.data = calloc(len, sizeof(char *));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
typedef struct {
  int len;
  list_int *data;
} list_list_int;
static list_list_int list_list_int_create(int len) {
  list_list_int l;
  l.len = len;
  l.data = calloc(len, sizeof(list_int));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _json_int(int v) { printf("%d", v); }
static void _json_float(double v) { printf("%g", v); }
static void _json_string(char *s) { printf("\"%s\"", s); }
static void _json_list_int(list_int v) {
  printf("[");
  for (int i = 0; i < v.len; i++) {
    if (i > 0)
      printf(",");
    _json_int(v.data[i]);
  }
  printf("]");
}
static void _json_list_float(list_float v) {
  printf("[");
  for (int i = 0; i < v.len; i++) {
    if (i > 0)
      printf(",");
    _json_float(v.data[i]);
  }
  printf("]");
}
static void _json_list_string(list_string v) {
  printf("[");
  for (int i = 0; i < v.len; i++) {
    if (i > 0)
      printf(",");
    _json_string(v.data[i]);
  }
  printf("]");
}
static void _json_list_list_int(list_list_int v) {
  printf("[");
  for (int i = 0; i < v.len; i++) {
    if (i > 0)
      printf(",");
    _json_list_int(v.data[i]);
  }
  printf("]");
}
typedef struct {
  const char *i_item_desc;
  const char *w_warehouse_name;
  int d_week_seq;
  int no_promo;
  int promo;
  int total_cnt;
} tmp1_t;
typedef struct {
  int len;
  tmp1_t *data;
} tmp1_list_t;
tmp1_list_t create_tmp1_list(int len) {
  tmp1_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(tmp1_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int cs_item_sk;
  int cs_order_number;
  int cs_quantity;
  int cs_sold_date_sk;
  int cs_ship_date_sk;
  int cs_bill_cdemo_sk;
  int cs_bill_hdemo_sk;
  int cs_promo_sk;
} catalog_sale_t;
typedef struct {
  int len;
  catalog_sale_t *data;
} catalog_sale_list_t;
catalog_sale_list_t create_catalog_sale_list(int len) {
  catalog_sale_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(catalog_sale_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int inv_item_sk;
  int inv_warehouse_sk;
  int inv_date_sk;
  int inv_quantity_on_hand;
} inventory_t;
typedef struct {
  int len;
  inventory_t *data;
} inventory_list_t;
inventory_list_t create_inventory_list(int len) {
  inventory_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(inventory_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int w_warehouse_sk;
  const char *w_warehouse_name;
} warehouse_t;
typedef struct {
  int len;
  warehouse_t *data;
} warehouse_list_t;
warehouse_list_t create_warehouse_list(int len) {
  warehouse_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(warehouse_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int i_item_sk;
  const char *i_item_desc;
} item_t;
typedef struct {
  int len;
  item_t *data;
} item_list_t;
item_list_t create_item_list(int len) {
  item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int cd_demo_sk;
  const char *cd_marital_status;
} customer_demographic_t;
typedef struct {
  int len;
  customer_demographic_t *data;
} customer_demographic_list_t;
customer_demographic_list_t create_customer_demographic_list(int len) {
  customer_demographic_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(customer_demographic_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int hd_demo_sk;
  const char *hd_buy_potential;
} household_demographic_t;
typedef struct {
  int len;
  household_demographic_t *data;
} household_demographic_list_t;
household_demographic_list_t create_household_demographic_list(int len) {
  household_demographic_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(household_demographic_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int d_date_sk;
  int d_week_seq;
  int d_date;
  int d_year;
} date_dim_t;
typedef struct {
  int len;
  date_dim_t *data;
} date_dim_list_t;
date_dim_list_t create_date_dim_list(int len) {
  date_dim_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(date_dim_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int i_item_desc;
  int w_warehouse_name;
  int d_week_seq;
  int no_promo;
  int promo;
  int total_cnt;
} result_item_t;
typedef struct {
  int len;
  result_item_t *data;
} result_item_list_t;
result_item_list_t create_result_item_list(int len) {
  result_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(result_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

static list_int test_TPCDS_Q72_simplified_result;
static void test_TPCDS_Q72_simplified() {
  tmp1_t tmp1[] = {(tmp1_t){.i_item_desc = "ItemA",
                            .w_warehouse_name = "Main",
                            .d_week_seq = 10,
                            .no_promo = 1,
                            .promo = 0,
                            .total_cnt = 1}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_TPCDS_Q72_simplified_result.len != tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0; i3 < test_TPCDS_Q72_simplified_result.len; i3++) {
      if (test_TPCDS_Q72_simplified_result.data[i3] != tmp1.data[i3]) {
        tmp2 = 0;
        break;
      }
    }
  }
  if (!(tmp2)) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  catalog_sale_t catalog_sales[] = {(catalog_sale_t){.cs_item_sk = 1,
                                                     .cs_order_number = 1,
                                                     .cs_quantity = 1,
                                                     .cs_sold_date_sk = 1,
                                                     .cs_ship_date_sk = 3,
                                                     .cs_bill_cdemo_sk = 1,
                                                     .cs_bill_hdemo_sk = 1,
                                                     .cs_promo_sk = 0}};
  int catalog_sales_len = sizeof(catalog_sales) / sizeof(catalog_sales[0]);
  inventory_t inventory[] = {(inventory_t){.inv_item_sk = 1,
                                           .inv_warehouse_sk = 1,
                                           .inv_date_sk = 2,
                                           .inv_quantity_on_hand = 0}};
  int inventory_len = sizeof(inventory) / sizeof(inventory[0]);
  warehouse_t warehouse[] = {
      (warehouse_t){.w_warehouse_sk = 1, .w_warehouse_name = "Main"}};
  int warehouse_len = sizeof(warehouse) / sizeof(warehouse[0]);
  item_t item[] = {(item_t){.i_item_sk = 1, .i_item_desc = "ItemA"}};
  int item_len = sizeof(item) / sizeof(item[0]);
  customer_demographic_t customer_demographics[] = {
      (customer_demographic_t){.cd_demo_sk = 1, .cd_marital_status = "M"}};
  int customer_demographics_len =
      sizeof(customer_demographics) / sizeof(customer_demographics[0]);
  household_demographic_t household_demographics[] = {(household_demographic_t){
      .hd_demo_sk = 1, .hd_buy_potential = "5001-10000"}};
  int household_demographics_len =
      sizeof(household_demographics) / sizeof(household_demographics[0]);
  date_dim_t date_dim[] = {
      (date_dim_t){
          .d_date_sk = 1, .d_week_seq = 10, .d_date = 1, .d_year = 2000},
      (date_dim_t){
          .d_date_sk = 2, .d_week_seq = 10, .d_date = 1, .d_year = 2000},
      (date_dim_t){
          .d_date_sk = 3, .d_week_seq = 10, .d_date = 7, .d_year = 2000}};
  int date_dim_len = sizeof(date_dim) / sizeof(date_dim[0]);
  result_item_list_t result = (result_item_list_t){0, NULL};
  printf("[");
  for (int i4 = 0; i4 < result.len; i4++) {
    if (i4 > 0)
      printf(",");
    result_item_t it = result.data[i4];
    printf("{");
    _json_string("i_item_desc");
    printf(":");
    _json_int(it.i_item_desc);
    printf(",");
    _json_string("w_warehouse_name");
    printf(":");
    _json_int(it.w_warehouse_name);
    printf(",");
    _json_string("d_week_seq");
    printf(":");
    _json_int(it.d_week_seq);
    printf(",");
    _json_string("no_promo");
    printf(":");
    _json_int(it.no_promo);
    printf(",");
    _json_string("promo");
    printf(":");
    _json_int(it.promo);
    printf(",");
    _json_string("total_cnt");
    printf(":");
    _json_int(it.total_cnt);
    printf("}");
  }
  printf("]");
  test_TPCDS_Q72_simplified_result = result;
  test_TPCDS_Q72_simplified();
  return 0;
}
