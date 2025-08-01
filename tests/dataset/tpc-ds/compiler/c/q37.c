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
  const char *i_item_id;
  const char *i_item_desc;
  double i_current_price;
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
  int i_item_sk;
  const char *i_item_id;
  const char *i_item_desc;
  double i_current_price;
  int i_manufact_id;
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
  int d_date_sk;
  const char *d_date;
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
  int cs_item_sk;
  int cs_sold_date_sk;
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
  int i_item_id;
  int i_item_desc;
  int i_current_price;
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

static list_int test_TPCDS_Q37_simplified_result;
static void test_TPCDS_Q37_simplified() {
  tmp1_t tmp1[] = {(tmp1_t){
      .i_item_id = "I1", .i_item_desc = "Item1", .i_current_price = 30.0}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_TPCDS_Q37_simplified_result.len != tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0; i3 < test_TPCDS_Q37_simplified_result.len; i3++) {
      if (test_TPCDS_Q37_simplified_result.data[i3] != tmp1.data[i3]) {
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
  item_t item[] = {(item_t){.i_item_sk = 1,
                            .i_item_id = "I1",
                            .i_item_desc = "Item1",
                            .i_current_price = 30.0,
                            .i_manufact_id = 800},
                   (item_t){.i_item_sk = 2,
                            .i_item_id = "I2",
                            .i_item_desc = "Item2",
                            .i_current_price = 60.0,
                            .i_manufact_id = 801}};
  int item_len = sizeof(item) / sizeof(item[0]);
  inventory_t inventory[] = {(inventory_t){.inv_item_sk = 1,
                                           .inv_warehouse_sk = 1,
                                           .inv_date_sk = 1,
                                           .inv_quantity_on_hand = 200},
                             (inventory_t){.inv_item_sk = 2,
                                           .inv_warehouse_sk = 1,
                                           .inv_date_sk = 1,
                                           .inv_quantity_on_hand = 300}};
  int inventory_len = sizeof(inventory) / sizeof(inventory[0]);
  date_dim_t date_dim[] = {
      (date_dim_t){.d_date_sk = 1, .d_date = "2000-01-15"}};
  int date_dim_len = sizeof(date_dim) / sizeof(date_dim[0]);
  catalog_sale_t catalog_sales[] = {
      (catalog_sale_t){.cs_item_sk = 1, .cs_sold_date_sk = 1}};
  int catalog_sales_len = sizeof(catalog_sales) / sizeof(catalog_sales[0]);
  result_item_list_t result = (result_item_list_t){0, NULL};
  printf("[");
  for (int i4 = 0; i4 < result.len; i4++) {
    if (i4 > 0)
      printf(",");
    result_item_t it = result.data[i4];
    printf("{");
    _json_string("i_item_id");
    printf(":");
    _json_int(it.i_item_id);
    printf(",");
    _json_string("i_item_desc");
    printf(":");
    _json_int(it.i_item_desc);
    printf(",");
    _json_string("i_current_price");
    printf(":");
    _json_int(it.i_current_price);
    printf("}");
  }
  printf("]");
  test_TPCDS_Q37_simplified_result = result;
  test_TPCDS_Q37_simplified();
  return 0;
}
