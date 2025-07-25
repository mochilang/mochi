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
  int d_year;
  int brand_id;
  const char *brand;
  double sum_agg;
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
  int d_year;
  int brand_id;
  const char *brand;
  double sum_agg;
} tmp_item_t;
typedef struct {
  int len;
  tmp_item_t *data;
} tmp_item_list_t;
tmp_item_list_t create_tmp_item_list(int len) {
  tmp_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(tmp_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int d_date_sk;
  int d_year;
  int d_moy;
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
  int ss_sold_date_sk;
  int ss_item_sk;
  double ss_ext_sales_price;
} store_sale_t;
typedef struct {
  int len;
  store_sale_t *data;
} store_sale_list_t;
store_sale_list_t create_store_sale_list(int len) {
  store_sale_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(store_sale_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int i_item_sk;
  int i_manufact_id;
  int i_brand_id;
  const char *i_brand;
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
  int d_year;
  int brand_id;
  int brand;
  double sum_agg;
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

static list_int test_TPCDS_Q3_result_result;
static void test_TPCDS_Q3_result() {
  tmp1_t tmp1[] = {
      (tmp1_t){
          .d_year = 1998, .brand_id = 2, .brand = "Brand2", .sum_agg = 20.0},
      (tmp_item_t){
          .d_year = 1998, .brand_id = 1, .brand = "Brand1", .sum_agg = 10.0}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_TPCDS_Q3_result_result.len != tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0; i3 < test_TPCDS_Q3_result_result.len; i3++) {
      if (test_TPCDS_Q3_result_result.data[i3] != tmp1.data[i3]) {
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
  date_dim_t date_dim[] = {
      (date_dim_t){.d_date_sk = 1, .d_year = 1998, .d_moy = 12}};
  int date_dim_len = sizeof(date_dim) / sizeof(date_dim[0]);
  store_sale_t store_sales[] = {
      (store_sale_t){
          .ss_sold_date_sk = 1, .ss_item_sk = 1, .ss_ext_sales_price = 10.0},
      (store_sale_t){
          .ss_sold_date_sk = 1, .ss_item_sk = 2, .ss_ext_sales_price = 20.0}};
  int store_sales_len = sizeof(store_sales) / sizeof(store_sales[0]);
  item_t item[] = {(item_t){.i_item_sk = 1,
                            .i_manufact_id = 100,
                            .i_brand_id = 1,
                            .i_brand = "Brand1"},
                   (item_t){.i_item_sk = 2,
                            .i_manufact_id = 100,
                            .i_brand_id = 2,
                            .i_brand = "Brand2"}};
  int item_len = sizeof(item) / sizeof(item[0]);
  result_item_list_t result = (result_item_list_t){0, NULL};
  printf("[");
  for (int i4 = 0; i4 < result.len; i4++) {
    if (i4 > 0)
      printf(",");
    result_item_t it = result.data[i4];
    printf("{");
    _json_string("d_year");
    printf(":");
    _json_int(it.d_year);
    printf(",");
    _json_string("brand_id");
    printf(":");
    _json_int(it.brand_id);
    printf(",");
    _json_string("brand");
    printf(":");
    _json_int(it.brand);
    printf(",");
    _json_string("sum_agg");
    printf(":");
    _json_float(it.sum_agg);
    printf("}");
  }
  printf("]");
  test_TPCDS_Q3_result_result = result;
  test_TPCDS_Q3_result();
  return 0;
}
