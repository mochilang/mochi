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
  int brand_id;
  double ext_price;
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
  int brand_id;
  double ext_price;
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
  int item;
  int sold_date;
  double price;
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
  int i_brand_id;
  int i_manager_id;
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
  int brand_id;
  double ext_price;
} grouped_item_t;
typedef struct {
  int len;
  grouped_item_t *data;
} grouped_item_list_t;
grouped_item_list_t create_grouped_item_list(int len) {
  grouped_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(grouped_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

static list_int test_TPCDS_Q55_simplified_result;
static void test_TPCDS_Q55_simplified() {
  tmp1_t tmp1[] = {(tmp1_t){.brand_id = 10, .ext_price = 35.0},
                   (tmp_item_t){.brand_id = 20, .ext_price = 20.0}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_TPCDS_Q55_simplified_result.len != tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0; i3 < test_TPCDS_Q55_simplified_result.len; i3++) {
      if (test_TPCDS_Q55_simplified_result.data[i3] != tmp1.data[i3]) {
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
  store_sale_t store_sales[] = {
      (store_sale_t){.item = 1, .sold_date = 1, .price = 10.0},
      (store_sale_t){.item = 2, .sold_date = 1, .price = 20.0},
      (store_sale_t){.item = 3, .sold_date = 1, .price = 25.0}};
  int store_sales_len = sizeof(store_sales) / sizeof(store_sales[0]);
  item_t item[] = {
      (item_t){.i_item_sk = 1, .i_brand_id = 10, .i_manager_id = 1},
      (item_t){.i_item_sk = 2, .i_brand_id = 20, .i_manager_id = 1},
      (item_t){.i_item_sk = 3, .i_brand_id = 10, .i_manager_id = 1}};
  int item_len = sizeof(item) / sizeof(item[0]);
  date_dim_t date_dim[] = {
      (date_dim_t){.d_date_sk = 1, .d_year = 2001, .d_moy = 11}};
  int date_dim_len = sizeof(date_dim) / sizeof(date_dim[0]);
  grouped_item_list_t grouped = (grouped_item_list_t){0, NULL};
  grouped_item_list_t tmp4 = grouped_item_list_t_create(grouped.len);
  int *tmp7 = (int *)malloc(sizeof(int) * grouped.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < grouped.len; tmp6++) {
    grouped_item_t g = grouped.data[tmp6];
    tmp4.data[tmp5] = g;
    list_int result = list_int_create(2);
    result.data[0] = (-g.ext_price);
    result.data[1] = g.brand_id;
    tmp7[tmp5] = result;
    tmp5++;
  }
  tmp4.len = tmp5;
  for (int i10 = 0; i10 < tmp5 - 1; i10++) {
    for (int i11 = i10 + 1; i11 < tmp5; i11++) {
      if (tmp7[i10] > tmp7[i11]) {
        int tmp8 = tmp7[i10];
        tmp7[i10] = tmp7[i11];
        tmp7[i11] = tmp8;
        grouped_item_t tmp9 = tmp4.data[i10];
        tmp4.data[i10] = tmp4.data[i11];
        tmp4.data[i11] = tmp9;
      }
    }
  }
  grouped_item_list_t result = tmp4;
  printf("[");
  for (int i12 = 0; i12 < 2; i12++) {
    if (i12 > 0)
      printf(",");
    grouped_item_t it = result.data[i12];
    printf("{");
    _json_string("brand_id");
    printf(":");
    _json_int(it.brand_id);
    printf(",");
    _json_string("ext_price");
    printf(":");
    _json_float(it.ext_price);
    printf("}");
  }
  printf("]");
  test_TPCDS_Q55_simplified_result = result;
  test_TPCDS_Q55_simplified();
  free(result.data);
  return 0;
}
