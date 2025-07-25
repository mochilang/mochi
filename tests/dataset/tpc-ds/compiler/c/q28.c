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
static double _avg_float(list_float v) {
  if (v.len == 0)
    return 0;
  double sum = 0;
  for (int i = 0; i < v.len; i++)
    sum += v.data[i];
  return sum / v.len;
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
typedef struct StoreSale StoreSale;

typedef struct {
  double b1__l_p;
  int b1__c_n_t;
  int b1__c_n_t_d;
  double b2__l_p;
  int b2__c_n_t;
  int b2__c_n_t_d;
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
  int ss_quantity;
  double ss_list_price;
  double ss_coupon_amt;
  double ss_wholesale_cost;
} store_sale_t;

typedef struct {
  double b1__l_p;
  int b1__c_n_t;
  int b1__c_n_t_d;
  double b2__l_p;
  int b2__c_n_t;
  int b2__c_n_t_d;
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

typedef struct StoreSale {
  int ss_quantity;
  double ss_list_price;
  double ss_coupon_amt;
  double ss_wholesale_cost;
} StoreSale;
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

static int test_TPCDS_Q28_buckets_result;
static void test_TPCDS_Q28_buckets() {
  if (!(test_TPCDS_Q28_buckets_result == (tmp_item_t){.b1__l_p = 100.0,
                                                      .b1__c_n_t = 1,
                                                      .b1__c_n_t_d = 1,
                                                      .b2__l_p = 80.0,
                                                      .b2__c_n_t = 1,
                                                      .b2__c_n_t_d = 1})) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  store_sale_t store_sales[] = {(store_sale_t){.ss_quantity = 3,
                                               .ss_list_price = 100.0,
                                               .ss_coupon_amt = 50.0,
                                               .ss_wholesale_cost = 30.0},
                                (store_sale_t){.ss_quantity = 8,
                                               .ss_list_price = 80.0,
                                               .ss_coupon_amt = 10.0,
                                               .ss_wholesale_cost = 20.0},
                                (store_sale_t){.ss_quantity = 12,
                                               .ss_list_price = 60.0,
                                               .ss_coupon_amt = 5.0,
                                               .ss_wholesale_cost = 15.0}};
  int store_sales_len = sizeof(store_sales) / sizeof(store_sales[0]);
  store_sale_list_t tmp1 = store_sale_list_t_create(store_sales_len);
  int tmp2 = 0;
  for (int tmp3 = 0; tmp3 < store_sales_len; tmp3++) {
    store_sale_t ss = store_sales[tmp3];
    if (!(ss.ss_quantity >= 0 && ss.ss_quantity <= 5 &&
          ((ss.ss_list_price >= 0 && ss.ss_list_price <= 110) ||
           (ss.ss_coupon_amt >= 0 && ss.ss_coupon_amt <= 1000) ||
           (ss.ss_wholesale_cost >= 0 && ss.ss_wholesale_cost <= 50)))) {
      continue;
    }
    tmp1.data[tmp2] = ss;
    tmp2++;
  }
  tmp1.len = tmp2;
  store_sale_list_t bucket1 = tmp1;
  store_sale_list_t tmp4 = store_sale_list_t_create(store_sales_len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < store_sales_len; tmp6++) {
    store_sale_t ss = store_sales[tmp6];
    if (!(ss.ss_quantity >= 6 && ss.ss_quantity <= 10 &&
          ((ss.ss_list_price >= 0 && ss.ss_list_price <= 110) ||
           (ss.ss_coupon_amt >= 0 && ss.ss_coupon_amt <= 1000) ||
           (ss.ss_wholesale_cost >= 0 && ss.ss_wholesale_cost <= 50)))) {
      continue;
    }
    tmp4.data[tmp5] = ss;
    tmp5++;
  }
  tmp4.len = tmp5;
  store_sale_list_t bucket2 = tmp4;
  list_float tmp7 = list_float_create(bucket1.len);
  int tmp8 = 0;
  for (int tmp9 = 0; tmp9 < bucket1.len; tmp9++) {
    store_sale_t x = bucket1.data[tmp9];
    tmp7.data[tmp8] = x.ss_list_price;
    tmp8++;
  }
  tmp7.len = tmp8;
  list_float tmp10 = list_float_create(bucket2.len);
  int tmp11 = 0;
  for (int tmp12 = 0; tmp12 < bucket2.len; tmp12++) {
    store_sale_t x = bucket2.data[tmp12];
    tmp10.data[tmp11] = x.ss_list_price;
    tmp11++;
  }
  tmp10.len = tmp11;
  result_item_t result =
      (result_item_t){.b1__l_p = _avg_float(tmp7),
                      .b1__c_n_t = bucket1.len,
                      .b1__c_n_t_d = (list_int){0, NULL}.len,
                      .b2__l_p = _avg_float(tmp10),
                      .b2__c_n_t = bucket2.len,
                      .b2__c_n_t_d = (list_int){0, NULL}.len};
  printf("{");
  _json_string("B1_LP");
  printf(":");
  _json_float(result.b1__l_p);
  printf(",");
  _json_string("B1_CNT");
  printf(":");
  _json_int(result.b1__c_n_t);
  printf(",");
  _json_string("B1_CNTD");
  printf(":");
  _json_int(result.b1__c_n_t_d);
  printf(",");
  _json_string("B2_LP");
  printf(":");
  _json_float(result.b2__l_p);
  printf(",");
  _json_string("B2_CNT");
  printf(":");
  _json_int(result.b2__c_n_t);
  printf(",");
  _json_string("B2_CNTD");
  printf(":");
  _json_int(result.b2__c_n_t_d);
  printf("}");
  test_TPCDS_Q28_buckets_result = result;
  test_TPCDS_Q28_buckets();
  free(bucket1.data);
  free(bucket2.data);
  free(tmp7.data);
  free(tmp10.data);
  return 0;
}
