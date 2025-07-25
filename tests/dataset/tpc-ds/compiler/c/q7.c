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
  double agg1;
  double agg2;
  double agg3;
  double agg4;
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
  int ss_cdemo_sk;
  int ss_sold_date_sk;
  int ss_item_sk;
  int ss_promo_sk;
  int ss_quantity;
  double ss_list_price;
  double ss_coupon_amt;
  double ss_sales_price;
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
  int cd_demo_sk;
  const char *cd_gender;
  const char *cd_marital_status;
  const char *cd_education_status;
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
  int d_date_sk;
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
  int i_item_sk;
  const char *i_item_id;
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
  int p_promo_sk;
  const char *p_channel_email;
  const char *p_channel_event;
} promotion_t;
typedef struct {
  int len;
  promotion_t *data;
} promotion_list_t;
promotion_list_t create_promotion_list(int len) {
  promotion_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(promotion_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int i_item_id;
  double agg1;
  double agg2;
  double agg3;
  double agg4;
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

static list_int test_TPCDS_Q7_result_result;
static void test_TPCDS_Q7_result() {
  tmp1_t tmp1[] = {(tmp1_t){
      .i_item_id = "I1", .agg1 = 5.0, .agg2 = 10.0, .agg3 = 2.0, .agg4 = 8.0}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_TPCDS_Q7_result_result.len != tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0; i3 < test_TPCDS_Q7_result_result.len; i3++) {
      if (test_TPCDS_Q7_result_result.data[i3] != tmp1.data[i3]) {
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
  store_sale_t store_sales[] = {(store_sale_t){.ss_cdemo_sk = 1,
                                               .ss_sold_date_sk = 1,
                                               .ss_item_sk = 1,
                                               .ss_promo_sk = 1,
                                               .ss_quantity = 5,
                                               .ss_list_price = 10.0,
                                               .ss_coupon_amt = 2.0,
                                               .ss_sales_price = 8.0}};
  int store_sales_len = sizeof(store_sales) / sizeof(store_sales[0]);
  customer_demographic_t customer_demographics[] = {
      (customer_demographic_t){.cd_demo_sk = 1,
                               .cd_gender = "M",
                               .cd_marital_status = "S",
                               .cd_education_status = "College"}};
  int customer_demographics_len =
      sizeof(customer_demographics) / sizeof(customer_demographics[0]);
  date_dim_t date_dim[] = {(date_dim_t){.d_date_sk = 1, .d_year = 1998}};
  int date_dim_len = sizeof(date_dim) / sizeof(date_dim[0]);
  item_t item[] = {(item_t){.i_item_sk = 1, .i_item_id = "I1"}};
  int item_len = sizeof(item) / sizeof(item[0]);
  promotion_t promotion[] = {(promotion_t){
      .p_promo_sk = 1, .p_channel_email = "N", .p_channel_event = "Y"}};
  int promotion_len = sizeof(promotion) / sizeof(promotion[0]);
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
    _json_string("agg1");
    printf(":");
    _json_float(it.agg1);
    printf(",");
    _json_string("agg2");
    printf(":");
    _json_float(it.agg2);
    printf(",");
    _json_string("agg3");
    printf(":");
    _json_float(it.agg3);
    printf(",");
    _json_string("agg4");
    printf(":");
    _json_float(it.agg4);
    printf("}");
  }
  printf("]");
  test_TPCDS_Q7_result_result = result;
  test_TPCDS_Q7_result();
  return 0;
}
