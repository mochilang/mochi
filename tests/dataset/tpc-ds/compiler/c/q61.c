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
static int _sum_int(list_int v) {
  int sum = 0;
  for (int i = 0; i < v.len; i++)
    sum += v.data[i];
  return sum;
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
  int promo;
  int price;
} sale_t;
typedef struct {
  int len;
  sale_t *data;
} sale_list_t;
sale_list_t create_sale_list(int len) {
  sale_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(sale_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

static double test_TPCDS_Q61_simplified_result;
static void test_TPCDS_Q61_simplified() {
  if (!(test_TPCDS_Q61_simplified_result == 61)) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  sale_t sales[] = {(sale_t){.promo = 1, .price = 20},
                    (sale_t){.promo = 1, .price = 41},
                    (sale_t){.promo = 0, .price = 39}};
  int sales_len = sizeof(sales) / sizeof(sales[0]);
  list_int tmp1 = list_int_create(sales_len);
  int tmp2 = 0;
  for (int tmp3 = 0; tmp3 < sales_len; tmp3++) {
    sale_t s = sales[tmp3];
    if (!(s.promo)) {
      continue;
    }
    tmp1.data[tmp2] = s.price;
    tmp2++;
  }
  tmp1.len = tmp2;
  double promotions = _sum_int(tmp1);
  list_int tmp4 = list_int_create(sales_len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < sales_len; tmp6++) {
    sale_t s = sales[tmp6];
    tmp4.data[tmp5] = s.price;
    tmp5++;
  }
  tmp4.len = tmp5;
  double total = _sum_int(tmp4);
  double result = promotions * 100 / total;
  _json_float(result);
  test_TPCDS_Q61_simplified_result = result;
  test_TPCDS_Q61_simplified();
  free(tmp1.data);
  free(tmp4.data);
  return 0;
}
