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
typedef struct StoreSale StoreSale;
typedef struct StoreReturn StoreReturn;
typedef struct CatalogSale CatalogSale;
typedef struct DateDim DateDim;
typedef struct Store Store;
typedef struct Item Item;

typedef struct {
  const char *i_item_id;
  const char *i_item_desc;
  const char *s_store_id;
  const char *s_store_name;
  double store_sales_profit;
  double store_returns_loss;
  double catalog_sales_profit;
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
  const char *i_item_id;
  const char *i_item_desc;
  const char *s_store_id;
  const char *s_store_name;
  double store_sales_profit;
  double store_returns_loss;
  double catalog_sales_profit;
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
  int ss_sold_date_sk;
  int ss_item_sk;
  int ss_store_sk;
  int ss_customer_sk;
  double ss_net_profit;
  int ss_ticket_number;
} store_sale_t;

typedef struct {
  int sr_returned_date_sk;
  int sr_item_sk;
  int sr_customer_sk;
  int sr_ticket_number;
  double sr_net_loss;
} store_return_t;

typedef struct {
  int cs_sold_date_sk;
  int cs_item_sk;
  int cs_bill_customer_sk;
  double cs_net_profit;
} catalog_sale_t;

typedef struct {
  int d_date_sk;
  int d_moy;
  int d_year;
} date_dim_t;

typedef struct {
  int s_store_sk;
  const char *s_store_id;
  const char *s_store_name;
} store_t;

typedef struct {
  int i_item_sk;
  const char *i_item_id;
  const char *i_item_desc;
} item_t;

typedef struct {
  int i_item_id;
  int i_item_desc;
  int s_store_id;
  int s_store_name;
  double store_sales_profit;
  double store_returns_loss;
  double catalog_sales_profit;
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
  int ss_sold_date_sk;
  int ss_item_sk;
  int ss_store_sk;
  int ss_customer_sk;
  double ss_net_profit;
  int ss_ticket_number;
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

typedef struct StoreReturn {
  int sr_returned_date_sk;
  int sr_item_sk;
  int sr_customer_sk;
  int sr_ticket_number;
  double sr_net_loss;
} StoreReturn;
typedef struct {
  int len;
  store_return_t *data;
} store_return_list_t;
store_return_list_t create_store_return_list(int len) {
  store_return_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(store_return_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct CatalogSale {
  int cs_sold_date_sk;
  int cs_item_sk;
  int cs_bill_customer_sk;
  double cs_net_profit;
} CatalogSale;
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

typedef struct DateDim {
  int d_date_sk;
  int d_moy;
  int d_year;
} DateDim;
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

typedef struct Store {
  int s_store_sk;
  char *s_store_id;
  char *s_store_name;
} Store;
typedef struct {
  int len;
  store_t *data;
} store_list_t;
store_list_t create_store_list(int len) {
  store_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(store_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct Item {
  int i_item_sk;
  char *i_item_id;
  char *i_item_desc;
} Item;
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

static list_int test_TPCDS_Q25_aggregated_profit_result;
static void test_TPCDS_Q25_aggregated_profit() {
  tmp1_t tmp1[] = {(tmp1_t){.i_item_id = "ITEM1",
                            .i_item_desc = "Desc1",
                            .s_store_id = "S1",
                            .s_store_name = "Store1",
                            .store_sales_profit = 50.0,
                            .store_returns_loss = 10.0,
                            .catalog_sales_profit = 30.0},
                   (tmp_item_t){.i_item_id = "ITEM2",
                                .i_item_desc = "Desc2",
                                .s_store_id = "S1",
                                .s_store_name = "Store1",
                                .store_sales_profit = 20.0,
                                .store_returns_loss = 5.0,
                                .catalog_sales_profit = 15.0}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_TPCDS_Q25_aggregated_profit_result.len != tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0; i3 < test_TPCDS_Q25_aggregated_profit_result.len; i3++) {
      if (test_TPCDS_Q25_aggregated_profit_result.data[i3] != tmp1.data[i3]) {
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
  store_sale_t store_sales[] = {(store_sale_t){.ss_sold_date_sk = 1,
                                               .ss_item_sk = 1,
                                               .ss_store_sk = 1,
                                               .ss_customer_sk = 1,
                                               .ss_net_profit = 50.0,
                                               .ss_ticket_number = 1},
                                (store_sale_t){.ss_sold_date_sk = 1,
                                               .ss_item_sk = 2,
                                               .ss_store_sk = 1,
                                               .ss_customer_sk = 2,
                                               .ss_net_profit = 20.0,
                                               .ss_ticket_number = 2}};
  int store_sales_len = sizeof(store_sales) / sizeof(store_sales[0]);
  store_return_t store_returns[] = {(store_return_t){.sr_returned_date_sk = 2,
                                                     .sr_item_sk = 1,
                                                     .sr_customer_sk = 1,
                                                     .sr_ticket_number = 1,
                                                     .sr_net_loss = 10.0},
                                    (store_return_t){.sr_returned_date_sk = 2,
                                                     .sr_item_sk = 2,
                                                     .sr_customer_sk = 2,
                                                     .sr_ticket_number = 2,
                                                     .sr_net_loss = 5.0}};
  int store_returns_len = sizeof(store_returns) / sizeof(store_returns[0]);
  catalog_sale_t catalog_sales[] = {(catalog_sale_t){.cs_sold_date_sk = 3,
                                                     .cs_item_sk = 1,
                                                     .cs_bill_customer_sk = 1,
                                                     .cs_net_profit = 30.0},
                                    (catalog_sale_t){.cs_sold_date_sk = 3,
                                                     .cs_item_sk = 2,
                                                     .cs_bill_customer_sk = 2,
                                                     .cs_net_profit = 15.0}};
  int catalog_sales_len = sizeof(catalog_sales) / sizeof(catalog_sales[0]);
  date_dim_t date_dim[] = {
      (date_dim_t){.d_date_sk = 1, .d_moy = 4, .d_year = 2000},
      (date_dim_t){.d_date_sk = 2, .d_moy = 5, .d_year = 2000},
      (date_dim_t){.d_date_sk = 3, .d_moy = 6, .d_year = 2000}};
  int date_dim_len = sizeof(date_dim) / sizeof(date_dim[0]);
  store_t store[] = {
      (store_t){.s_store_sk = 1, .s_store_id = "S1", .s_store_name = "Store1"}};
  int store_len = sizeof(store) / sizeof(store[0]);
  item_t item[] = {
      (item_t){.i_item_sk = 1, .i_item_id = "ITEM1", .i_item_desc = "Desc1"},
      (item_t){.i_item_sk = 2, .i_item_id = "ITEM2", .i_item_desc = "Desc2"}};
  int item_len = sizeof(item) / sizeof(item[0]);
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
    _json_string("s_store_id");
    printf(":");
    _json_int(it.s_store_id);
    printf(",");
    _json_string("s_store_name");
    printf(":");
    _json_int(it.s_store_name);
    printf(",");
    _json_string("store_sales_profit");
    printf(":");
    _json_float(it.store_sales_profit);
    printf(",");
    _json_string("store_returns_loss");
    printf(":");
    _json_float(it.store_returns_loss);
    printf(",");
    _json_string("catalog_sales_profit");
    printf(":");
    _json_float(it.catalog_sales_profit);
    printf("}");
  }
  printf("]");
  test_TPCDS_Q25_aggregated_profit_result = result;
  test_TPCDS_Q25_aggregated_profit();
  return 0;
}
