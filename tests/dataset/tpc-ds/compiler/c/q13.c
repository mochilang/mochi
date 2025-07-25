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
typedef struct Store Store;
typedef struct CustomerDemographics CustomerDemographics;
typedef struct HouseholdDemographics HouseholdDemographics;
typedef struct CustomerAddress CustomerAddress;
typedef struct DateDim DateDim;

typedef struct {
  double avg_ss_quantity;
  double avg_ss_ext_sales_price;
  double avg_ss_ext_wholesale_cost;
  double sum_ss_ext_wholesale_cost;
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
  int ss_store_sk;
  int ss_sold_date_sk;
  int ss_hdemo_sk;
  int ss_cdemo_sk;
  int ss_addr_sk;
  double ss_sales_price;
  double ss_net_profit;
  int ss_quantity;
  double ss_ext_sales_price;
  double ss_ext_wholesale_cost;
} store_sale_t;

typedef struct {
  int s_store_sk;
  const char *s_state;
} store_t;

typedef struct {
  int cd_demo_sk;
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
  int hd_demo_sk;
  int hd_dep_count;
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
  int ca_address_sk;
  const char *ca_country;
  const char *ca_state;
} customer_addres_t;
typedef struct {
  int len;
  customer_addres_t *data;
} customer_addres_list_t;
customer_addres_list_t create_customer_addres_list(int len) {
  customer_addres_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(customer_addres_t));
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
  double avg_ss_quantity;
  double avg_ss_ext_sales_price;
  double avg_ss_ext_wholesale_cost;
  double sum_ss_ext_wholesale_cost;
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
  int ss_store_sk;
  int ss_sold_date_sk;
  int ss_hdemo_sk;
  int ss_cdemo_sk;
  int ss_addr_sk;
  double ss_sales_price;
  double ss_net_profit;
  int ss_quantity;
  double ss_ext_sales_price;
  double ss_ext_wholesale_cost;
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

typedef struct Store {
  int s_store_sk;
  char *s_state;
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

typedef struct CustomerDemographics {
  int cd_demo_sk;
  char *cd_marital_status;
  char *cd_education_status;
} CustomerDemographics;
typedef struct {
  int len;
  customer_demographics_t *data;
} customer_demographics_list_t;
customer_demographics_list_t create_customer_demographics_list(int len) {
  customer_demographics_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(customer_demographics_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct HouseholdDemographics {
  int hd_demo_sk;
  int hd_dep_count;
} HouseholdDemographics;
typedef struct {
  int len;
  household_demographics_t *data;
} household_demographics_list_t;
household_demographics_list_t create_household_demographics_list(int len) {
  household_demographics_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(household_demographics_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct CustomerAddress {
  int ca_address_sk;
  char *ca_country;
  char *ca_state;
} CustomerAddress;
typedef struct {
  int len;
  customer_address_t *data;
} customer_address_list_t;
customer_address_list_t create_customer_address_list(int len) {
  customer_address_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(customer_address_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct DateDim {
  int d_date_sk;
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

static list_int test_TPCDS_Q13_averages_result;
static void test_TPCDS_Q13_averages() {
  tmp1_t tmp1[] = {(tmp1_t){.avg_ss_quantity = 10.0,
                            .avg_ss_ext_sales_price = 100.0,
                            .avg_ss_ext_wholesale_cost = 50.0,
                            .sum_ss_ext_wholesale_cost = 50.0}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_TPCDS_Q13_averages_result.len != tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0; i3 < test_TPCDS_Q13_averages_result.len; i3++) {
      if (test_TPCDS_Q13_averages_result.data[i3] != tmp1.data[i3]) {
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
  store_sale_t store_sales[] = {(store_sale_t){.ss_store_sk = 1,
                                               .ss_sold_date_sk = 1,
                                               .ss_hdemo_sk = 1,
                                               .ss_cdemo_sk = 1,
                                               .ss_addr_sk = 1,
                                               .ss_sales_price = 120.0,
                                               .ss_net_profit = 150.0,
                                               .ss_quantity = 10,
                                               .ss_ext_sales_price = 100.0,
                                               .ss_ext_wholesale_cost = 50.0}};
  int store_sales_len = sizeof(store_sales) / sizeof(store_sales[0]);
  store_t store[] = {(store_t){.s_store_sk = 1, .s_state = "CA"}};
  int store_len = sizeof(store) / sizeof(store[0]);
  customer_demographic_t customer_demographics[] = {
      (customer_demographic_t){.cd_demo_sk = 1,
                               .cd_marital_status = "M1",
                               .cd_education_status = "ES1"}};
  int customer_demographics_len =
      sizeof(customer_demographics) / sizeof(customer_demographics[0]);
  household_demographic_t household_demographics[] = {
      (household_demographic_t){.hd_demo_sk = 1, .hd_dep_count = 3}};
  int household_demographics_len =
      sizeof(household_demographics) / sizeof(household_demographics[0]);
  customer_addres_t customer_address[] = {(customer_addres_t){
      .ca_address_sk = 1, .ca_country = "United States", .ca_state = "CA"}};
  int customer_address_len =
      sizeof(customer_address) / sizeof(customer_address[0]);
  date_dim_t date_dim[] = {(date_dim_t){.d_date_sk = 1, .d_year = 2001}};
  int date_dim_len = sizeof(date_dim) / sizeof(date_dim[0]);
  store_sale_list_t tmp4 = store_sale_list_t_create(
      store_sales.len * store.len * customer_demographics.len *
      household_demographics.len * customer_address.len * date_dim.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < store_sales_len; tmp6++) {
    store_sale_t ss = store_sales[tmp6];
    for (int tmp7 = 0; tmp7 < store_len; tmp7++) {
      store_t s = store[tmp7];
      if (!(ss.ss_store_sk == s.s_store_sk)) {
        continue;
      }
      for (int tmp8 = 0; tmp8 < customer_demographics_len; tmp8++) {
        customer_demographic_t cd = customer_demographics[tmp8];
        if (!(ss.ss_cdemo_sk == cd.cd_demo_sk && cd.cd_marital_status == "M1" &&
              cd.cd_education_status == "ES1")) {
          continue;
        }
        for (int tmp9 = 0; tmp9 < household_demographics_len; tmp9++) {
          household_demographic_t hd = household_demographics[tmp9];
          if (!(ss.ss_hdemo_sk == hd.hd_demo_sk && hd.hd_dep_count == 3)) {
            continue;
          }
          for (int tmp10 = 0; tmp10 < customer_address_len; tmp10++) {
            customer_addres_t ca = customer_address[tmp10];
            if (!(ss.ss_addr_sk == ca.ca_address_sk &&
                  ca.ca_country == "United States" && ca.ca_state == "CA")) {
              continue;
            }
            for (int tmp11 = 0; tmp11 < date_dim_len; tmp11++) {
              date_dim_t d = date_dim[tmp11];
              if (!(ss.ss_sold_date_sk == d.d_date_sk && d.d_year == 2001)) {
                continue;
              }
              tmp4.data[tmp5] = ss;
              tmp5++;
            }
          }
        }
      }
    }
  }
  tmp4.len = tmp5;
  store_sale_list_t filtered = tmp4;
  result_item_list_t result = (result_item_list_t){0, NULL};
  printf("[");
  for (int i12 = 0; i12 < result.len; i12++) {
    if (i12 > 0)
      printf(",");
    result_item_t it = result.data[i12];
    printf("{");
    _json_string("avg_ss_quantity");
    printf(":");
    _json_float(it.avg_ss_quantity);
    printf(",");
    _json_string("avg_ss_ext_sales_price");
    printf(":");
    _json_float(it.avg_ss_ext_sales_price);
    printf(",");
    _json_string("avg_ss_ext_wholesale_cost");
    printf(":");
    _json_float(it.avg_ss_ext_wholesale_cost);
    printf(",");
    _json_string("sum_ss_ext_wholesale_cost");
    printf(":");
    _json_float(it.sum_ss_ext_wholesale_cost);
    printf("}");
  }
  printf("]");
  test_TPCDS_Q13_averages_result = result;
  test_TPCDS_Q13_averages();
  return 0;
}
