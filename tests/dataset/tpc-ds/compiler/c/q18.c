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
typedef struct CatalogSale CatalogSale;
typedef struct CustomerDemographics CustomerDemographics;
typedef struct Customer Customer;
typedef struct CustomerAddress CustomerAddress;
typedef struct DateDim DateDim;
typedef struct Item Item;

typedef struct {
  const char *i_item_id;
  const char *ca_country;
  const char *ca_state;
  const char *ca_county;
  double agg1;
  double agg2;
  double agg3;
  double agg4;
  double agg5;
  double agg6;
  double agg7;
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
  int cs_quantity;
  double cs_list_price;
  double cs_coupon_amt;
  double cs_sales_price;
  double cs_net_profit;
  int cs_bill_cdemo_sk;
  int cs_bill_customer_sk;
  int cs_sold_date_sk;
  int cs_item_sk;
} catalog_sale_t;

typedef struct {
  int cd_demo_sk;
  const char *cd_gender;
  const char *cd_education_status;
  int cd_dep_count;
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
  int c_customer_sk;
  int c_current_cdemo_sk;
  int c_current_addr_sk;
  int c_birth_year;
  int c_birth_month;
} customer_t;

typedef struct {
  int ca_address_sk;
  const char *ca_country;
  const char *ca_state;
  const char *ca_county;
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
  int i_item_sk;
  const char *i_item_id;
} item_t;

typedef struct {
  const char *i_item_id;
  const char *ca_country;
  const char *ca_state;
  const char *ca_county;
  int q;
  double lp;
  double cp;
  double sp;
  double np;
  int by;
  int dep;
} joined_item_t;
typedef struct {
  int len;
  joined_item_t *data;
} joined_item_list_t;
joined_item_list_t create_joined_item_list(int len) {
  joined_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(joined_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int i_item_id;
  int ca_country;
  int ca_state;
  int ca_county;
  double agg1;
  double agg2;
  double agg3;
  double agg4;
  double agg5;
  double agg6;
  double agg7;
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

typedef struct CatalogSale {
  int cs_quantity;
  double cs_list_price;
  double cs_coupon_amt;
  double cs_sales_price;
  double cs_net_profit;
  int cs_bill_cdemo_sk;
  int cs_bill_customer_sk;
  int cs_sold_date_sk;
  int cs_item_sk;
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

typedef struct CustomerDemographics {
  int cd_demo_sk;
  char *cd_gender;
  char *cd_education_status;
  int cd_dep_count;
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

typedef struct Customer {
  int c_customer_sk;
  int c_current_cdemo_sk;
  int c_current_addr_sk;
  int c_birth_year;
  int c_birth_month;
} Customer;
typedef struct {
  int len;
  customer_t *data;
} customer_list_t;
customer_list_t create_customer_list(int len) {
  customer_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(customer_t));
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
  char *ca_county;
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

typedef struct Item {
  int i_item_sk;
  char *i_item_id;
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

static list_int test_TPCDS_Q18_averages_result;
static void test_TPCDS_Q18_averages() {
  tmp1_t tmp1[] = {(tmp1_t){.i_item_id = "I1",
                            .ca_country = "US",
                            .ca_state = "CA",
                            .ca_county = "County1",
                            .agg1 = 1.0,
                            .agg2 = 10.0,
                            .agg3 = 1.0,
                            .agg4 = 9.0,
                            .agg5 = 2.0,
                            .agg6 = 1980.0,
                            .agg7 = 2.0}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_TPCDS_Q18_averages_result.len != tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0; i3 < test_TPCDS_Q18_averages_result.len; i3++) {
      if (test_TPCDS_Q18_averages_result.data[i3] != tmp1.data[i3]) {
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
  catalog_sale_t catalog_sales[] = {(catalog_sale_t){.cs_quantity = 1,
                                                     .cs_list_price = 10.0,
                                                     .cs_coupon_amt = 1.0,
                                                     .cs_sales_price = 9.0,
                                                     .cs_net_profit = 2.0,
                                                     .cs_bill_cdemo_sk = 1,
                                                     .cs_bill_customer_sk = 1,
                                                     .cs_sold_date_sk = 1,
                                                     .cs_item_sk = 1}};
  int catalog_sales_len = sizeof(catalog_sales) / sizeof(catalog_sales[0]);
  customer_demographic_t customer_demographics[] = {
      (customer_demographic_t){.cd_demo_sk = 1,
                               .cd_gender = "M",
                               .cd_education_status = "College",
                               .cd_dep_count = 2},
      (customer_demographic_t){.cd_demo_sk = 2,
                               .cd_gender = "F",
                               .cd_education_status = "College",
                               .cd_dep_count = 2}};
  int customer_demographics_len =
      sizeof(customer_demographics) / sizeof(customer_demographics[0]);
  customer_t customer[] = {(customer_t){.c_customer_sk = 1,
                                        .c_current_cdemo_sk = 2,
                                        .c_current_addr_sk = 1,
                                        .c_birth_year = 1980,
                                        .c_birth_month = 1}};
  int customer_len = sizeof(customer) / sizeof(customer[0]);
  customer_addres_t customer_address[] = {
      (customer_addres_t){.ca_address_sk = 1,
                          .ca_country = "US",
                          .ca_state = "CA",
                          .ca_county = "County1"}};
  int customer_address_len =
      sizeof(customer_address) / sizeof(customer_address[0]);
  date_dim_t date_dim[] = {(date_dim_t){.d_date_sk = 1, .d_year = 1999}};
  int date_dim_len = sizeof(date_dim) / sizeof(date_dim[0]);
  item_t item[] = {(item_t){.i_item_sk = 1, .i_item_id = "I1"}};
  int item_len = sizeof(item) / sizeof(item[0]);
  joined_item_list_t tmp4 =
      joined_item_list_t_create(catalog_sales.len * customer_demographics.len *
                                customer.len * customer_demographics.len *
                                customer_address.len * date_dim.len * item.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < catalog_sales_len; tmp6++) {
    catalog_sale_t cs = catalog_sales[tmp6];
    for (int tmp7 = 0; tmp7 < customer_demographics_len; tmp7++) {
      customer_demographic_t cd1 = customer_demographics[tmp7];
      if (!(cs.cs_bill_cdemo_sk == cd1.cd_demo_sk && cd1.cd_gender == "M" &&
            cd1.cd_education_status == "College")) {
        continue;
      }
      for (int tmp8 = 0; tmp8 < customer_len; tmp8++) {
        customer_t c = customer[tmp8];
        if (!(cs.cs_bill_customer_sk == c.c_customer_sk)) {
          continue;
        }
        for (int tmp9 = 0; tmp9 < customer_demographics_len; tmp9++) {
          customer_demographic_t cd2 = customer_demographics[tmp9];
          if (!(c.c_current_cdemo_sk == cd2.cd_demo_sk)) {
            continue;
          }
          for (int tmp10 = 0; tmp10 < customer_address_len; tmp10++) {
            customer_addres_t ca = customer_address[tmp10];
            if (!(c.c_current_addr_sk == ca.ca_address_sk)) {
              continue;
            }
            for (int tmp11 = 0; tmp11 < date_dim_len; tmp11++) {
              date_dim_t d = date_dim[tmp11];
              if (!(cs.cs_sold_date_sk == d.d_date_sk && d.d_year == 1999)) {
                continue;
              }
              for (int tmp12 = 0; tmp12 < item_len; tmp12++) {
                item_t i = item[tmp12];
                if (!(cs.cs_item_sk == i.i_item_sk)) {
                  continue;
                }
                tmp4.data[tmp5] = (joined_item_t){.i_item_id = i.i_item_id,
                                                  .ca_country = ca.ca_country,
                                                  .ca_state = ca.ca_state,
                                                  .ca_county = ca.ca_county,
                                                  .q = cs.cs_quantity,
                                                  .lp = cs.cs_list_price,
                                                  .cp = cs.cs_coupon_amt,
                                                  .sp = cs.cs_sales_price,
                                                  .np = cs.cs_net_profit,
                                                  .by = c.c_birth_year,
                                                  .dep = cd1.cd_dep_count};
                tmp5++;
              }
            }
          }
        }
      }
    }
  }
  tmp4.len = tmp5;
  joined_item_list_t joined = tmp4;
  result_item_list_t result = (result_item_list_t){0, NULL};
  printf("[");
  for (int i13 = 0; i13 < result.len; i13++) {
    if (i13 > 0)
      printf(",");
    result_item_t it = result.data[i13];
    printf("{");
    _json_string("i_item_id");
    printf(":");
    _json_int(it.i_item_id);
    printf(",");
    _json_string("ca_country");
    printf(":");
    _json_int(it.ca_country);
    printf(",");
    _json_string("ca_state");
    printf(":");
    _json_int(it.ca_state);
    printf(",");
    _json_string("ca_county");
    printf(":");
    _json_int(it.ca_county);
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
    printf(",");
    _json_string("agg5");
    printf(":");
    _json_float(it.agg5);
    printf(",");
    _json_string("agg6");
    printf(":");
    _json_float(it.agg6);
    printf(",");
    _json_string("agg7");
    printf(":");
    _json_float(it.agg7);
    printf("}");
  }
  printf("]");
  test_TPCDS_Q18_averages_result = result;
  test_TPCDS_Q18_averages();
  return 0;
}
