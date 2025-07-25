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
  const char *channel;
  int id;
  double sales;
  double returns;
  double profit;
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
  const char *channel;
  int id;
  double sales;
  double returns;
  double profit;
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
  const char *channel;
  int id;
  double sales;
  double returns;
  double profit;
} tmp_item1_t;
typedef struct {
  int len;
  tmp_item1_t *data;
} tmp_item1_list_t;
tmp_item1_list_t create_tmp_item1_list(int len) {
  tmp_item1_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(tmp_item1_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int d_date_sk;
  int d_date;
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
  int s_store_sk;
  double ss_ext_sales_price;
  double ss_net_profit;
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
  int sr_returned_date_sk;
  int s_store_sk;
  double sr_return_amt;
  double sr_net_loss;
} store_return_t;
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

typedef struct {
  int cs_sold_date_sk;
  int cs_call_center_sk;
  double cs_ext_sales_price;
  double cs_net_profit;
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
  int cr_returned_date_sk;
  int cr_call_center_sk;
  double cr_return_amount;
  double cr_net_loss;
} catalog_return_t;
typedef struct {
  int len;
  catalog_return_t *data;
} catalog_return_list_t;
catalog_return_list_t create_catalog_return_list(int len) {
  catalog_return_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(catalog_return_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int ws_sold_date_sk;
  int ws_web_page_sk;
  double ws_ext_sales_price;
  double ws_net_profit;
} web_sale_t;
typedef struct {
  int len;
  web_sale_t *data;
} web_sale_list_t;
web_sale_list_t create_web_sale_list(int len) {
  web_sale_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(web_sale_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int wr_returned_date_sk;
  int wr_web_page_sk;
  double wr_return_amt;
  double wr_net_loss;
} web_return_t;
typedef struct {
  int len;
  web_return_t *data;
} web_return_list_t;
web_return_list_t create_web_return_list(int len) {
  web_return_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(web_return_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int s_store_sk;
  double sales;
  double profit;
} ss_item_t;
typedef struct {
  int len;
  ss_item_t *data;
} ss_item_list_t;
ss_item_list_t create_ss_item_list(int len) {
  ss_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(ss_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int s_store_sk;
  double returns;
  double profit_loss;
} sr_item_t;
typedef struct {
  int len;
  sr_item_t *data;
} sr_item_list_t;
sr_item_list_t create_sr_item_list(int len) {
  sr_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(sr_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int cs_call_center_sk;
  double sales;
  double profit;
} cs_item_t;
typedef struct {
  int len;
  cs_item_t *data;
} cs_item_list_t;
cs_item_list_t create_cs_item_list(int len) {
  cs_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(cs_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int cr_call_center_sk;
  double returns;
  double profit_loss;
} cr_item_t;
typedef struct {
  int len;
  cr_item_t *data;
} cr_item_list_t;
cr_item_list_t create_cr_item_list(int len) {
  cr_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(cr_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int wp_web_page_sk;
  double sales;
  double profit;
} ws_item_t;
typedef struct {
  int len;
  ws_item_t *data;
} ws_item_list_t;
ws_item_list_t create_ws_item_list(int len) {
  ws_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(ws_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int wp_web_page_sk;
  double returns;
  double profit_loss;
} wr_item_t;
typedef struct {
  int len;
  wr_item_t *data;
} wr_item_list_t;
wr_item_list_t create_wr_item_list(int len) {
  wr_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(wr_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  const char *channel;
  int id;
  double sales;
  double returns;
  double profit;
} s_item_t;
typedef struct {
  int len;
  s_item_t *data;
} s_item_list_t;
s_item_list_t create_s_item_list(int len) {
  s_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(s_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  const char *channel;
  int id;
  double sales;
  double returns;
  double profit;
} c_item_t;
typedef struct {
  int len;
  c_item_t *data;
} c_item_list_t;
c_item_list_t create_c_item_list(int len) {
  c_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(c_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  const char *channel;
  int id;
  double sales;
  double returns;
  double profit;
} w_item_t;
typedef struct {
  int len;
  w_item_t *data;
} w_item_list_t;
w_item_list_t create_w_item_list(int len) {
  w_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(w_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int channel;
  int id;
  double sales;
  double returns;
  double profit;
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

static list_int test_TPCDS_Q77_simplified_result;
static void test_TPCDS_Q77_simplified() {
  tmp1_t tmp1[] = {(tmp1_t){.channel = "catalog channel",
                            .id = 1,
                            .sales = 150.0,
                            .returns = 7.0,
                            .profit = 12.0},
                   (tmp_item_t){.channel = "store channel",
                                .id = 1,
                                .sales = 100.0,
                                .returns = 5.0,
                                .profit = 9.0},
                   (tmp_item1_t){.channel = "web channel",
                                 .id = 1,
                                 .sales = 200.0,
                                 .returns = 10.0,
                                 .profit = 18.0}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_TPCDS_Q77_simplified_result.len != tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0; i3 < test_TPCDS_Q77_simplified_result.len; i3++) {
      if (test_TPCDS_Q77_simplified_result.data[i3] != tmp1.data[i3]) {
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
  date_dim_t date_dim[] = {(date_dim_t){.d_date_sk = 1, .d_date = 1}};
  int date_dim_len = sizeof(date_dim) / sizeof(date_dim[0]);
  store_sale_t store_sales[] = {(store_sale_t){.ss_sold_date_sk = 1,
                                               .s_store_sk = 1,
                                               .ss_ext_sales_price = 100.0,
                                               .ss_net_profit = 10.0}};
  int store_sales_len = sizeof(store_sales) / sizeof(store_sales[0]);
  store_return_t store_returns[] = {(store_return_t){.sr_returned_date_sk = 1,
                                                     .s_store_sk = 1,
                                                     .sr_return_amt = 5.0,
                                                     .sr_net_loss = 1.0}};
  int store_returns_len = sizeof(store_returns) / sizeof(store_returns[0]);
  catalog_sale_t catalog_sales[] = {
      (catalog_sale_t){.cs_sold_date_sk = 1,
                       .cs_call_center_sk = 1,
                       .cs_ext_sales_price = 150.0,
                       .cs_net_profit = 15.0}};
  int catalog_sales_len = sizeof(catalog_sales) / sizeof(catalog_sales[0]);
  catalog_return_t catalog_returns[] = {
      (catalog_return_t){.cr_returned_date_sk = 1,
                         .cr_call_center_sk = 1,
                         .cr_return_amount = 7.0,
                         .cr_net_loss = 3.0}};
  int catalog_returns_len =
      sizeof(catalog_returns) / sizeof(catalog_returns[0]);
  web_sale_t web_sales[] = {(web_sale_t){.ws_sold_date_sk = 1,
                                         .ws_web_page_sk = 1,
                                         .ws_ext_sales_price = 200.0,
                                         .ws_net_profit = 20.0}};
  int web_sales_len = sizeof(web_sales) / sizeof(web_sales[0]);
  web_return_t web_returns[] = {(web_return_t){.wr_returned_date_sk = 1,
                                               .wr_web_page_sk = 1,
                                               .wr_return_amt = 10.0,
                                               .wr_net_loss = 2.0}};
  int web_returns_len = sizeof(web_returns) / sizeof(web_returns[0]);
  ss_item_t ss[10];
  int ss_len = 0;
  for (int i4 = 0; i4 < store_sales_len; i4++) {
    store_sale_t ss = store_sales[i4];
    for (int i5 = 0; i5 < date_dim_len; i5++) {
      date_dim_t d = date_dim[i5];
      if (!(d.d_date_sk == ss.ss_sold_date_sk)) {
        continue;
      }
      int tmp6 = -1;
      for (int i7 = 0; i7 < ss_len; i7++) {
        if (strcmp(ss[i7].name, ss.s_store_sk) == 0) {
          tmp6 = i7;
          break;
        }
      }
      if (tmp6 == -1) {
        ss[ss_len].name = ss.s_store_sk;
        ss[ss_len].count = 1;
        tmp6 = ss_len;
        ss_len++;
      } else {
        ss[tmp6].count++;
      }
    }
  }
  sr_item_t sr[10];
  int sr_len = 0;
  for (int i8 = 0; i8 < store_returns_len; i8++) {
    store_return_t sr = store_returns[i8];
    for (int i9 = 0; i9 < date_dim_len; i9++) {
      date_dim_t d = date_dim[i9];
      if (!(d.d_date_sk == sr.sr_returned_date_sk)) {
        continue;
      }
      int tmp10 = -1;
      for (int i11 = 0; i11 < sr_len; i11++) {
        if (strcmp(sr[i11].name, sr.s_store_sk) == 0) {
          tmp10 = i11;
          break;
        }
      }
      if (tmp10 == -1) {
        sr[sr_len].name = sr.s_store_sk;
        sr[sr_len].count = 1;
        tmp10 = sr_len;
        sr_len++;
      } else {
        sr[tmp10].count++;
      }
    }
  }
  cs_item_t cs[10];
  int cs_len = 0;
  for (int i12 = 0; i12 < catalog_sales_len; i12++) {
    catalog_sale_t cs = catalog_sales[i12];
    for (int i13 = 0; i13 < date_dim_len; i13++) {
      date_dim_t d = date_dim[i13];
      if (!(d.d_date_sk == cs.cs_sold_date_sk)) {
        continue;
      }
      int tmp14 = -1;
      for (int i15 = 0; i15 < cs_len; i15++) {
        if (strcmp(cs[i15].name, cs.cs_call_center_sk) == 0) {
          tmp14 = i15;
          break;
        }
      }
      if (tmp14 == -1) {
        cs[cs_len].name = cs.cs_call_center_sk;
        cs[cs_len].count = 1;
        tmp14 = cs_len;
        cs_len++;
      } else {
        cs[tmp14].count++;
      }
    }
  }
  cr_item_t cr[10];
  int cr_len = 0;
  for (int i16 = 0; i16 < catalog_returns_len; i16++) {
    catalog_return_t cr = catalog_returns[i16];
    for (int i17 = 0; i17 < date_dim_len; i17++) {
      date_dim_t d = date_dim[i17];
      if (!(d.d_date_sk == cr.cr_returned_date_sk)) {
        continue;
      }
      int tmp18 = -1;
      for (int i19 = 0; i19 < cr_len; i19++) {
        if (strcmp(cr[i19].name, cr.cr_call_center_sk) == 0) {
          tmp18 = i19;
          break;
        }
      }
      if (tmp18 == -1) {
        cr[cr_len].name = cr.cr_call_center_sk;
        cr[cr_len].count = 1;
        tmp18 = cr_len;
        cr_len++;
      } else {
        cr[tmp18].count++;
      }
    }
  }
  ws_item_t ws[10];
  int ws_len = 0;
  for (int i20 = 0; i20 < web_sales_len; i20++) {
    web_sale_t ws = web_sales[i20];
    for (int i21 = 0; i21 < date_dim_len; i21++) {
      date_dim_t d = date_dim[i21];
      if (!(d.d_date_sk == ws.ws_sold_date_sk)) {
        continue;
      }
      int tmp22 = -1;
      for (int i23 = 0; i23 < ws_len; i23++) {
        if (strcmp(ws[i23].name, ws.ws_web_page_sk) == 0) {
          tmp22 = i23;
          break;
        }
      }
      if (tmp22 == -1) {
        ws[ws_len].name = ws.ws_web_page_sk;
        ws[ws_len].count = 1;
        tmp22 = ws_len;
        ws_len++;
      } else {
        ws[tmp22].count++;
      }
    }
  }
  wr_item_t wr[10];
  int wr_len = 0;
  for (int i24 = 0; i24 < web_returns_len; i24++) {
    web_return_t wr = web_returns[i24];
    for (int i25 = 0; i25 < date_dim_len; i25++) {
      date_dim_t d = date_dim[i25];
      if (!(d.d_date_sk == wr.wr_returned_date_sk)) {
        continue;
      }
      int tmp26 = -1;
      for (int i27 = 0; i27 < wr_len; i27++) {
        if (strcmp(wr[i27].name, wr.wr_web_page_sk) == 0) {
          tmp26 = i27;
          break;
        }
      }
      if (tmp26 == -1) {
        wr[wr_len].name = wr.wr_web_page_sk;
        wr[wr_len].count = 1;
        tmp26 = wr_len;
        wr_len++;
      } else {
        wr[tmp26].count++;
      }
    }
  }
  s_item_list_t tmp28 = s_item_list_t_create(ss.len * sr.len);
  int tmp29 = 0;
  for (int tmp30 = 0; tmp30 < ss_len; tmp30++) {
    ss_item_t s = ss[tmp30];
    int tmp32 = 0;
    for (int tmp31 = 0; tmp31 < sr_len; tmp31++) {
      sr_item_t r = sr[tmp31];
      if (!(s.s_store_sk == r.s_store_sk)) {
        continue;
      }
      tmp32 = 1;
      tmp28.data[tmp29] =
          (s_item_t){.channel = "store channel",
                     .id = s.s_store_sk,
                     .sales = s.sales,
                     .returns = ((r == 0) ? 0.0 : r.returns),
                     .profit = s.profit - (((r == 0) ? 0.0 : r.profit_loss))};
      tmp29++;
    }
    if (!tmp32) {
      sr_item_t r = (sr_item_t){0};
      tmp28.data[tmp29] =
          (s_item_t){.channel = "store channel",
                     .id = s.s_store_sk,
                     .sales = s.sales,
                     .returns = ((r == 0) ? 0.0 : r.returns),
                     .profit = s.profit - (((r == 0) ? 0.0 : r.profit_loss))};
      tmp29++;
    }
  }
  tmp28.len = tmp29;
  c_item_list_t tmp33 = c_item_list_t_create(cs.len * cr.len);
  int tmp34 = 0;
  for (int tmp35 = 0; tmp35 < cs_len; tmp35++) {
    cs_item_t c = cs[tmp35];
    for (int tmp36 = 0; tmp36 < cr_len; tmp36++) {
      cr_item_t r = cr[tmp36];
      if (!(c.cs_call_center_sk == r.cr_call_center_sk)) {
        continue;
      }
      tmp33.data[tmp34] = (c_item_t){.channel = "catalog channel",
                                     .id = c.cs_call_center_sk,
                                     .sales = c.sales,
                                     .returns = r.returns,
                                     .profit = c.profit - r.profit_loss};
      tmp34++;
    }
  }
  tmp33.len = tmp34;
  w_item_list_t tmp37 = w_item_list_t_create(ws.len * wr.len);
  int tmp38 = 0;
  for (int tmp39 = 0; tmp39 < ws_len; tmp39++) {
    ws_item_t w = ws[tmp39];
    int tmp41 = 0;
    for (int tmp40 = 0; tmp40 < wr_len; tmp40++) {
      wr_item_t r = wr[tmp40];
      if (!(w.wp_web_page_sk == r.wp_web_page_sk)) {
        continue;
      }
      tmp41 = 1;
      tmp37.data[tmp38] =
          (w_item_t){.channel = "web channel",
                     .id = w.wp_web_page_sk,
                     .sales = w.sales,
                     .returns = ((r == 0) ? 0.0 : r.returns),
                     .profit = w.profit - (((r == 0) ? 0.0 : r.profit_loss))};
      tmp38++;
    }
    if (!tmp41) {
      wr_item_t r = (wr_item_t){0};
      tmp37.data[tmp38] =
          (w_item_t){.channel = "web channel",
                     .id = w.wp_web_page_sk,
                     .sales = w.sales,
                     .returns = ((r == 0) ? 0.0 : r.returns),
                     .profit = w.profit - (((r == 0) ? 0.0 : r.profit_loss))};
      tmp38++;
    }
  }
  tmp37.len = tmp38;
  list_int per_channel = concat(tmp28, tmp33, tmp37);
  result_item_list_t result = (result_item_list_t){0, NULL};
  printf("[");
  for (int i42 = 0; i42 < result.len; i42++) {
    if (i42 > 0)
      printf(",");
    result_item_t it = result.data[i42];
    printf("{");
    _json_string("channel");
    printf(":");
    _json_int(it.channel);
    printf(",");
    _json_string("id");
    printf(":");
    _json_int(it.id);
    printf(",");
    _json_string("sales");
    printf(":");
    _json_float(it.sales);
    printf(",");
    _json_string("returns");
    printf(":");
    _json_float(it.returns);
    printf(",");
    _json_string("profit");
    printf(":");
    _json_float(it.profit);
    printf("}");
  }
  printf("]");
  test_TPCDS_Q77_simplified_result = result;
  test_TPCDS_Q77_simplified();
  return 0;
}
