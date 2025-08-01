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
typedef struct Inventory Inventory;
typedef struct Warehouse Warehouse;
typedef struct Item Item;
typedef struct DateDim DateDim;

typedef struct {
  const char *w_warehouse_name;
  const char *i_item_id;
  int inv_before;
  int inv_after;
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
  const char *w_warehouse_name;
  const char *i_item_id;
  int inv_before;
  int inv_after;
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
  int inv_item_sk;
  int inv_warehouse_sk;
  int inv_date_sk;
  int inv_quantity_on_hand;
} inventory_t;

typedef struct {
  int w_warehouse_sk;
  const char *w_warehouse_name;
} warehouse_t;

typedef struct {
  int i_item_sk;
  const char *i_item_id;
} item_t;

typedef struct {
  int d_date_sk;
  const char *d_date;
} date_dim_t;

typedef struct {
  int w;
  int i;
  double qty;
} before_item_t;
typedef struct {
  int len;
  before_item_t *data;
} before_item_list_t;
before_item_list_t create_before_item_list(int len) {
  before_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(before_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int w;
  int i;
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
  int w;
  int i;
  double qty;
} after_item_t;
typedef struct {
  int len;
  after_item_t *data;
} after_item_list_t;
after_item_list_t create_after_item_list(int len) {
  after_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(after_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  const char *w_name;
  const char *i_id;
  double before_qty;
  double after_qty;
  double ratio;
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
  const char *w_warehouse_name;
  const char *i_item_id;
  double inv_before;
  double inv_after;
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

typedef struct Inventory {
  int inv_item_sk;
  int inv_warehouse_sk;
  int inv_date_sk;
  int inv_quantity_on_hand;
} Inventory;
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

typedef struct Warehouse {
  int w_warehouse_sk;
  char *w_warehouse_name;
} Warehouse;
typedef struct {
  int len;
  warehouse_t *data;
} warehouse_list_t;
warehouse_list_t create_warehouse_list(int len) {
  warehouse_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(warehouse_t));
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

typedef struct DateDim {
  int d_date_sk;
  char *d_date;
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

static list_int test_TPCDS_Q21_inventory_ratio_result;
static void test_TPCDS_Q21_inventory_ratio() {
  tmp1_t tmp1[] = {(tmp1_t){.w_warehouse_name = "Backup",
                            .i_item_id = "ITEM2",
                            .inv_before = 20,
                            .inv_after = 20},
                   (tmp_item_t){.w_warehouse_name = "Main",
                                .i_item_id = "ITEM1",
                                .inv_before = 30,
                                .inv_after = 40}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_TPCDS_Q21_inventory_ratio_result.len != tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0; i3 < test_TPCDS_Q21_inventory_ratio_result.len; i3++) {
      if (test_TPCDS_Q21_inventory_ratio_result.data[i3] != tmp1.data[i3]) {
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
  inventory_t inventory[] = {(inventory_t){.inv_item_sk = 1,
                                           .inv_warehouse_sk = 1,
                                           .inv_date_sk = 1,
                                           .inv_quantity_on_hand = 30},
                             (inventory_t){.inv_item_sk = 1,
                                           .inv_warehouse_sk = 1,
                                           .inv_date_sk = 2,
                                           .inv_quantity_on_hand = 40},
                             (inventory_t){.inv_item_sk = 2,
                                           .inv_warehouse_sk = 2,
                                           .inv_date_sk = 1,
                                           .inv_quantity_on_hand = 20},
                             (inventory_t){.inv_item_sk = 2,
                                           .inv_warehouse_sk = 2,
                                           .inv_date_sk = 2,
                                           .inv_quantity_on_hand = 20}};
  int inventory_len = sizeof(inventory) / sizeof(inventory[0]);
  warehouse_t warehouse[] = {
      (warehouse_t){.w_warehouse_sk = 1, .w_warehouse_name = "Main"},
      (warehouse_t){.w_warehouse_sk = 2, .w_warehouse_name = "Backup"}};
  int warehouse_len = sizeof(warehouse) / sizeof(warehouse[0]);
  item_t item[] = {(item_t){.i_item_sk = 1, .i_item_id = "ITEM1"},
                   (item_t){.i_item_sk = 2, .i_item_id = "ITEM2"}};
  int item_len = sizeof(item) / sizeof(item[0]);
  date_dim_t date_dim[] = {
      (date_dim_t){.d_date_sk = 1, .d_date = "2000-03-01"},
      (date_dim_t){.d_date_sk = 2, .d_date = "2000-03-20"}};
  int date_dim_len = sizeof(date_dim) / sizeof(date_dim[0]);
  before_item_t before[10];
  int before_len = 0;
  for (int i4 = 0; i4 < inventory_len; i4++) {
    inventory_t inv = inventory[i4];
    for (int i5 = 0; i5 < date_dim_len; i5++) {
      date_dim_t d = date_dim[i5];
      if (!(inv.inv_date_sk == d.d_date_sk)) {
        continue;
      }
      if (!((strcmp(d.d_date, "2000-03-15") < 0))) {
        continue;
      }
      int tmp6 = -1;
      for (int i7 = 0; i7 < before_len; i7++) {
        if (strcmp(before[i7].name, (tmp_item1_t){.w = inv.inv_warehouse_sk,
                                                  .i = inv.inv_item_sk}) == 0) {
          tmp6 = i7;
          break;
        }
      }
      if (tmp6 == -1) {
        before[before_len].name =
            (tmp_item1_t){.w = inv.inv_warehouse_sk, .i = inv.inv_item_sk};
        before[before_len].count = 1;
        tmp6 = before_len;
        before_len++;
      } else {
        before[tmp6].count++;
      }
    }
  }
  after_item_t after[10];
  int after_len = 0;
  for (int i8 = 0; i8 < inventory_len; i8++) {
    inventory_t inv = inventory[i8];
    for (int i9 = 0; i9 < date_dim_len; i9++) {
      date_dim_t d = date_dim[i9];
      if (!(inv.inv_date_sk == d.d_date_sk)) {
        continue;
      }
      if (!((strcmp(d.d_date, "2000-03-15") >= 0))) {
        continue;
      }
      int tmp10 = -1;
      for (int i11 = 0; i11 < after_len; i11++) {
        if (strcmp(after[i11].name, (tmp_item1_t){.w = inv.inv_warehouse_sk,
                                                  .i = inv.inv_item_sk}) == 0) {
          tmp10 = i11;
          break;
        }
      }
      if (tmp10 == -1) {
        after[after_len].name =
            (tmp_item1_t){.w = inv.inv_warehouse_sk, .i = inv.inv_item_sk};
        after[after_len].count = 1;
        tmp10 = after_len;
        after_len++;
      } else {
        after[tmp10].count++;
      }
    }
  }
  joined_item_list_t tmp12 = joined_item_list_t_create(
      before.len * after.len * warehouse.len * item.len);
  int tmp13 = 0;
  for (int tmp14 = 0; tmp14 < before_len; tmp14++) {
    before_item_t b = before[tmp14];
    for (int tmp15 = 0; tmp15 < after_len; tmp15++) {
      after_item_t a = after[tmp15];
      if (!(b.w == a.w && b.i == a.i)) {
        continue;
      }
      for (int tmp16 = 0; tmp16 < warehouse_len; tmp16++) {
        warehouse_t w = warehouse[tmp16];
        if (!(w.w_warehouse_sk == b.w)) {
          continue;
        }
        for (int tmp17 = 0; tmp17 < item_len; tmp17++) {
          item_t it = item[tmp17];
          if (!(it.i_item_sk == b.i)) {
            continue;
          }
          tmp12.data[tmp13] = (joined_item_t){.w_name = w.w_warehouse_name,
                                              .i_id = it.i_item_id,
                                              .before_qty = b.qty,
                                              .after_qty = a.qty,
                                              .ratio = a.qty / b.qty};
          tmp13++;
        }
      }
    }
  }
  tmp12.len = tmp13;
  joined_item_list_t joined = tmp12;
  result_item_list_t tmp18 = create_result_item_list(joined.len);
  int *tmp21 = (int *)malloc(sizeof(int) * joined.len);
  int tmp19 = 0;
  for (int tmp20 = 0; tmp20 < joined.len; tmp20++) {
    joined_item_t r = joined.data[tmp20];
    if (!(r.ratio >= (2.0 / 3.0) && r.ratio <= (3.0 / 2.0))) {
      continue;
    }
    tmp18.data[tmp19] = (result_item_t){.w_warehouse_name = r.w_name,
                                        .i_item_id = r.i_id,
                                        .inv_before = r.before_qty,
                                        .inv_after = r.after_qty};
    list_string result = list_string_create(2);
    result.data[0] = r.w_name;
    result.data[1] = r.i_id;
    tmp21[tmp19] = result;
    tmp19++;
  }
  tmp18.len = tmp19;
  for (int i24 = 0; i24 < tmp19 - 1; i24++) {
    for (int i25 = i24 + 1; i25 < tmp19; i25++) {
      if (tmp21[i24] > tmp21[i25]) {
        int tmp22 = tmp21[i24];
        tmp21[i24] = tmp21[i25];
        tmp21[i25] = tmp22;
        result_item_t tmp23 = tmp18.data[i24];
        tmp18.data[i24] = tmp18.data[i25];
        tmp18.data[i25] = tmp23;
      }
    }
  }
  result_item_list_t result = tmp18;
  printf("[");
  for (int i26 = 0; i26 < result.len; i26++) {
    if (i26 > 0)
      printf(",");
    result_item_t it = result.data[i26];
    printf("{");
    _json_string("w_warehouse_name");
    printf(":");
    _json_string(it.w_warehouse_name);
    printf(",");
    _json_string("i_item_id");
    printf(":");
    _json_string(it.i_item_id);
    printf(",");
    _json_string("inv_before");
    printf(":");
    _json_float(it.inv_before);
    printf(",");
    _json_string("inv_after");
    printf(":");
    _json_float(it.inv_after);
    printf("}");
  }
  printf("]");
  test_TPCDS_Q21_inventory_ratio_result = result;
  test_TPCDS_Q21_inventory_ratio();
  free(result.data);
  return 0;
}
