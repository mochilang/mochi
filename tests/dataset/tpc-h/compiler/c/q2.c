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
  l.data = (int *)malloc(sizeof(int) * len);
  return l;
}
typedef struct {
  int len;
  double *data;
} list_float;
static list_float list_float_create(int len) {
  list_float l;
  l.len = len;
  l.data = (double *)malloc(sizeof(double) * len);
  return l;
}
typedef struct {
  int len;
  char **data;
} list_string;
static list_string list_string_create(int len) {
  list_string l;
  l.len = len;
  l.data = (char **)malloc(sizeof(char *) * len);
  return l;
}
typedef struct {
  int len;
  list_int *data;
} list_list_int;
static list_list_int list_list_int_create(int len) {
  list_list_int l;
  l.len = len;
  l.data = (list_int *)malloc(sizeof(list_int) * len);
  return l;
}
typedef struct {
  int key;
  int value;
} map_int_bool_item;
static map_int_bool_item *map_int_bool_item_new(int key, int value) {
  map_int_bool_item *it =
      (map_int_bool_item *)malloc(sizeof(map_int_bool_item));
  it->key = key;
  it->value = value;
  return it;
}
typedef struct {
  int len;
  int cap;
  map_int_bool_item **data;
} map_int_bool;
static map_int_bool map_int_bool_create(int cap) {
  map_int_bool m;
  m.len = 0;
  m.cap = cap;
  m.data = cap ? (map_int_bool_item **)malloc(sizeof(map_int_bool_item *) * cap)
               : NULL;
  return m;
}
static void map_int_bool_put(map_int_bool *m, int key, int value) {
  for (int i = 0; i < m->len; i++)
    if (m->data[i]->key == key) {
      m->data[i]->value = value;
      return;
    }
  if (m->len >= m->cap) {
    m->cap = m->cap ? m->cap * 2 : 4;
    m->data = (map_int_bool_item **)realloc(
        m->data, sizeof(map_int_bool_item *) * m->cap);
  }
  m->data[m->len++] = map_int_bool_item_new(key, value);
}
static int map_int_bool_contains(map_int_bool m, int key) {
  for (int i = 0; i < m.len; i++)
    if (m.data[i]->key == key)
      return 1;
  return 0;
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
static void
test_Q2_returns_only_supplier_with_min_cost_in_Europe_for_brass_part() {
  list_int _t1 = list_int_create(1);
  map_int_bool _t2 = map_int_bool_create(9);
  map_int_bool_put(&_t2, s_acctbal, 1000.0);
  map_int_bool_put(&_t2, s_name, "BestSupplier");
  map_int_bool_put(&_t2, n_name, "FRANCE");
  map_int_bool_put(&_t2, p_partkey, 1000);
  map_int_bool_put(&_t2, p_mfgr, "M1");
  map_int_bool_put(&_t2, s_address, "123 Rue");
  map_int_bool_put(&_t2, s_phone, "123");
  map_int_bool_put(&_t2, s_comment, "Fast and reliable");
  map_int_bool_put(&_t2, ps_supplycost, 10.0);
  _t1.data[0] = _t2;
  if (!((result == _t1))) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  typedef struct {
    int r_regionkey;
    char *r_name;
  } regionItem;
  typedef struct {
    int len;
    regionItem *data;
  } list_regionItem;
  static list_regionItem list_regionItem_create(int len) {
    list_regionItem l;
    l.len = len;
    l.data = (regionItem *)malloc(sizeof(regionItem) * len);
    return l;
  }
  list_regionItem _t3 = list_regionItem_create(2);
  _t3.data[0] = (regionItem){.r_regionkey = 1, .r_name = "EUROPE"};
  _t3.data[1] = (regionItem){.r_regionkey = 2, .r_name = "ASIA"};
  int region = _t3;
  typedef struct {
    int n_nationkey;
    int n_regionkey;
    char *n_name;
  } nationItem;
  typedef struct {
    int len;
    nationItem *data;
  } list_nationItem;
  static list_nationItem list_nationItem_create(int len) {
    list_nationItem l;
    l.len = len;
    l.data = (nationItem *)malloc(sizeof(nationItem) * len);
    return l;
  }
  list_nationItem _t4 = list_nationItem_create(2);
  _t4.data[0] =
      (nationItem){.n_nationkey = 10, .n_regionkey = 1, .n_name = "FRANCE"};
  _t4.data[1] =
      (nationItem){.n_nationkey = 20, .n_regionkey = 2, .n_name = "CHINA"};
  int nation = _t4;
  typedef struct {
    int s_suppkey;
    char *s_name;
    char *s_address;
    int s_nationkey;
    char *s_phone;
    double s_acctbal;
    char *s_comment;
  } supplierItem;
  typedef struct {
    int len;
    supplierItem *data;
  } list_supplierItem;
  static list_supplierItem list_supplierItem_create(int len) {
    list_supplierItem l;
    l.len = len;
    l.data = (supplierItem *)malloc(sizeof(supplierItem) * len);
    return l;
  }
  list_supplierItem _t5 = list_supplierItem_create(2);
  _t5.data[0] = (supplierItem){.s_suppkey = 100,
                               .s_name = "BestSupplier",
                               .s_address = "123 Rue",
                               .s_nationkey = 10,
                               .s_phone = "123",
                               .s_acctbal = 1000.0,
                               .s_comment = "Fast and reliable"};
  _t5.data[1] = (supplierItem){.s_suppkey = 200,
                               .s_name = "AltSupplier",
                               .s_address = "456 Way",
                               .s_nationkey = 20,
                               .s_phone = "456",
                               .s_acctbal = 500.0,
                               .s_comment = "Slow"};
  int supplier = _t5;
  typedef struct {
    int p_partkey;
    char *p_type;
    int p_size;
    char *p_mfgr;
  } partItem;
  typedef struct {
    int len;
    partItem *data;
  } list_partItem;
  static list_partItem list_partItem_create(int len) {
    list_partItem l;
    l.len = len;
    l.data = (partItem *)malloc(sizeof(partItem) * len);
    return l;
  }
  list_partItem _t6 = list_partItem_create(2);
  _t6.data[0] = (partItem){
      .p_partkey = 1000, .p_type = "LARGE BRASS", .p_size = 15, .p_mfgr = "M1"};
  _t6.data[1] = (partItem){.p_partkey = 2000,
                           .p_type = "SMALL COPPER",
                           .p_size = 15,
                           .p_mfgr = "M2"};
  int part = _t6;
  typedef struct {
    int ps_partkey;
    int ps_suppkey;
    double ps_supplycost;
  } partsuppItem;
  typedef struct {
    int len;
    partsuppItem *data;
  } list_partsuppItem;
  static list_partsuppItem list_partsuppItem_create(int len) {
    list_partsuppItem l;
    l.len = len;
    l.data = (partsuppItem *)malloc(sizeof(partsuppItem) * len);
    return l;
  }
  list_partsuppItem _t7 = list_partsuppItem_create(2);
  _t7.data[0] = (partsuppItem){
      .ps_partkey = 1000, .ps_suppkey = 100, .ps_supplycost = 10.0};
  _t7.data[1] = (partsuppItem){
      .ps_partkey = 1000, .ps_suppkey = 200, .ps_supplycost = 15.0};
  int partsupp = _t7;
  int europe_nations = 0;
  list_int europe_suppliers = 0;
  int _t8 = int_create(part.len);
  int _t9 = 0;
  for (int _t10 = 0; _t10 < part.len; _t10++) {
    partItem p = part.data[_t10];
    if (!((((p.p_size == 15) && p.p_type) == "LARGE BRASS"))) {
      continue;
    }
    _t8.data[_t9] = p;
    _t9++;
  }
  _t8.len = _t9;
  int target_parts = _t8;
  list_int target_partsupp = 0;
  list_int _t11 = list_int_create(target_partsupp.len);
  int _t12 = 0;
  for (int _t13 = 0; _t13 < target_partsupp.len; _t13++) {
    int x = target_partsupp.data[_t13];
    _t11.data[_t12] = x.ps_supplycost;
    _t12++;
  }
  _t11.len = _t12;
  list_int costs = _t11;
  int min_cost = min(costs);
  list_int _t14 = list_int_create(target_partsupp.len);
  int *_t17 = (int *)malloc(sizeof(int) * target_partsupp.len);
  int _t15 = 0;
  for (int _t16 = 0; _t16 < target_partsupp.len; _t16++) {
    int x = target_partsupp.data[_t16];
    if (!((x.ps_supplycost == min_cost))) {
      continue;
    }
    _t14.data[_t15] = x;
    _t17[_t15] = (-x.s_acctbal);
    _t15++;
  }
  _t14.len = _t15;
  for (int i = 0; i < _t15 - 1; i++) {
    for (int j = i + 1; j < _t15; j++) {
      if (_t17[i] > _t17[j]) {
        int _t18 = _t17[i];
        _t17[i] = _t17[j];
        _t17[j] = _t18;
        int _t19 = _t14.data[i];
        _t14.data[i] = _t14.data[j];
        _t14.data[j] = _t19;
      }
    }
    list_int result = _t14;
    _json_int(result);
    test_Q2_returns_only_supplier_with_min_cost_in_Europe_for_brass_part();
    return 0;
  }
