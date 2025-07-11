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
static double _sum_float(list_float v) {
  double sum = 0;
  for (int i = 0; i < v.len; i++)
    sum += v.data[i];
  return sum;
}
typedef struct {
  int key;
  list_int items;
} _GroupInt;
typedef struct {
  int len;
  int cap;
  _GroupInt *data;
} list_group_int;
static list_group_int _group_by_int(list_int src) {
  list_group_int res;
  res.len = 0;
  res.cap = 0;
  res.data = NULL;
  for (int i = 0; i < src.len; i++) {
    int key = src.data[i];
    int idx = -1;
    for (int j = 0; j < res.len; j++)
      if (res.data[j].key == key) {
        idx = j;
        break;
      }
    if (idx == -1) {
      if (res.len >= res.cap) {
        res.cap = res.cap ? res.cap * 2 : 4;
        res.data = (_GroupInt *)realloc(res.data, sizeof(_GroupInt) * res.cap);
      }
      res.data[res.len].key = key;
      res.data[res.len].items = list_int_create(0);
      idx = res.len++;
    }
    _GroupInt *g = &res.data[idx];
    g->items.data =
        (int *)realloc(g->items.data, sizeof(int) * (g->items.len + 1));
    g->items.data[g->items.len++] = src.data[i];
  }
  return res;
}
typedef struct {
  int id;
  char *name;
} nationsItem;
typedef struct {
  int len;
  nationsItem *data;
} list_nationsItem;
static list_nationsItem list_nationsItem_create(int len) {
  list_nationsItem l;
  l.len = len;
  l.data = calloc(len, sizeof(nationsItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  int nation;
} suppliersItem;
typedef struct {
  int len;
  suppliersItem *data;
} list_suppliersItem;
static list_suppliersItem list_suppliersItem_create(int len) {
  list_suppliersItem l;
  l.len = len;
  l.data = calloc(len, sizeof(suppliersItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int part;
  int supplier;
  double cost;
  int qty;
} partsuppItem;
typedef struct {
  int len;
  partsuppItem *data;
} list_partsuppItem;
static list_partsuppItem list_partsuppItem_create(int len) {
  list_partsuppItem l;
  l.len = len;
  l.data = calloc(len, sizeof(partsuppItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int part;
  double value;
} filteredItem;
typedef struct {
  int len;
  filteredItem *data;
} list_filteredItem;
static list_filteredItem list_filteredItem_create(int len) {
  list_filteredItem l;
  l.len = len;
  l.data = calloc(len, sizeof(filteredItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int part;
  double total;
} groupedItem;
typedef struct {
  int len;
  groupedItem *data;
} list_groupedItem;
static list_groupedItem list_groupedItem_create(int len) {
  list_groupedItem l;
  l.len = len;
  l.data = calloc(len, sizeof(groupedItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  nationsItem tmp1_data[] = {(nationsItem){.id = 1, .name = "A"},
                             (nationsItem){.id = 2, .name = "B"}};
  list_nationsItem tmp1 = {2, tmp1_data};
  list_nationsItem nations = tmp1;
  suppliersItem tmp2_data[] = {(suppliersItem){.id = 1, .nation = 1},
                               (suppliersItem){.id = 2, .nation = 2}};
  list_suppliersItem tmp2 = {2, tmp2_data};
  list_suppliersItem suppliers = tmp2;
  partsuppItem tmp3_data[] = {
      (partsuppItem){.part = 100, .supplier = 1, .cost = 10.0, .qty = 2},
      (partsuppItem){.part = 100, .supplier = 2, .cost = 20.0, .qty = 1},
      (partsuppItem){.part = 200, .supplier = 1, .cost = 5.0, .qty = 3}};
  list_partsuppItem tmp3 = {3, tmp3_data};
  list_partsuppItem partsupp = tmp3;
  list_filteredItem tmp4 =
      list_filteredItem_create(partsupp.len * suppliers.len * nations.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < partsupp.len; tmp6++) {
    partsuppItem ps = partsupp.data[tmp6];
    for (int tmp7 = 0; tmp7 < suppliers.len; tmp7++) {
      suppliersItem s = suppliers.data[tmp7];
      if (!(s.id == ps.supplier)) {
        continue;
      }
      for (int tmp8 = 0; tmp8 < nations.len; tmp8++) {
        nationsItem n = nations.data[tmp8];
        if (!(n.id == s.nation)) {
          continue;
        }
        if (!((strcmp(n.name, "A") == 0))) {
          continue;
        }
        tmp4.data[tmp5] =
            (filteredItem){.part = ps.part, .value = ps.cost * ps.qty};
        tmp5++;
      }
    }
  }
  tmp4.len = tmp5;
  list_filteredItem filtered = tmp4;
  list_filteredItem tmp9 = list_filteredItem_create(filtered.len);
  list_int tmp10 = list_int_create(filtered.len);
  int tmp11 = 0;
  for (int i = 0; i < filtered.len; i++) {
    filteredItem x = filtered.data[i];
    tmp9.data[tmp11] = x;
    tmp10.data[tmp11] = x.part;
    tmp11++;
  }
  tmp9.len = tmp11;
  tmp10.len = tmp11;
  list_group_int tmp12 = _group_by_int(tmp10);
  list_groupedItem tmp13 = list_groupedItem_create(tmp12.len);
  int tmp14 = 0;
  for (int gi = 0; gi < tmp12.len; gi++) {
    _GroupInt _gp = tmp12.data[gi];
    list_filteredItem tmp15 = list_filteredItem_create(_gp.items.len);
    for (int j = 0; j < _gp.items.len; j++) {
      tmp15.data[j] = tmp9.data[_gp.items.data[j]];
    }
    tmp15.len = _gp.items.len;
    struct {
      int key;
      list_filteredItem items;
    } g = {_gp.key, tmp15};
    list_float tmp16 = list_float_create(g.items.len);
    int tmp17 = 0;
    for (int i = 0; i < g.items.len; i++) {
      filteredItem r = g.items.data[i];
      tmp16.data[tmp17] = r.value;
      tmp17++;
    }
    tmp16.len = tmp17;
    tmp13.data[tmp14] =
        (groupedItem){.part = g.key, .total = _sum_float(tmp16)};
    tmp14++;
  }
  tmp13.len = tmp14;
  list_groupedItem grouped = tmp13;
  printf("%.16g\n", grouped);
  return 0;
}
