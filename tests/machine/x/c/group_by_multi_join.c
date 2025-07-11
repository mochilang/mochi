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
} NationsItem;
typedef struct {
  int len;
  NationsItem *data;
} list_NationsItem;
static list_NationsItem list_NationsItem_create(int len) {
  list_NationsItem l;
  l.len = len;
  l.data = calloc(len, sizeof(NationsItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _print_list_nationsItem(list_NationsItem v) {
  for (int i = 0; i < v.len; i++) {
    NationsItem s = v.data[i];
    printf("map[");
    printf("id:");
    printf("%d", s.id);
    printf(" ");
    printf("name:");
    printf("%s", s.name);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

typedef struct {
  int id;
  int nation;
} SuppliersItem;
typedef struct {
  int len;
  SuppliersItem *data;
} list_SuppliersItem;
static list_SuppliersItem list_SuppliersItem_create(int len) {
  list_SuppliersItem l;
  l.len = len;
  l.data = calloc(len, sizeof(SuppliersItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _print_list_suppliersItem(list_SuppliersItem v) {
  for (int i = 0; i < v.len; i++) {
    SuppliersItem s = v.data[i];
    printf("map[");
    printf("id:");
    printf("%d", s.id);
    printf(" ");
    printf("nation:");
    printf("%d", s.nation);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

typedef struct {
  int part;
  int supplier;
  double cost;
  int qty;
} PartsuppItem;
typedef struct {
  int len;
  PartsuppItem *data;
} list_PartsuppItem;
static list_PartsuppItem list_PartsuppItem_create(int len) {
  list_PartsuppItem l;
  l.len = len;
  l.data = calloc(len, sizeof(PartsuppItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _print_list_partsuppItem(list_PartsuppItem v) {
  for (int i = 0; i < v.len; i++) {
    PartsuppItem s = v.data[i];
    printf("map[");
    printf("part:");
    printf("%d", s.part);
    printf(" ");
    printf("supplier:");
    printf("%d", s.supplier);
    printf(" ");
    printf("cost:");
    printf("%g", s.cost);
    printf(" ");
    printf("qty:");
    printf("%d", s.qty);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

typedef struct {
  int part;
  double value;
} FilteredItem;
typedef struct {
  int len;
  FilteredItem *data;
} list_FilteredItem;
static list_FilteredItem list_FilteredItem_create(int len) {
  list_FilteredItem l;
  l.len = len;
  l.data = calloc(len, sizeof(FilteredItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _print_list_filteredItem(list_FilteredItem v) {
  for (int i = 0; i < v.len; i++) {
    FilteredItem s = v.data[i];
    printf("map[");
    printf("part:");
    printf("%d", s.part);
    printf(" ");
    printf("value:");
    printf("%g", s.value);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

typedef struct {
  int part;
  double total;
} GroupedItem;
typedef struct {
  int len;
  GroupedItem *data;
} list_GroupedItem;
static list_GroupedItem list_GroupedItem_create(int len) {
  list_GroupedItem l;
  l.len = len;
  l.data = calloc(len, sizeof(GroupedItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _print_list_groupedItem(list_GroupedItem v) {
  for (int i = 0; i < v.len; i++) {
    GroupedItem s = v.data[i];
    printf("map[");
    printf("part:");
    printf("%d", s.part);
    printf(" ");
    printf("total:");
    printf("%g", s.total);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

int main() {
  NationsItem tmp1_data[] = {(NationsItem){.id = 1, .name = "A"},
                             (NationsItem){.id = 2, .name = "B"}};
  list_NationsItem tmp1 = {2, tmp1_data};
  list_NationsItem nations = tmp1;
  SuppliersItem tmp2_data[] = {(SuppliersItem){.id = 1, .nation = 1},
                               (SuppliersItem){.id = 2, .nation = 2}};
  list_SuppliersItem tmp2 = {2, tmp2_data};
  list_SuppliersItem suppliers = tmp2;
  PartsuppItem tmp3_data[] = {
      (PartsuppItem){.part = 100, .supplier = 1, .cost = 10.0, .qty = 2},
      (PartsuppItem){.part = 100, .supplier = 2, .cost = 20.0, .qty = 1},
      (PartsuppItem){.part = 200, .supplier = 1, .cost = 5.0, .qty = 3}};
  list_PartsuppItem tmp3 = {3, tmp3_data};
  list_PartsuppItem partsupp = tmp3;
  list_FilteredItem tmp4 =
      list_FilteredItem_create(partsupp.len * suppliers.len * nations.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < partsupp.len; tmp6++) {
    PartsuppItem ps = partsupp.data[tmp6];
    for (int tmp7 = 0; tmp7 < suppliers.len; tmp7++) {
      SuppliersItem s = suppliers.data[tmp7];
      if (!(s.id == ps.supplier)) {
        continue;
      }
      for (int tmp8 = 0; tmp8 < nations.len; tmp8++) {
        NationsItem n = nations.data[tmp8];
        if (!(n.id == s.nation)) {
          continue;
        }
        if (!((strcmp(n.name, "A") == 0))) {
          continue;
        }
        tmp4.data[tmp5] =
            (FilteredItem){.part = ps.part, .value = ps.cost * ps.qty};
        tmp5++;
      }
    }
  }
  tmp4.len = tmp5;
  list_FilteredItem filtered = tmp4;
  list_FilteredItem tmp9 = list_FilteredItem_create(filtered.len);
  list_int tmp10 = list_int_create(filtered.len);
  int tmp11 = 0;
  for (int i12 = 0; i12 < filtered.len; i12++) {
    FilteredItem x = filtered.data[i12];
    tmp9.data[tmp11] = x;
    tmp10.data[tmp11] = x.part;
    tmp11++;
  }
  tmp9.len = tmp11;
  tmp10.len = tmp11;
  list_group_int tmp13 = _group_by_int(tmp10);
  list_GroupedItem tmp14 = list_GroupedItem_create(tmp13.len);
  int tmp15 = 0;
  for (int gi = 0; gi < tmp13.len; gi++) {
    _GroupInt _gp = tmp13.data[gi];
    list_FilteredItem tmp16 = list_FilteredItem_create(_gp.items.len);
    for (int i17 = 0; i17 < _gp.items.len; i17++) {
      tmp16.data[i17] = tmp9.data[_gp.items.data[i17]];
    }
    tmp16.len = _gp.items.len;
    struct {
      int key;
      list_FilteredItem items;
    } g = {_gp.key, tmp16};
    list_float tmp18 = list_float_create(g.items.len);
    int tmp19 = 0;
    for (int i20 = 0; i20 < g.items.len; i20++) {
      FilteredItem r = g.items.data[i20];
      tmp18.data[tmp19] = r.value;
      tmp19++;
    }
    tmp18.len = tmp19;
    tmp14.data[tmp15] =
        (GroupedItem){.part = g.key, .total = _sum_float(tmp18)};
    tmp15++;
  }
  tmp14.len = tmp15;
  list_GroupedItem grouped = tmp14;
  _print_list_groupedItem(grouped);
  printf("\n");
  return 0;
}
