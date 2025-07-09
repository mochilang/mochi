#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
  l.data = (nationsItem *)malloc(sizeof(nationsItem) * len);
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
  l.data = (suppliersItem *)malloc(sizeof(suppliersItem) * len);
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
  l.data = (partsuppItem *)malloc(sizeof(partsuppItem) * len);
  return l;
}

typedef struct {
  int part;
  int value;
} filteredItem;
typedef struct {
  int len;
  filteredItem *data;
} list_filteredItem;
static list_filteredItem list_filteredItem_create(int len) {
  list_filteredItem l;
  l.len = len;
  l.data = (filteredItem *)malloc(sizeof(filteredItem) * len);
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
  l.data = (groupedItem *)malloc(sizeof(groupedItem) * len);
  return l;
}

int main() {
  list_nationsItem _t1 = list_nationsItem_create(2);
  _t1.data[0] = (nationsItem){.id = 1, .name = "A"};
  _t1.data[1] = (nationsItem){.id = 2, .name = "B"};
  list_nationsItem nations = _t1;
  list_suppliersItem _t2 = list_suppliersItem_create(2);
  _t2.data[0] = (suppliersItem){.id = 1, .nation = 1};
  _t2.data[1] = (suppliersItem){.id = 2, .nation = 2};
  list_suppliersItem suppliers = _t2;
  list_partsuppItem _t3 = list_partsuppItem_create(3);
  _t3.data[0] =
      (partsuppItem){.part = 100, .supplier = 1, .cost = 10.0, .qty = 2};
  _t3.data[1] =
      (partsuppItem){.part = 100, .supplier = 2, .cost = 20.0, .qty = 1};
  _t3.data[2] =
      (partsuppItem){.part = 200, .supplier = 1, .cost = 5.0, .qty = 3};
  list_partsuppItem partsupp = _t3;
  list_int _t4 = list_int_create(partsupp.len * suppliers.len * nations.len);
  int _t5 = 0;
  for (int _t6 = 0; _t6 < partsupp.len; _t6++) {
    partsuppItem ps = partsupp.data[_t6];
    for (int _t7 = 0; _t7 < suppliers.len; _t7++) {
      suppliersItem s = suppliers.data[_t7];
      if (!(s.id == ps.supplier)) {
        continue;
      }
      for (int _t8 = 0; _t8 < nations.len; _t8++) {
        nationsItem n = nations.data[_t8];
        if (!(n.id == s.nation)) {
          continue;
        }
        if (!((strcmp(n.name, "A") == 0))) {
          continue;
        }
        _t4.data[_t5] =
            (filteredItem){.part = ps.part, .value = ps.cost * ps.qty};
        _t5++;
      }
    }
  }
  _t4.len = _t5;
  list_filteredItem filtered = _t4;
  list_groupedItem grouped = 0;
  printf("%d\n", grouped);
  return 0;
}
