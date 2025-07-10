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
  char **data;
} list_string;
static list_string list_string_create(int len) {
  list_string l;
  l.len = len;
  l.data = (char **)malloc(sizeof(char *) * len);
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
typedef struct {
  char *key;
  list_int items;
} _GroupString;
typedef struct {
  int len;
  int cap;
  _GroupString *data;
} list_group_string;
static list_group_string _group_by_string(list_string src) {
  list_group_string res;
  res.len = 0;
  res.cap = 0;
  res.data = NULL;
  for (int i = 0; i < src.len; i++) {
    char *key = src.data[i];
    int idx = -1;
    for (int j = 0; j < res.len; j++)
      if (strcmp(res.data[j].key, key) == 0) {
        idx = j;
        break;
      }
    if (idx == -1) {
      if (res.len >= res.cap) {
        res.cap = res.cap ? res.cap * 2 : 4;
        res.data =
            (_GroupString *)realloc(res.data, sizeof(_GroupString) * res.cap);
      }
      res.data[res.len].key = key;
      res.data[res.len].items = list_int_create(0);
      idx = res.len++;
    }
    _GroupString *g = &res.data[idx];
    g->items.data =
        (int *)realloc(g->items.data, sizeof(int) * (g->items.len + 1));
    g->items.data[g->items.len++] = i;
  }
  return res;
}
typedef struct {
  char *tag;
  int val;
} dataItem;
typedef struct {
  int len;
  dataItem *data;
} list_dataItem;
static list_dataItem list_dataItem_create(int len) {
  list_dataItem l;
  l.len = len;
  l.data = (dataItem *)malloc(sizeof(dataItem) * len);
  return l;
}

int main() {
  list_dataItem _t1 = list_dataItem_create(3);
  _t1.data[0] = (dataItem){.tag = "a", .val = 1};
  _t1.data[1] = (dataItem){.tag = "a", .val = 2};
  _t1.data[2] = (dataItem){.tag = "b", .val = 3};
  __auto_type data = _t1;
  list_dataItem _t2 = list_dataItem_create(data.len);
  list_string _t3 = list_string_create(data.len);
  int _t4 = 0;
  for (int i = 0; i < data.len; i++) {
    dataItem d = data.data[i];
    _t2.data[_t4] = d;
    _t3.data[_t4] = d.tag;
    _t4++;
  }
  _t2.len = _t4;
  _t3.len = _t4;
  list_group_string _t5 = _group_by_string(_t3);
  list_int _t6 = list_int_create(_t5.len);
  int _t7 = 0;
  for (int gi = 0; gi < _t5.len; gi++) {
    _GroupString _gp = _t5.data[gi];
    list_dataItem _t8 = list_dataItem_create(_gp.items.len);
    for (int j = 0; j < _gp.items.len; j++) {
      _t8.data[j] = _t2.data[_gp.items.data[j]];
    }
    _t8.len = _gp.items.len;
    struct {
      char *key;
      list_dataItem items;
    } g = {_gp.key, _t8};
    _t6.data[_t7] = g;
    _t7++;
  }
  _t6.len = _t7;
  __auto_type groups = _t6;
  __auto_type tmp = _t9;
  for (int _t10 = 0; _t10 < groups.len; _t10++) {
    int g = groups.data[_t10];
    __auto_type total = 0;
    for (int _t11 = 0; _t11 < g.items.len; _t11++) {
      dataItem x = g.items.data[_t11];
      total = total + x.val;
    }
    map_int_bool _t12 = map_int_bool_create(2);
    map_int_bool_put(&_t12, "tag", g.key);
    map_int_bool_put(&_t12, "total", total);
    tmp = 0;
  }
  list_int _t13 = list_int_create(tmp.len);
  int *_t16 = (int *)malloc(sizeof(int) * tmp.len);
  int _t14 = 0;
  for (int _t15 = 0; _t15 < tmp.len; _t15++) {
    int r = tmp.data[_t15];
    _t13.data[_t14] = r;
    _t16[_t14] = r.tag;
    _t14++;
  }
  _t13.len = _t14;
  for (int i = 0; i < _t14 - 1; i++) {
    for (int j = i + 1; j < _t14; j++) {
      if (_t16[i] > _t16[j]) {
        int _t17 = _t16[i];
        _t16[i] = _t16[j];
        _t16[j] = _t17;
        int _t18 = _t13.data[i];
        _t13.data[i] = _t13.data[j];
        _t13.data[j] = _t18;
      }
    }
  }
  __auto_type result = _t13;
  printf("%d\n", result);
  return 0;
}
