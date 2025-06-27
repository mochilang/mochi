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
static void test_Q6_finds_marvel_movie_with_Robert_Downey() {
  list_int _t1 = list_int_create(1);
  map_int_bool _t2 = map_int_bool_create(3);
  map_int_bool_put(&_t2, movie_keyword, "marvel-cinematic-universe");
  map_int_bool_put(&_t2, actor_name, "Downey Robert Jr.");
  map_int_bool_put(&_t2, marvel_movie, "Iron Man 3");
  _t1.data[0] = _t2;
  if (!((result == _t1))) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  typedef struct {
    int movie_id;
    int person_id;
  } cast_infoItem;
  typedef struct {
    int len;
    cast_infoItem *data;
  } list_cast_infoItem;
  static list_cast_infoItem list_cast_infoItem_create(int len) {
    list_cast_infoItem l;
    l.len = len;
    l.data = (cast_infoItem *)malloc(sizeof(cast_infoItem) * len);
    return l;
  }
  list_cast_infoItem _t3 = list_cast_infoItem_create(2);
  _t3.data[0] = (cast_infoItem){.movie_id = 1, .person_id = 101};
  _t3.data[1] = (cast_infoItem){.movie_id = 2, .person_id = 102};
  int cast_info = _t3;
  typedef struct {
    int id;
    char *keyword;
  } keywordItem;
  typedef struct {
    int len;
    keywordItem *data;
  } list_keywordItem;
  static list_keywordItem list_keywordItem_create(int len) {
    list_keywordItem l;
    l.len = len;
    l.data = (keywordItem *)malloc(sizeof(keywordItem) * len);
    return l;
  }
  list_keywordItem _t4 = list_keywordItem_create(2);
  _t4.data[0] =
      (keywordItem){.id = 100, .keyword = "marvel-cinematic-universe"};
  _t4.data[1] = (keywordItem){.id = 200, .keyword = "other"};
  int keyword = _t4;
  typedef struct {
    int movie_id;
    int keyword_id;
  } movie_keywordItem;
  typedef struct {
    int len;
    movie_keywordItem *data;
  } list_movie_keywordItem;
  static list_movie_keywordItem list_movie_keywordItem_create(int len) {
    list_movie_keywordItem l;
    l.len = len;
    l.data = (movie_keywordItem *)malloc(sizeof(movie_keywordItem) * len);
    return l;
  }
  list_movie_keywordItem _t5 = list_movie_keywordItem_create(2);
  _t5.data[0] = (movie_keywordItem){.movie_id = 1, .keyword_id = 100};
  _t5.data[1] = (movie_keywordItem){.movie_id = 2, .keyword_id = 200};
  int movie_keyword = _t5;
  typedef struct {
    int id;
    char *name;
  } nameItem;
  typedef struct {
    int len;
    nameItem *data;
  } list_nameItem;
  static list_nameItem list_nameItem_create(int len) {
    list_nameItem l;
    l.len = len;
    l.data = (nameItem *)malloc(sizeof(nameItem) * len);
    return l;
  }
  list_nameItem _t6 = list_nameItem_create(2);
  _t6.data[0] = (nameItem){.id = 101, .name = "Downey Robert Jr."};
  _t6.data[1] = (nameItem){.id = 102, .name = "Chris Evans"};
  int name = _t6;
  typedef struct {
    int id;
    char *title;
    int production_year;
  } titleItem;
  typedef struct {
    int len;
    titleItem *data;
  } list_titleItem;
  static list_titleItem list_titleItem_create(int len) {
    list_titleItem l;
    l.len = len;
    l.data = (titleItem *)malloc(sizeof(titleItem) * len);
    return l;
  }
  list_titleItem _t7 = list_titleItem_create(2);
  _t7.data[0] =
      (titleItem){.id = 1, .title = "Iron Man 3", .production_year = 2013};
  _t7.data[1] =
      (titleItem){.id = 2, .title = "Old Movie", .production_year = 2000};
  int title = _t7;
  list_int result = 0;
  _json_int(result);
  test_Q6_finds_marvel_movie_with_Robert_Downey();
  return 0;
}
