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
static void test_Q4_returns_minimum_rating_and_title_for_sequels() {
  list_int _t1 = list_int_create(1);
  map_int_bool _t2 = map_int_bool_create(2);
  map_int_bool_put(&_t2, rating, "6.2");
  map_int_bool_put(&_t2, movie_title, "Alpha Movie");
  _t1.data[0] = _t2;
  if (!((result == _t1))) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  typedef struct {
    int id;
    char *info;
  } info_typeItem;
  typedef struct {
    int len;
    info_typeItem *data;
  } list_info_typeItem;
  static list_info_typeItem list_info_typeItem_create(int len) {
    list_info_typeItem l;
    l.len = len;
    l.data = (info_typeItem *)malloc(sizeof(info_typeItem) * len);
    return l;
  }
  list_info_typeItem _t3 = list_info_typeItem_create(2);
  _t3.data[0] = (info_typeItem){.id = 1, .info = "rating"};
  _t3.data[1] = (info_typeItem){.id = 2, .info = "other"};
  int info_type = _t3;
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
  _t4.data[0] = (keywordItem){.id = 1, .keyword = "great sequel"};
  _t4.data[1] = (keywordItem){.id = 2, .keyword = "prequel"};
  int keyword = _t4;
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
  list_titleItem _t5 = list_titleItem_create(3);
  _t5.data[0] =
      (titleItem){.id = 10, .title = "Alpha Movie", .production_year = 2006};
  _t5.data[1] =
      (titleItem){.id = 20, .title = "Beta Film", .production_year = 2007};
  _t5.data[2] =
      (titleItem){.id = 30, .title = "Old Film", .production_year = 2004};
  int title = _t5;
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
  list_movie_keywordItem _t6 = list_movie_keywordItem_create(3);
  _t6.data[0] = (movie_keywordItem){.movie_id = 10, .keyword_id = 1};
  _t6.data[1] = (movie_keywordItem){.movie_id = 20, .keyword_id = 1};
  _t6.data[2] = (movie_keywordItem){.movie_id = 30, .keyword_id = 1};
  int movie_keyword = _t6;
  typedef struct {
    int movie_id;
    int info_type_id;
    char *info;
  } movie_info_idxItem;
  typedef struct {
    int len;
    movie_info_idxItem *data;
  } list_movie_info_idxItem;
  static list_movie_info_idxItem list_movie_info_idxItem_create(int len) {
    list_movie_info_idxItem l;
    l.len = len;
    l.data = (movie_info_idxItem *)malloc(sizeof(movie_info_idxItem) * len);
    return l;
  }
  list_movie_info_idxItem _t7 = list_movie_info_idxItem_create(3);
  _t7.data[0] =
      (movie_info_idxItem){.movie_id = 10, .info_type_id = 1, .info = "6.2"};
  _t7.data[1] =
      (movie_info_idxItem){.movie_id = 20, .info_type_id = 1, .info = "7.8"};
  _t7.data[2] =
      (movie_info_idxItem){.movie_id = 30, .info_type_id = 1, .info = "4.5"};
  int movie_info_idx = _t7;
  list_int rows = 0;
  typedef struct {
    int rating;
    int movie_title;
  } resultItem;
  typedef struct {
    int len;
    resultItem *data;
  } list_resultItem;
  static list_resultItem list_resultItem_create(int len) {
    list_resultItem l;
    l.len = len;
    l.data = (resultItem *)malloc(sizeof(resultItem) * len);
    return l;
  }
  list_resultItem _t8 = list_resultItem_create(1);
  list_string _t9 = list_string_create(rows.len);
  int _t10 = 0;
  for (int _t11 = 0; _t11 < rows.len; _t11++) {
    int r = rows.data[_t11];
    _t9.data[_t10] = r.rating;
    _t10++;
  }
  _t9.len = _t10;
  list_string _t12 = list_string_create(rows.len);
  int _t13 = 0;
  for (int _t14 = 0; _t14 < rows.len; _t14++) {
    int r = rows.data[_t14];
    _t12.data[_t13] = r.title;
    _t13++;
  }
  _t12.len = _t13;
  _t8.data[0] = (resultItem){.rating = min(_t9), .movie_title = min(_t12)};
  int result = _t8;
  _json_int(result);
  test_Q4_returns_minimum_rating_and_title_for_sequels();
  return 0;
}
