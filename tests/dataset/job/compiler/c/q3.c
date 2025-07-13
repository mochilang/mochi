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
static void test_Q3_returns_lexicographically_smallest_sequel_title() {
  list_int _t1 = list_int_create(1);
  map_int_bool _t2 = map_int_bool_create(1);
  map_int_bool_put(&_t2, movie_title, "Alpha");
  _t1.data[0] = _t2;
  if (!((result == _t1))) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
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
  list_keywordItem _t3 = list_keywordItem_create(2);
  _t3.data[0] = (keywordItem){.id = 1, .keyword = "amazing sequel"};
  _t3.data[1] = (keywordItem){.id = 2, .keyword = "prequel"};
  int keyword = _t3;
  typedef struct {
    int movie_id;
    char *info;
  } movie_infoItem;
  typedef struct {
    int len;
    movie_infoItem *data;
  } list_movie_infoItem;
  static list_movie_infoItem list_movie_infoItem_create(int len) {
    list_movie_infoItem l;
    l.len = len;
    l.data = (movie_infoItem *)malloc(sizeof(movie_infoItem) * len);
    return l;
  }
  list_movie_infoItem _t4 = list_movie_infoItem_create(3);
  _t4.data[0] = (movie_infoItem){.movie_id = 10, .info = "Germany"};
  _t4.data[1] = (movie_infoItem){.movie_id = 30, .info = "Sweden"};
  _t4.data[2] = (movie_infoItem){.movie_id = 20, .info = "France"};
  int movie_info = _t4;
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
  list_movie_keywordItem _t5 = list_movie_keywordItem_create(4);
  _t5.data[0] = (movie_keywordItem){.movie_id = 10, .keyword_id = 1};
  _t5.data[1] = (movie_keywordItem){.movie_id = 30, .keyword_id = 1};
  _t5.data[2] = (movie_keywordItem){.movie_id = 20, .keyword_id = 1};
  _t5.data[3] = (movie_keywordItem){.movie_id = 10, .keyword_id = 2};
  int movie_keyword = _t5;
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
  list_titleItem _t6 = list_titleItem_create(3);
  _t6.data[0] =
      (titleItem){.id = 10, .title = "Alpha", .production_year = 2006};
  _t6.data[1] = (titleItem){.id = 30, .title = "Beta", .production_year = 2008};
  _t6.data[2] =
      (titleItem){.id = 20, .title = "Gamma", .production_year = 2009};
  int title = _t6;
  list_string _t7 = list_string_create(8);
  _t7.data[0] = "Sweden";
  _t7.data[1] = "Norway";
  _t7.data[2] = "Germany";
  _t7.data[3] = "Denmark";
  _t7.data[4] = "Swedish";
  _t7.data[5] = "Denish";
  _t7.data[6] = "Norwegian";
  _t7.data[7] = "German";
  list_string allowed_infos = _t7;
  list_string candidate_titles = 0;
  typedef struct {
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
  _t8.data[0] = (resultItem){.movie_title = min(candidate_titles)};
  int result = _t8;
  _json_int(result);
  test_Q3_returns_lexicographically_smallest_sequel_title();
  return 0;
}
