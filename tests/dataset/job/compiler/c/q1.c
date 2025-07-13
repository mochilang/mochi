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
test_Q1_returns_min_note__title_and_year_for_top_ranked_co_production() {
  map_int_bool _t1 = map_int_bool_create(3);
  map_int_bool_put(&_t1, production_note, "ACME (co-production)");
  map_int_bool_put(&_t1, movie_title, "Good Movie");
  map_int_bool_put(&_t1, movie_year, 1995);
  if (!((result == _t1))) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  typedef struct {
    int id;
    char *kind;
  } company_typeItem;
  typedef struct {
    int len;
    company_typeItem *data;
  } list_company_typeItem;
  static list_company_typeItem list_company_typeItem_create(int len) {
    list_company_typeItem l;
    l.len = len;
    l.data = (company_typeItem *)malloc(sizeof(company_typeItem) * len);
    return l;
  }
  list_company_typeItem _t2 = list_company_typeItem_create(2);
  _t2.data[0] = (company_typeItem){.id = 1, .kind = "production companies"};
  _t2.data[1] = (company_typeItem){.id = 2, .kind = "distributors"};
  int company_type = _t2;
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
  _t3.data[0] = (info_typeItem){.id = 10, .info = "top 250 rank"};
  _t3.data[1] = (info_typeItem){.id = 20, .info = "bottom 10 rank"};
  int info_type = _t3;
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
  list_titleItem _t4 = list_titleItem_create(2);
  _t4.data[0] =
      (titleItem){.id = 100, .title = "Good Movie", .production_year = 1995};
  _t4.data[1] =
      (titleItem){.id = 200, .title = "Bad Movie", .production_year = 2000};
  int title = _t4;
  typedef struct {
    int movie_id;
    int company_type_id;
    char *note;
  } movie_companiesItem;
  typedef struct {
    int len;
    movie_companiesItem *data;
  } list_movie_companiesItem;
  static list_movie_companiesItem list_movie_companiesItem_create(int len) {
    list_movie_companiesItem l;
    l.len = len;
    l.data = (movie_companiesItem *)malloc(sizeof(movie_companiesItem) * len);
    return l;
  }
  list_movie_companiesItem _t5 = list_movie_companiesItem_create(2);
  _t5.data[0] = (movie_companiesItem){
      .movie_id = 100, .company_type_id = 1, .note = "ACME (co-production)"};
  _t5.data[1] =
      (movie_companiesItem){.movie_id = 200,
                            .company_type_id = 1,
                            .note = "MGM (as Metro-Goldwyn-Mayer Pictures)"};
  int movie_companies = _t5;
  typedef struct {
    int movie_id;
    int info_type_id;
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
  list_movie_info_idxItem _t6 = list_movie_info_idxItem_create(2);
  _t6.data[0] = (movie_info_idxItem){.movie_id = 100, .info_type_id = 10};
  _t6.data[1] = (movie_info_idxItem){.movie_id = 200, .info_type_id = 20};
  int movie_info_idx = _t6;
  list_int filtered = 0;
  map_int_bool _t7 = map_int_bool_create(3);
  list_int _t8 = list_int_create(filtered.len);
  int _t9 = 0;
  for (int _t10 = 0; _t10 < filtered.len; _t10++) {
    int r = filtered.data[_t10];
    _t8.data[_t9] = r.note;
    _t9++;
  }
  _t8.len = _t9;
  map_int_bool_put(&_t7, production_note, min(_t8));
  list_int _t11 = list_int_create(filtered.len);
  int _t12 = 0;
  for (int _t13 = 0; _t13 < filtered.len; _t13++) {
    int r = filtered.data[_t13];
    _t11.data[_t12] = r.title;
    _t12++;
  }
  _t11.len = _t12;
  map_int_bool_put(&_t7, movie_title, min(_t11));
  list_int _t14 = list_int_create(filtered.len);
  int _t15 = 0;
  for (int _t16 = 0; _t16 < filtered.len; _t16++) {
    int r = filtered.data[_t16];
    _t14.data[_t15] = r.year;
    _t15++;
  }
  _t14.len = _t15;
  map_int_bool_put(&_t7, movie_year, min(_t14));
  int result = _t7;
  list_int _t17 = list_int_create(1);
  _t17.data[0] = result;
  _json_int(_t17);
  test_Q1_returns_min_note__title_and_year_for_top_ranked_co_production();
  return 0;
}
