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
static void test_Q5_finds_the_lexicographically_first_qualifying_title() {
  list_int _t1 = list_int_create(1);
  map_int_bool _t2 = map_int_bool_create(1);
  map_int_bool_put(&_t2, typical_european_movie, "A Film");
  _t1.data[0] = _t2;
  if (!((result == _t1))) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  typedef struct {
    int ct_id;
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
  list_company_typeItem _t3 = list_company_typeItem_create(2);
  _t3.data[0] = (company_typeItem){.ct_id = 1, .kind = "production companies"};
  _t3.data[1] = (company_typeItem){.ct_id = 2, .kind = "other"};
  int company_type = _t3;
  typedef struct {
    int it_id;
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
  list_info_typeItem _t4 = list_info_typeItem_create(1);
  _t4.data[0] = (info_typeItem){.it_id = 10, .info = "languages"};
  int info_type = _t4;
  typedef struct {
    int t_id;
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
      (titleItem){.t_id = 100, .title = "B Movie", .production_year = 2010};
  _t5.data[1] =
      (titleItem){.t_id = 200, .title = "A Film", .production_year = 2012};
  _t5.data[2] =
      (titleItem){.t_id = 300, .title = "Old Movie", .production_year = 2000};
  int title = _t5;
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
  list_movie_companiesItem _t6 = list_movie_companiesItem_create(3);
  _t6.data[0] = (movie_companiesItem){.movie_id = 100,
                                      .company_type_id = 1,
                                      .note = "ACME (France) (theatrical)"};
  _t6.data[1] = (movie_companiesItem){.movie_id = 200,
                                      .company_type_id = 1,
                                      .note = "ACME (France) (theatrical)"};
  _t6.data[2] = (movie_companiesItem){.movie_id = 300,
                                      .company_type_id = 1,
                                      .note = "ACME (France) (theatrical)"};
  int movie_companies = _t6;
  typedef struct {
    int movie_id;
    char *info;
    int info_type_id;
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
  list_movie_infoItem _t7 = list_movie_infoItem_create(3);
  _t7.data[0] =
      (movie_infoItem){.movie_id = 100, .info = "German", .info_type_id = 10};
  _t7.data[1] =
      (movie_infoItem){.movie_id = 200, .info = "Swedish", .info_type_id = 10};
  _t7.data[2] =
      (movie_infoItem){.movie_id = 300, .info = "German", .info_type_id = 10};
  int movie_info = _t7;
  list_string candidate_titles = 0;
  typedef struct {
    int typical_european_movie;
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
  _t8.data[0] = (resultItem){.typical_european_movie = min(candidate_titles)};
  int result = _t8;
  _json_int(result);
  test_Q5_finds_the_lexicographically_first_qualifying_title();
  return 0;
}
