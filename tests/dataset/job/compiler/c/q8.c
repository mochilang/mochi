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
test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing() {
  list_int _t1 = list_int_create(1);
  map_int_bool _t2 = map_int_bool_create(2);
  map_int_bool_put(&_t2, actress_pseudonym, "Y. S.");
  map_int_bool_put(&_t2, japanese_movie_dubbed, "Dubbed Film");
  _t1.data[0] = _t2;
  if (!((result == _t1))) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  typedef struct {
    int person_id;
    char *name;
  } aka_nameItem;
  typedef struct {
    int len;
    aka_nameItem *data;
  } list_aka_nameItem;
  static list_aka_nameItem list_aka_nameItem_create(int len) {
    list_aka_nameItem l;
    l.len = len;
    l.data = (aka_nameItem *)malloc(sizeof(aka_nameItem) * len);
    return l;
  }
  list_aka_nameItem _t3 = list_aka_nameItem_create(1);
  _t3.data[0] = (aka_nameItem){.person_id = 1, .name = "Y. S."};
  int aka_name = _t3;
  typedef struct {
    int person_id;
    int movie_id;
    char *note;
    int role_id;
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
  list_cast_infoItem _t4 = list_cast_infoItem_create(1);
  _t4.data[0] = (cast_infoItem){.person_id = 1,
                                .movie_id = 10,
                                .note = "(voice: English version)",
                                .role_id = 1000};
  int cast_info = _t4;
  typedef struct {
    int id;
    char *country_code;
  } company_nameItem;
  typedef struct {
    int len;
    company_nameItem *data;
  } list_company_nameItem;
  static list_company_nameItem list_company_nameItem_create(int len) {
    list_company_nameItem l;
    l.len = len;
    l.data = (company_nameItem *)malloc(sizeof(company_nameItem) * len);
    return l;
  }
  list_company_nameItem _t5 = list_company_nameItem_create(1);
  _t5.data[0] = (company_nameItem){.id = 50, .country_code = "[jp]"};
  int company_name = _t5;
  typedef struct {
    int movie_id;
    int company_id;
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
  list_movie_companiesItem _t6 = list_movie_companiesItem_create(1);
  _t6.data[0] = (movie_companiesItem){
      .movie_id = 10, .company_id = 50, .note = "Studio (Japan)"};
  int movie_companies = _t6;
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
  list_nameItem _t7 = list_nameItem_create(2);
  _t7.data[0] = (nameItem){.id = 1, .name = "Yoko Ono"};
  _t7.data[1] = (nameItem){.id = 2, .name = "Yuichi"};
  int name = _t7;
  typedef struct {
    int id;
    char *role;
  } role_typeItem;
  typedef struct {
    int len;
    role_typeItem *data;
  } list_role_typeItem;
  static list_role_typeItem list_role_typeItem_create(int len) {
    list_role_typeItem l;
    l.len = len;
    l.data = (role_typeItem *)malloc(sizeof(role_typeItem) * len);
    return l;
  }
  list_role_typeItem _t8 = list_role_typeItem_create(1);
  _t8.data[0] = (role_typeItem){.id = 1000, .role = "actress"};
  int role_type = _t8;
  typedef struct {
    int id;
    char *title;
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
  list_titleItem _t9 = list_titleItem_create(1);
  _t9.data[0] = (titleItem){.id = 10, .title = "Dubbed Film"};
  int title = _t9;
  list_int eligible = 0;
  typedef struct {
    int actress_pseudonym;
    int japanese_movie_dubbed;
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
  list_resultItem _t10 = list_resultItem_create(1);
  list_string _t11 = list_string_create(eligible.len);
  int _t12 = 0;
  for (int _t13 = 0; _t13 < eligible.len; _t13++) {
    int x = eligible.data[_t13];
    _t11.data[_t12] = x.pseudonym;
    _t12++;
  }
  _t11.len = _t12;
  list_string _t14 = list_string_create(eligible.len);
  int _t15 = 0;
  for (int _t16 = 0; _t16 < eligible.len; _t16++) {
    int x = eligible.data[_t16];
    _t14.data[_t15] = x.movie_title;
    _t15++;
  }
  _t14.len = _t15;
  _t10.data[0] = (resultItem){.actress_pseudonym = min(_t11),
                              .japanese_movie_dubbed = min(_t14)};
  int result = _t10;
  _json_int(result);
  test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing();
  return 0;
}
