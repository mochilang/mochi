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
static void test_Q10_finds_uncredited_voice_actor_in_Russian_movie() {
  list_int _t1 = list_int_create(1);
  map_int_bool _t2 = map_int_bool_create(2);
  map_int_bool_put(&_t2, uncredited_voiced_character, "Ivan");
  map_int_bool_put(&_t2, russian_movie, "Vodka Dreams");
  _t1.data[0] = _t2;
  if (!((result == _t1))) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  typedef struct {
    int id;
    char *name;
  } char_nameItem;
  typedef struct {
    int len;
    char_nameItem *data;
  } list_char_nameItem;
  static list_char_nameItem list_char_nameItem_create(int len) {
    list_char_nameItem l;
    l.len = len;
    l.data = (char_nameItem *)malloc(sizeof(char_nameItem) * len);
    return l;
  }
  list_char_nameItem _t3 = list_char_nameItem_create(2);
  _t3.data[0] = (char_nameItem){.id = 1, .name = "Ivan"};
  _t3.data[1] = (char_nameItem){.id = 2, .name = "Alex"};
  int char_name = _t3;
  typedef struct {
    int movie_id;
    int person_role_id;
    int role_id;
    char *note;
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
  list_cast_infoItem _t4 = list_cast_infoItem_create(2);
  _t4.data[0] = (cast_infoItem){.movie_id = 10,
                                .person_role_id = 1,
                                .role_id = 1,
                                .note = "Soldier (voice) (uncredited)"};
  _t4.data[1] = (cast_infoItem){
      .movie_id = 11, .person_role_id = 2, .role_id = 1, .note = "(voice)"};
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
  list_company_nameItem _t5 = list_company_nameItem_create(2);
  _t5.data[0] = (company_nameItem){.id = 1, .country_code = "[ru]"};
  _t5.data[1] = (company_nameItem){.id = 2, .country_code = "[us]"};
  int company_name = _t5;
  typedef struct {
    int id;
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
  list_company_typeItem _t6 = list_company_typeItem_create(2);
  _t6.data[0] = (company_typeItem){.id = 1};
  _t6.data[1] = (company_typeItem){.id = 2};
  int company_type = _t6;
  typedef struct {
    int movie_id;
    int company_id;
    int company_type_id;
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
  list_movie_companiesItem _t7 = list_movie_companiesItem_create(2);
  _t7.data[0] = (movie_companiesItem){
      .movie_id = 10, .company_id = 1, .company_type_id = 1};
  _t7.data[1] = (movie_companiesItem){
      .movie_id = 11, .company_id = 2, .company_type_id = 1};
  int movie_companies = _t7;
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
  list_role_typeItem _t8 = list_role_typeItem_create(2);
  _t8.data[0] = (role_typeItem){.id = 1, .role = "actor"};
  _t8.data[1] = (role_typeItem){.id = 2, .role = "director"};
  int role_type = _t8;
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
  list_titleItem _t9 = list_titleItem_create(2);
  _t9.data[0] =
      (titleItem){.id = 10, .title = "Vodka Dreams", .production_year = 2006};
  _t9.data[1] =
      (titleItem){.id = 11, .title = "Other Film", .production_year = 2004};
  int title = _t9;
  list_int matches = 0;
  typedef struct {
    int uncredited_voiced_character;
    int russian_movie;
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
  list_string _t11 = list_string_create(matches.len);
  int _t12 = 0;
  for (int _t13 = 0; _t13 < matches.len; _t13++) {
    int x = matches.data[_t13];
    _t11.data[_t12] = x.character;
    _t12++;
  }
  _t11.len = _t12;
  list_string _t14 = list_string_create(matches.len);
  int _t15 = 0;
  for (int _t16 = 0; _t16 < matches.len; _t16++) {
    int x = matches.data[_t16];
    _t14.data[_t15] = x.movie;
    _t15++;
  }
  _t14.len = _t15;
  _t10.data[0] = (resultItem){.uncredited_voiced_character = min(_t11),
                              .russian_movie = min(_t14)};
  int result = _t10;
  _json_int(result);
  test_Q10_finds_uncredited_voice_actor_in_Russian_movie();
  return 0;
}
