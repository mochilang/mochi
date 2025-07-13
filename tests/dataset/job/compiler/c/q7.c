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
static void test_Q7_finds_movie_features_biography_for_person() {
  list_int _t1 = list_int_create(1);
  map_int_bool _t2 = map_int_bool_create(2);
  map_int_bool_put(&_t2, of_person, "Alan Brown");
  map_int_bool_put(&_t2, biography_movie, "Feature Film");
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
  list_aka_nameItem _t3 = list_aka_nameItem_create(2);
  _t3.data[0] = (aka_nameItem){.person_id = 1, .name = "Anna Mae"};
  _t3.data[1] = (aka_nameItem){.person_id = 2, .name = "Chris"};
  int aka_name = _t3;
  typedef struct {
    int person_id;
    int movie_id;
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
  _t4.data[0] = (cast_infoItem){.person_id = 1, .movie_id = 10};
  _t4.data[1] = (cast_infoItem){.person_id = 2, .movie_id = 20};
  int cast_info = _t4;
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
  list_info_typeItem _t5 = list_info_typeItem_create(2);
  _t5.data[0] = (info_typeItem){.id = 1, .info = "mini biography"};
  _t5.data[1] = (info_typeItem){.id = 2, .info = "trivia"};
  int info_type = _t5;
  typedef struct {
    int id;
    char *link;
  } link_typeItem;
  typedef struct {
    int len;
    link_typeItem *data;
  } list_link_typeItem;
  static list_link_typeItem list_link_typeItem_create(int len) {
    list_link_typeItem l;
    l.len = len;
    l.data = (link_typeItem *)malloc(sizeof(link_typeItem) * len);
    return l;
  }
  list_link_typeItem _t6 = list_link_typeItem_create(2);
  _t6.data[0] = (link_typeItem){.id = 1, .link = "features"};
  _t6.data[1] = (link_typeItem){.id = 2, .link = "references"};
  int link_type = _t6;
  typedef struct {
    int linked_movie_id;
    int link_type_id;
  } movie_linkItem;
  typedef struct {
    int len;
    movie_linkItem *data;
  } list_movie_linkItem;
  static list_movie_linkItem list_movie_linkItem_create(int len) {
    list_movie_linkItem l;
    l.len = len;
    l.data = (movie_linkItem *)malloc(sizeof(movie_linkItem) * len);
    return l;
  }
  list_movie_linkItem _t7 = list_movie_linkItem_create(2);
  _t7.data[0] = (movie_linkItem){.linked_movie_id = 10, .link_type_id = 1};
  _t7.data[1] = (movie_linkItem){.linked_movie_id = 20, .link_type_id = 2};
  int movie_link = _t7;
  typedef struct {
    int id;
    char *name;
    char *name_pcode_cf;
    char *gender;
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
  list_nameItem _t8 = list_nameItem_create(2);
  _t8.data[0] = (nameItem){
      .id = 1, .name = "Alan Brown", .name_pcode_cf = "B", .gender = "m"};
  _t8.data[1] =
      (nameItem){.id = 2, .name = "Zoe", .name_pcode_cf = "Z", .gender = "f"};
  int name = _t8;
  typedef struct {
    int person_id;
    int info_type_id;
    char *note;
  } person_infoItem;
  typedef struct {
    int len;
    person_infoItem *data;
  } list_person_infoItem;
  static list_person_infoItem list_person_infoItem_create(int len) {
    list_person_infoItem l;
    l.len = len;
    l.data = (person_infoItem *)malloc(sizeof(person_infoItem) * len);
    return l;
  }
  list_person_infoItem _t9 = list_person_infoItem_create(2);
  _t9.data[0] = (person_infoItem){
      .person_id = 1, .info_type_id = 1, .note = "Volker Boehm"};
  _t9.data[1] =
      (person_infoItem){.person_id = 2, .info_type_id = 1, .note = "Other"};
  int person_info = _t9;
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
  list_titleItem _t10 = list_titleItem_create(2);
  _t10.data[0] =
      (titleItem){.id = 10, .title = "Feature Film", .production_year = 1990};
  _t10.data[1] =
      (titleItem){.id = 20, .title = "Late Film", .production_year = 2000};
  int title = _t10;
  list_int rows = 0;
  typedef struct {
    int of_person;
    int biography_movie;
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
  list_resultItem _t11 = list_resultItem_create(1);
  list_string _t12 = list_string_create(rows.len);
  int _t13 = 0;
  for (int _t14 = 0; _t14 < rows.len; _t14++) {
    int r = rows.data[_t14];
    _t12.data[_t13] = r.person_name;
    _t13++;
  }
  _t12.len = _t13;
  list_string _t15 = list_string_create(rows.len);
  int _t16 = 0;
  for (int _t17 = 0; _t17 < rows.len; _t17++) {
    int r = rows.data[_t17];
    _t15.data[_t16] = r.movie_title;
    _t16++;
  }
  _t15.len = _t16;
  _t11.data[0] =
      (resultItem){.of_person = min(_t12), .biography_movie = min(_t15)};
  int result = _t11;
  _json_int(result);
  test_Q7_finds_movie_features_biography_for_person();
  return 0;
}
