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
  l.data = calloc(len, sizeof(int));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
typedef struct {
  int len;
  double *data;
} list_float;
static list_float list_float_create(int len) {
  list_float l;
  l.len = len;
  l.data = calloc(len, sizeof(double));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
typedef struct {
  int len;
  char **data;
} list_string;
static list_string list_string_create(int len) {
  list_string l;
  l.len = len;
  l.data = calloc(len, sizeof(char *));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
typedef struct {
  int len;
  list_int *data;
} list_list_int;
static list_list_int list_list_int_create(int len) {
  list_list_int l;
  l.len = len;
  l.data = calloc(len, sizeof(list_int));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static char *_min_string(list_string v) {
  if (v.len == 0)
    return "";
  char *m = v.data[0];
  for (int i = 1; i < v.len; i++)
    if (strcmp(v.data[i], m) < 0)
      m = v.data[i];
  return m;
}
static int contains_string(char *s, char *sub) {
  return strstr(s, sub) != NULL;
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
typedef struct {
  const char *of_person;
  const char *biography_movie;
} tmp1_t;
typedef struct {
  int len;
  tmp1_t *data;
} tmp1_list_t;
tmp1_list_t create_tmp1_list(int len) {
  tmp1_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(tmp1_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int person_id;
  const char *name;
} aka_name_t;
typedef struct {
  int len;
  aka_name_t *data;
} aka_name_list_t;
aka_name_list_t create_aka_name_list(int len) {
  aka_name_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(aka_name_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int person_id;
  int movie_id;
} cast_info_t;
typedef struct {
  int len;
  cast_info_t *data;
} cast_info_list_t;
cast_info_list_t create_cast_info_list(int len) {
  cast_info_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(cast_info_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  const char *info;
} info_type_t;
typedef struct {
  int len;
  info_type_t *data;
} info_type_list_t;
info_type_list_t create_info_type_list(int len) {
  info_type_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(info_type_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  const char *link;
} link_type_t;
typedef struct {
  int len;
  link_type_t *data;
} link_type_list_t;
link_type_list_t create_link_type_list(int len) {
  link_type_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(link_type_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int linked_movie_id;
  int link_type_id;
} movie_link_t;
typedef struct {
  int len;
  movie_link_t *data;
} movie_link_list_t;
movie_link_list_t create_movie_link_list(int len) {
  movie_link_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(movie_link_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  const char *name;
  const char *name_pcode_cf;
  const char *gender;
} name_t;
typedef struct {
  int len;
  name_t *data;
} name_list_t;
name_list_t create_name_list(int len) {
  name_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(name_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int person_id;
  int info_type_id;
  const char *note;
} person_info_t;
typedef struct {
  int len;
  person_info_t *data;
} person_info_list_t;
person_info_list_t create_person_info_list(int len) {
  person_info_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(person_info_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  const char *title;
  int production_year;
} title_t;
typedef struct {
  int len;
  title_t *data;
} title_list_t;
title_list_t create_title_list(int len) {
  title_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(title_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  const char *person_name;
  const char *movie_title;
} rows_item_t;
typedef struct {
  int len;
  rows_item_t *data;
} rows_item_list_t;
rows_item_list_t create_rows_item_list(int len) {
  rows_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(rows_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  const char *of_person;
  const char *biography_movie;
} result_t;
typedef struct {
  int len;
  result_t *data;
} result_list_t;
result_list_t create_result_list(int len) {
  result_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(result_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

static list_int test_Q7_finds_movie_features_biography_for_person_result;
static void test_Q7_finds_movie_features_biography_for_person() {
  tmp1_t tmp1[] = {
      (tmp1_t){.of_person = "Alan Brown", .biography_movie = "Feature Film"}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_Q7_finds_movie_features_biography_for_person_result.len !=
      tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0;
         i3 < test_Q7_finds_movie_features_biography_for_person_result.len;
         i3++) {
      if (test_Q7_finds_movie_features_biography_for_person_result.data[i3] !=
          tmp1.data[i3]) {
        tmp2 = 0;
        break;
      }
    }
  }
  if (!(tmp2)) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  aka_name_t aka_name[] = {(aka_name_t){.person_id = 1, .name = "Anna Mae"},
                           (aka_name_t){.person_id = 2, .name = "Chris"}};
  int aka_name_len = sizeof(aka_name) / sizeof(aka_name[0]);
  cast_info_t cast_info[] = {(cast_info_t){.person_id = 1, .movie_id = 10},
                             (cast_info_t){.person_id = 2, .movie_id = 20}};
  int cast_info_len = sizeof(cast_info) / sizeof(cast_info[0]);
  info_type_t info_type[] = {(info_type_t){.id = 1, .info = "mini biography"},
                             (info_type_t){.id = 2, .info = "trivia"}};
  int info_type_len = sizeof(info_type) / sizeof(info_type[0]);
  link_type_t link_type[] = {(link_type_t){.id = 1, .link = "features"},
                             (link_type_t){.id = 2, .link = "references"}};
  int link_type_len = sizeof(link_type) / sizeof(link_type[0]);
  movie_link_t movie_link[] = {
      (movie_link_t){.linked_movie_id = 10, .link_type_id = 1},
      (movie_link_t){.linked_movie_id = 20, .link_type_id = 2}};
  int movie_link_len = sizeof(movie_link) / sizeof(movie_link[0]);
  name_t name[] = {
      (name_t){
          .id = 1, .name = "Alan Brown", .name_pcode_cf = "B", .gender = "m"},
      (name_t){.id = 2, .name = "Zoe", .name_pcode_cf = "Z", .gender = "f"}};
  int name_len = sizeof(name) / sizeof(name[0]);
  person_info_t person_info[] = {
      (person_info_t){
          .person_id = 1, .info_type_id = 1, .note = "Volker Boehm"},
      (person_info_t){.person_id = 2, .info_type_id = 1, .note = "Other"}};
  int person_info_len = sizeof(person_info) / sizeof(person_info[0]);
  title_t title[] = {
      (title_t){.id = 10, .title = "Feature Film", .production_year = 1990},
      (title_t){.id = 20, .title = "Late Film", .production_year = 2000}};
  int title_len = sizeof(title) / sizeof(title[0]);
  rows_item_list_t tmp4 = rows_item_list_t_create(
      aka_name.len * name.len * person_info.len * info_type.len *
      cast_info.len * title.len * movie_link.len * link_type.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < aka_name_len; tmp6++) {
    aka_name_t an = aka_name[tmp6];
    for (int tmp7 = 0; tmp7 < name_len; tmp7++) {
      name_t n = name[tmp7];
      if (!(n.id == an.person_id)) {
        continue;
      }
      for (int tmp8 = 0; tmp8 < person_info_len; tmp8++) {
        person_info_t pi = person_info[tmp8];
        if (!(pi.person_id == an.person_id)) {
          continue;
        }
        for (int tmp9 = 0; tmp9 < info_type_len; tmp9++) {
          info_type_t it = info_type[tmp9];
          if (!(it.id == pi.info_type_id)) {
            continue;
          }
          for (int tmp10 = 0; tmp10 < cast_info_len; tmp10++) {
            cast_info_t ci = cast_info[tmp10];
            if (!(ci.person_id == n.id)) {
              continue;
            }
            for (int tmp11 = 0; tmp11 < title_len; tmp11++) {
              title_t t = title[tmp11];
              if (!(t.id == ci.movie_id)) {
                continue;
              }
              for (int tmp12 = 0; tmp12 < movie_link_len; tmp12++) {
                movie_link_t ml = movie_link[tmp12];
                if (!(ml.linked_movie_id == t.id)) {
                  continue;
                }
                for (int tmp13 = 0; tmp13 < link_type_len; tmp13++) {
                  link_type_t lt = link_type[tmp13];
                  if (!(lt.id == ml.link_type_id)) {
                    continue;
                  }
                  if (!((contains_string(an.name, "a") &&
                         it.info == "mini biography" && lt.link == "features" &&
                         n.name_pcode_cf >= "A" && n.name_pcode_cf <= "F" &&
                         ((strcmp(n.gender, "m") == 0) ||
                          ((strcmp(n.gender, "f") == 0) &&
                           n.name.starts_with("B"))) &&
                         pi.note == "Volker Boehm" &&
                         t.production_year >= 1980 &&
                         t.production_year <= 1995 &&
                         pi.person_id == an.person_id &&
                         pi.person_id == ci.person_id &&
                         an.person_id == ci.person_id &&
                         ci.movie_id == ml.linked_movie_id))) {
                    continue;
                  }
                  tmp4.data[tmp5] = (rows_item_t){.person_name = n.name,
                                                  .movie_title = t.title};
                  tmp5++;
                }
              }
            }
          }
        }
      }
    }
  }
  tmp4.len = tmp5;
  rows_item_list_t rows = tmp4;
  int tmp14 = int_create(rows.len);
  int tmp15 = 0;
  for (int tmp16 = 0; tmp16 < rows.len; tmp16++) {
    rows_item_t r = rows.data[tmp16];
    tmp14.data[tmp15] = r.person_name;
    tmp15++;
  }
  tmp14.len = tmp15;
  int tmp17 = int_create(rows.len);
  int tmp18 = 0;
  for (int tmp19 = 0; tmp19 < rows.len; tmp19++) {
    rows_item_t r = rows.data[tmp19];
    tmp17.data[tmp18] = r.movie_title;
    tmp18++;
  }
  tmp17.len = tmp18;
  result_t result[] = {(result_t){.of_person = _min_string(tmp14),
                                  .biography_movie = _min_string(tmp17)}};
  int result_len = sizeof(result) / sizeof(result[0]);
  printf("[");
  for (int i20 = 0; i20 < result_len; i20++) {
    if (i20 > 0)
      printf(",");
    result_t it = result[i20];
    printf("{");
    _json_string("of_person");
    printf(":");
    _json_string(it.of_person);
    printf(",");
    _json_string("biography_movie");
    printf(":");
    _json_string(it.biography_movie);
    printf("}");
  }
  printf("]");
  test_Q7_finds_movie_features_biography_for_person_result = result;
  test_Q7_finds_movie_features_biography_for_person();
  free(tmp14.data);
  free(tmp17.data);
  return 0;
}
