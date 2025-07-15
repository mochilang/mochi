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
static int _min_int(list_int v) {
  if (v.len == 0)
    return 0;
  int m = v.data[0];
  for (int i = 1; i < v.len; i++)
    if (v.data[i] < m)
      m = v.data[i];
  return m;
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
static int contains_list_string(list_string v, char *item) {
  for (int i = 0; i < v.len; i++)
    if (strcmp(v.data[i], item) == 0)
      return 1;
  return 0;
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
  int movie_budget;
  int movie_votes;
  const char *movie_title;
} tmp_item_t;
typedef struct {
  int len;
  tmp_item_t *data;
} tmp_item_list_t;
tmp_item_list_t create_tmp_item_list(int len) {
  tmp_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(tmp_item_t));
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
  const char *name;
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
  int id;
  const char *title;
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
  int movie_id;
  int person_id;
  const char *note;
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
  int movie_id;
  int info_type_id;
  int info;
} movie_info_t;
typedef struct {
  int len;
  movie_info_t *data;
} movie_info_list_t;
movie_info_list_t create_movie_info_list(int len) {
  movie_info_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(movie_info_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int movie_id;
  int info_type_id;
  int info;
} movie_info_idx_t;
typedef struct {
  int len;
  movie_info_idx_t *data;
} movie_info_idx_list_t;
movie_info_idx_list_t create_movie_info_idx_list(int len) {
  movie_info_idx_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(movie_info_idx_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int budget;
  int votes;
  const char *title;
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
  int movie_budget;
  int movie_votes;
  int movie_title;
} result_item_t;
typedef struct {
  int len;
  result_item_t *data;
} result_item_list_t;
result_item_list_t create_result_item_list(int len) {
  result_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(result_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

static int
    test_Q18_finds_minimal_budget__votes_and_title_for_Tim_productions_result;
static void
test_Q18_finds_minimal_budget__votes_and_title_for_Tim_productions() {
  if (!(test_Q18_finds_minimal_budget__votes_and_title_for_Tim_productions_result ==
        (tmp_item_t){
            .movie_budget = 90, .movie_votes = 400, .movie_title = "Alpha"})) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  info_type_t info_type[] = {(info_type_t){.id = 1, .info = "budget"},
                             (info_type_t){.id = 2, .info = "votes"},
                             (info_type_t){.id = 3, .info = "rating"}};
  int info_type_len = sizeof(info_type) / sizeof(info_type[0]);
  name_t name[] = {(name_t){.id = 1, .name = "Big Tim", .gender = "m"},
                   (name_t){.id = 2, .name = "Slim Tim", .gender = "m"},
                   (name_t){.id = 3, .name = "Alice", .gender = "f"}};
  int name_len = sizeof(name) / sizeof(name[0]);
  title_t title[] = {(title_t){.id = 10, .title = "Alpha"},
                     (title_t){.id = 20, .title = "Beta"},
                     (title_t){.id = 30, .title = "Gamma"}};
  int title_len = sizeof(title) / sizeof(title[0]);
  cast_info_t cast_info[] = {
      (cast_info_t){.movie_id = 10, .person_id = 1, .note = "(producer)"},
      (cast_info_t){
          .movie_id = 20, .person_id = 2, .note = "(executive producer)"},
      (cast_info_t){.movie_id = 30, .person_id = 3, .note = "(producer)"}};
  int cast_info_len = sizeof(cast_info) / sizeof(cast_info[0]);
  movie_info_t movie_info[] = {
      (movie_info_t){.movie_id = 10, .info_type_id = 1, .info = 90},
      (movie_info_t){.movie_id = 20, .info_type_id = 1, .info = 120},
      (movie_info_t){.movie_id = 30, .info_type_id = 1, .info = 110}};
  int movie_info_len = sizeof(movie_info) / sizeof(movie_info[0]);
  movie_info_idx_t movie_info_idx[] = {
      (movie_info_idx_t){.movie_id = 10, .info_type_id = 2, .info = 500},
      (movie_info_idx_t){.movie_id = 20, .info_type_id = 2, .info = 400},
      (movie_info_idx_t){.movie_id = 30, .info_type_id = 2, .info = 800}};
  int movie_info_idx_len = sizeof(movie_info_idx) / sizeof(movie_info_idx[0]);
  list_string rows = list_string_create(2);
  rows.data[0] = "(producer)";
  rows.data[1] = "(executive producer)";
  rows_item_list_t tmp1 = rows_item_list_t_create(
      cast_info.len * name.len * title.len * movie_info.len *
      movie_info_idx.len * info_type.len * info_type.len);
  int tmp2 = 0;
  for (int tmp3 = 0; tmp3 < cast_info_len; tmp3++) {
    cast_info_t ci = cast_info[tmp3];
    for (int tmp4 = 0; tmp4 < name_len; tmp4++) {
      name_t n = name[tmp4];
      if (!(n.id == ci.person_id)) {
        continue;
      }
      for (int tmp5 = 0; tmp5 < title_len; tmp5++) {
        title_t t = title[tmp5];
        if (!(t.id == ci.movie_id)) {
          continue;
        }
        for (int tmp6 = 0; tmp6 < movie_info_len; tmp6++) {
          movie_info_t mi = movie_info[tmp6];
          if (!(mi.movie_id == t.id)) {
            continue;
          }
          for (int tmp7 = 0; tmp7 < movie_info_idx_len; tmp7++) {
            movie_info_idx_t mi_idx = movie_info_idx[tmp7];
            if (!(mi_idx.movie_id == t.id)) {
              continue;
            }
            for (int tmp8 = 0; tmp8 < info_type_len; tmp8++) {
              info_type_t it1 = info_type[tmp8];
              if (!(it1.id == mi.info_type_id)) {
                continue;
              }
              for (int tmp9 = 0; tmp9 < info_type_len; tmp9++) {
                info_type_t it2 = info_type[tmp9];
                if (!(it2.id == mi_idx.info_type_id)) {
                  continue;
                }
                if (!((contains_list_string(rows, ci.note) &&
                       it1.info == "budget" && it2.info == "votes" &&
                       n.gender == "m" && contains_string(n.name, "Tim") &&
                       t.id == ci.movie_id && ci.movie_id == mi.movie_id &&
                       ci.movie_id == mi_idx.movie_id &&
                       mi.movie_id == mi_idx.movie_id))) {
                  continue;
                }
                tmp1.data[tmp2] = (rows_item_t){
                    .budget = mi.info, .votes = mi_idx.info, .title = t.title};
                tmp2++;
              }
            }
          }
        }
      }
    }
  }
  tmp1.len = tmp2;
  rows_item_list_t rows = tmp1;
  list_int tmp10 = list_int_create(rows.len);
  int tmp11 = 0;
  for (int tmp12 = 0; tmp12 < rows.len; tmp12++) {
    rows_item_t r = rows.data[tmp12];
    tmp10.data[tmp11] = r.budget;
    tmp11++;
  }
  tmp10.len = tmp11;
  list_int tmp13 = list_int_create(rows.len);
  int tmp14 = 0;
  for (int tmp15 = 0; tmp15 < rows.len; tmp15++) {
    rows_item_t r = rows.data[tmp15];
    tmp13.data[tmp14] = r.votes;
    tmp14++;
  }
  tmp13.len = tmp14;
  int tmp16 = int_create(rows.len);
  int tmp17 = 0;
  for (int tmp18 = 0; tmp18 < rows.len; tmp18++) {
    rows_item_t r = rows.data[tmp18];
    tmp16.data[tmp17] = r.title;
    tmp17++;
  }
  tmp16.len = tmp17;
  result_item_t result = (result_item_t){.movie_budget = _min_int(tmp10),
                                         .movie_votes = _min_int(tmp13),
                                         .movie_title = _min_string(tmp16)};
  printf("{");
  _json_string("movie_budget");
  printf(":");
  _json_int(result.movie_budget);
  printf(",");
  _json_string("movie_votes");
  printf(":");
  _json_int(result.movie_votes);
  printf(",");
  _json_string("movie_title");
  printf(":");
  _json_int(result.movie_title);
  printf("}");
  test_Q18_finds_minimal_budget__votes_and_title_for_Tim_productions_result =
      result;
  test_Q18_finds_minimal_budget__votes_and_title_for_Tim_productions();
  free(tmp10.data);
  free(tmp13.data);
  free(tmp16.data);
  return 0;
}
