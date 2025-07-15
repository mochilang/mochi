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
static double _min_float(list_float v) {
  if (v.len == 0)
    return 0;
  double m = v.data[0];
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
  double rating;
  const char *northern_dark_movie;
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
  const char *keyword;
} keyword_t;
typedef struct {
  int len;
  keyword_t *data;
} keyword_list_t;
keyword_list_t create_keyword_list(int len) {
  keyword_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(keyword_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  const char *kind;
} kind_type_t;
typedef struct {
  int len;
  kind_type_t *data;
} kind_type_list_t;
kind_type_list_t create_kind_type_list(int len) {
  kind_type_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(kind_type_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  int kind_id;
  int production_year;
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
  int info_type_id;
  const char *info;
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
  double info;
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
  int movie_id;
  int keyword_id;
} movie_keyword_t;
typedef struct {
  int len;
  movie_keyword_t *data;
} movie_keyword_list_t;
movie_keyword_list_t create_movie_keyword_list(int len) {
  movie_keyword_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(movie_keyword_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  double rating;
  const char *title;
} matches_item_t;
typedef struct {
  int len;
  matches_item_t *data;
} matches_item_list_t;
matches_item_list_t create_matches_item_list(int len) {
  matches_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(matches_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int rating;
  int northern_dark_movie;
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

static int test_Q14_selects_minimal_rating_and_title_for_dark_movies_result;
static void test_Q14_selects_minimal_rating_and_title_for_dark_movies() {
  if (!(test_Q14_selects_minimal_rating_and_title_for_dark_movies_result ==
        (tmp_item_t){.rating = 7.0, .northern_dark_movie = "A Dark Movie"})) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  info_type_t info_type[] = {(info_type_t){.id = 1, .info = "countries"},
                             (info_type_t){.id = 2, .info = "rating"}};
  int info_type_len = sizeof(info_type) / sizeof(info_type[0]);
  keyword_t keyword[] = {(keyword_t){.id = 1, .keyword = "murder"},
                         (keyword_t){.id = 2, .keyword = "blood"},
                         (keyword_t){.id = 3, .keyword = "romance"}};
  int keyword_len = sizeof(keyword) / sizeof(keyword[0]);
  kind_type_t kind_type[] = {(kind_type_t){.id = 1, .kind = "movie"}};
  int kind_type_len = sizeof(kind_type) / sizeof(kind_type[0]);
  title_t title[] = {
      (title_t){.id = 1,
                .kind_id = 1,
                .production_year = 2012,
                .title = "A Dark Movie"},
      (title_t){.id = 2,
                .kind_id = 1,
                .production_year = 2013,
                .title = "Brutal Blood"},
      (title_t){
          .id = 3, .kind_id = 1, .production_year = 2008, .title = "Old Film"}};
  int title_len = sizeof(title) / sizeof(title[0]);
  movie_info_t movie_info[] = {
      (movie_info_t){.movie_id = 1, .info_type_id = 1, .info = "Sweden"},
      (movie_info_t){.movie_id = 2, .info_type_id = 1, .info = "USA"},
      (movie_info_t){.movie_id = 3, .info_type_id = 1, .info = "USA"}};
  int movie_info_len = sizeof(movie_info) / sizeof(movie_info[0]);
  movie_info_idx_t movie_info_idx[] = {
      (movie_info_idx_t){.movie_id = 1, .info_type_id = 2, .info = 7.0},
      (movie_info_idx_t){.movie_id = 2, .info_type_id = 2, .info = 7.5},
      (movie_info_idx_t){.movie_id = 3, .info_type_id = 2, .info = 9.1}};
  int movie_info_idx_len = sizeof(movie_info_idx) / sizeof(movie_info_idx[0]);
  movie_keyword_t movie_keyword[] = {
      (movie_keyword_t){.movie_id = 1, .keyword_id = 1},
      (movie_keyword_t){.movie_id = 2, .keyword_id = 2},
      (movie_keyword_t){.movie_id = 3, .keyword_id = 3}};
  int movie_keyword_len = sizeof(movie_keyword) / sizeof(movie_keyword[0]);
  list_string allowed_keywords = list_string_create(4);
  allowed_keywords.data[0] = "murder";
  allowed_keywords.data[1] = "murder-in-title";
  allowed_keywords.data[2] = "blood";
  allowed_keywords.data[3] = "violence";
  int allowed_keywords = allowed_keywords;
  list_string allowed_countries = list_string_create(10);
  allowed_countries.data[0] = "Sweden";
  allowed_countries.data[1] = "Norway";
  allowed_countries.data[2] = "Germany";
  allowed_countries.data[3] = "Denmark";
  allowed_countries.data[4] = "Swedish";
  allowed_countries.data[5] = "Denish";
  allowed_countries.data[6] = "Norwegian";
  allowed_countries.data[7] = "German";
  allowed_countries.data[8] = "USA";
  allowed_countries.data[9] = "American";
  int allowed_countries = allowed_countries;
  matches_item_list_t tmp1 = create_matches_item_list(
      info_type_len * info_type_len * keyword_len * kind_type_len *
      movie_info_len * movie_info_idx_len * movie_keyword_len * title_len);
  int tmp2 = 0;
  for (int it1_idx = 0; it1_idx < info_type_len; it1_idx++) {
    info_type_t it1 = info_type[it1_idx];
    for (int it2_idx = 0; it2_idx < info_type_len; it2_idx++) {
      info_type_t it2 = info_type[it2_idx];
      for (int k_idx = 0; k_idx < keyword_len; k_idx++) {
        keyword_t k = keyword[k_idx];
        for (int kt_idx = 0; kt_idx < kind_type_len; kt_idx++) {
          kind_type_t kt = kind_type[kt_idx];
          for (int mi_idx = 0; mi_idx < movie_info_len; mi_idx++) {
            movie_info_t mi = movie_info[mi_idx];
            for (int mi_idx_idx = 0; mi_idx_idx < movie_info_idx_len;
                 mi_idx_idx++) {
              movie_info_idx_t mi_idx = movie_info_idx[mi_idx_idx];
              for (int mk_idx = 0; mk_idx < movie_keyword_len; mk_idx++) {
                movie_keyword_t mk = movie_keyword[mk_idx];
                for (int t_idx = 0; t_idx < title_len; t_idx++) {
                  title_t t = title[t_idx];
                  if (!(((strcmp(it1.info, "countries") == 0) &&
                         it2.info == "rating" &&
                         (contains_list_string(allowed_keywords, k.keyword)) &&
                         kt.kind == "movie" &&
                         (contains_list_string(allowed_countries, mi.info)) &&
                         mi_idx.info < 8.5 && t.production_year > 2010 &&
                         kt.id == t.kind_id && t.id == mi.movie_id &&
                         t.id == mk.movie_id && t.id == mi_idx.movie_id &&
                         mk.movie_id == mi.movie_id &&
                         mk.movie_id == mi_idx.movie_id &&
                         mi.movie_id == mi_idx.movie_id &&
                         k.id == mk.keyword_id && it1.id == mi.info_type_id &&
                         it2.id == mi_idx.info_type_id))) {
                    continue;
                  }
                  tmp1.data[tmp2] =
                      (matches_item_t){.rating = mi_idx.info, .title = t.title};
                  tmp2++;
                }
              }
            }
          }
        }
      }
    }
  }
  tmp1.len = tmp2;
  matches_item_list_t matches = tmp1;
  list_float tmp3 = list_float_create(matches.len);
  int tmp4 = 0;
  for (int tmp5 = 0; tmp5 < matches.len; tmp5++) {
    matches_item_t x = matches.data[tmp5];
    tmp3.data[tmp4] = x.rating;
    tmp4++;
  }
  tmp3.len = tmp4;
  int tmp6 = int_create(matches.len);
  int tmp7 = 0;
  for (int tmp8 = 0; tmp8 < matches.len; tmp8++) {
    matches_item_t x = matches.data[tmp8];
    tmp6.data[tmp7] = x.title;
    tmp7++;
  }
  tmp6.len = tmp7;
  result_item_t result = (result_item_t){
      .rating = _min_float(tmp3), .northern_dark_movie = _min_string(tmp6)};
  printf("{");
  _json_string("rating");
  printf(":");
  _json_int(result.rating);
  printf(",");
  _json_string("northern_dark_movie");
  printf(":");
  _json_int(result.northern_dark_movie);
  printf("}");
  test_Q14_selects_minimal_rating_and_title_for_dark_movies_result = result;
  test_Q14_selects_minimal_rating_and_title_for_dark_movies();
  free(matches.data);
  free(tmp3.data);
  free(tmp6.data);
  return 0;
}
