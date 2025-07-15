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
  const char *movie_company;
  double rating;
  const char *drama_horror_movie;
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
  int id;
  const char *name;
  const char *country_code;
} company_name_t;
typedef struct {
  int len;
  company_name_t *data;
} company_name_list_t;
company_name_list_t create_company_name_list(int len) {
  company_name_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(company_name_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  const char *kind;
} company_type_t;
typedef struct {
  int len;
  company_type_t *data;
} company_type_list_t;
company_type_list_t create_company_type_list(int len) {
  company_type_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(company_type_t));
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
  int movie_id;
  int company_id;
  int company_type_id;
} movie_companie_t;
typedef struct {
  int len;
  movie_companie_t *data;
} movie_companie_list_t;
movie_companie_list_t create_movie_companie_list(int len) {
  movie_companie_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(movie_companie_t));
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
  int id;
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
  const char *movie_company;
  double rating;
  const char *drama_horror_movie;
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

static list_int
    test_Q12_finds_high_rated_US_drama_or_horror_with_company_result;
static void test_Q12_finds_high_rated_US_drama_or_horror_with_company() {
  tmp1_t tmp1[] = {(tmp1_t){.movie_company = "Best Pictures",
                            .rating = 8.3,
                            .drama_horror_movie = "Great Drama"}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_Q12_finds_high_rated_US_drama_or_horror_with_company_result.len !=
      tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0;
         i3 <
         test_Q12_finds_high_rated_US_drama_or_horror_with_company_result.len;
         i3++) {
      if (test_Q12_finds_high_rated_US_drama_or_horror_with_company_result
              .data[i3] != tmp1.data[i3]) {
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
  company_name_t company_name[] = {
      (company_name_t){
          .id = 1, .name = "Best Pictures", .country_code = "[us]"},
      (company_name_t){
          .id = 2, .name = "Foreign Films", .country_code = "[uk]"}};
  int company_name_len = sizeof(company_name) / sizeof(company_name[0]);
  company_type_t company_type[] = {
      (company_type_t){.id = 10, .kind = "production companies"},
      (company_type_t){.id = 20, .kind = "distributors"}};
  int company_type_len = sizeof(company_type) / sizeof(company_type[0]);
  info_type_t info_type[] = {(info_type_t){.id = 100, .info = "genres"},
                             (info_type_t){.id = 200, .info = "rating"}};
  int info_type_len = sizeof(info_type) / sizeof(info_type[0]);
  movie_companie_t movie_companies[] = {
      (movie_companie_t){
          .movie_id = 1000, .company_id = 1, .company_type_id = 10},
      (movie_companie_t){
          .movie_id = 2000, .company_id = 2, .company_type_id = 10}};
  int movie_companies_len =
      sizeof(movie_companies) / sizeof(movie_companies[0]);
  movie_info_t movie_info[] = {
      (movie_info_t){.movie_id = 1000, .info_type_id = 100, .info = "Drama"},
      (movie_info_t){.movie_id = 2000, .info_type_id = 100, .info = "Horror"}};
  int movie_info_len = sizeof(movie_info) / sizeof(movie_info[0]);
  movie_info_idx_t movie_info_idx[] = {
      (movie_info_idx_t){.movie_id = 1000, .info_type_id = 200, .info = 8.3},
      (movie_info_idx_t){.movie_id = 2000, .info_type_id = 200, .info = 7.5}};
  int movie_info_idx_len = sizeof(movie_info_idx) / sizeof(movie_info_idx[0]);
  title_t title[] = {
      (title_t){.id = 1000, .production_year = 2006, .title = "Great Drama"},
      (title_t){.id = 2000, .production_year = 2007, .title = "Low Rated"}};
  int title_len = sizeof(title) / sizeof(title[0]);
  result_item_list_t tmp4 = result_item_list_t_create(
      company_name.len * movie_companies.len * company_type.len * title.len *
      movie_info.len * info_type.len * movie_info_idx.len * info_type.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < company_name_len; tmp6++) {
    company_name_t cn = company_name[tmp6];
    for (int tmp7 = 0; tmp7 < movie_companies_len; tmp7++) {
      movie_companie_t mc = movie_companies[tmp7];
      if (!(mc.company_id == cn.id)) {
        continue;
      }
      for (int tmp8 = 0; tmp8 < company_type_len; tmp8++) {
        company_type_t ct = company_type[tmp8];
        if (!(ct.id == mc.company_type_id)) {
          continue;
        }
        for (int tmp9 = 0; tmp9 < title_len; tmp9++) {
          title_t t = title[tmp9];
          if (!(t.id == mc.movie_id)) {
            continue;
          }
          for (int tmp10 = 0; tmp10 < movie_info_len; tmp10++) {
            movie_info_t mi = movie_info[tmp10];
            if (!(mi.movie_id == t.id)) {
              continue;
            }
            for (int tmp11 = 0; tmp11 < info_type_len; tmp11++) {
              info_type_t it1 = info_type[tmp11];
              if (!(it1.id == mi.info_type_id)) {
                continue;
              }
              for (int tmp12 = 0; tmp12 < movie_info_idx_len; tmp12++) {
                movie_info_idx_t mi_idx = movie_info_idx[tmp12];
                if (!(mi_idx.movie_id == t.id)) {
                  continue;
                }
                for (int tmp13 = 0; tmp13 < info_type_len; tmp13++) {
                  info_type_t it2 = info_type[tmp13];
                  if (!(it2.id == mi_idx.info_type_id)) {
                    continue;
                  }
                  if (!((strcmp(cn.country_code, "[us]") == 0) &&
                        ct.kind == "production companies" &&
                        it1.info == "genres" && it2.info == "rating" &&
                        ((strcmp(mi.info, "Drama") == 0) ||
                         mi.info == "Horror") &&
                        mi_idx.info > 8.0 && t.production_year >= 2005 &&
                        t.production_year <= 2008)) {
                    continue;
                  }
                  tmp4.data[tmp5] =
                      (result_item_t){.movie_company = cn.name,
                                      .rating = mi_idx.info,
                                      .drama_horror_movie = t.title};
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
  result_item_list_t result = tmp4;
  printf("[");
  for (int i14 = 0; i14 < result.len; i14++) {
    if (i14 > 0)
      printf(",");
    result_item_t it = result.data[i14];
    printf("{");
    _json_string("movie_company");
    printf(":");
    _json_string(it.movie_company);
    printf(",");
    _json_string("rating");
    printf(":");
    _json_float(it.rating);
    printf(",");
    _json_string("drama_horror_movie");
    printf(":");
    _json_string(it.drama_horror_movie);
    printf("}");
  }
  printf("]");
  test_Q12_finds_high_rated_US_drama_or_horror_with_company_result = result;
  test_Q12_finds_high_rated_US_drama_or_horror_with_company();
  return 0;
}
