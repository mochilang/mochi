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
  const char *movie_company;
  double rating;
  const char *western_violent_movie;
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
  int movie_id;
  int company_id;
  int company_type_id;
  const char *note;
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
  const char *company;
  double rating;
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
  const char *movie_company;
  double rating;
  const char *western_violent_movie;
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

static list_int test_Q22_finds_western_violent_movie_with_low_rating_result;
static void test_Q22_finds_western_violent_movie_with_low_rating() {
  tmp1_t tmp1[] = {(tmp1_t){.movie_company = "Euro Films",
                            .rating = 6.5,
                            .western_violent_movie = "Violent Western"}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_Q22_finds_western_violent_movie_with_low_rating_result.len !=
      tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0;
         i3 < test_Q22_finds_western_violent_movie_with_low_rating_result.len;
         i3++) {
      if (test_Q22_finds_western_violent_movie_with_low_rating_result
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
      (company_name_t){.id = 1, .name = "Euro Films", .country_code = "[de]"},
      (company_name_t){.id = 2, .name = "US Films", .country_code = "[us]"}};
  int company_name_len = sizeof(company_name) / sizeof(company_name[0]);
  company_type_t company_type[] = {
      (company_type_t){.id = 1, .kind = "production"}};
  int company_type_len = sizeof(company_type) / sizeof(company_type[0]);
  info_type_t info_type[] = {(info_type_t){.id = 10, .info = "countries"},
                             (info_type_t){.id = 20, .info = "rating"}};
  int info_type_len = sizeof(info_type) / sizeof(info_type[0]);
  keyword_t keyword[] = {(keyword_t){.id = 1, .keyword = "murder"},
                         (keyword_t){.id = 2, .keyword = "comedy"}};
  int keyword_len = sizeof(keyword) / sizeof(keyword[0]);
  kind_type_t kind_type[] = {(kind_type_t){.id = 100, .kind = "movie"},
                             (kind_type_t){.id = 200, .kind = "episode"}};
  int kind_type_len = sizeof(kind_type) / sizeof(kind_type[0]);
  movie_companie_t movie_companies[] = {
      (movie_companie_t){.movie_id = 10,
                         .company_id = 1,
                         .company_type_id = 1,
                         .note = "release (2009) (worldwide)"},
      (movie_companie_t){.movie_id = 20,
                         .company_id = 2,
                         .company_type_id = 1,
                         .note = "release (2007) (USA)"}};
  int movie_companies_len =
      sizeof(movie_companies) / sizeof(movie_companies[0]);
  movie_info_t movie_info[] = {
      (movie_info_t){.movie_id = 10, .info_type_id = 10, .info = "Germany"},
      (movie_info_t){.movie_id = 20, .info_type_id = 10, .info = "USA"}};
  int movie_info_len = sizeof(movie_info) / sizeof(movie_info[0]);
  movie_info_idx_t movie_info_idx[] = {
      (movie_info_idx_t){.movie_id = 10, .info_type_id = 20, .info = 6.5},
      (movie_info_idx_t){.movie_id = 20, .info_type_id = 20, .info = 7.8}};
  int movie_info_idx_len = sizeof(movie_info_idx) / sizeof(movie_info_idx[0]);
  movie_keyword_t movie_keyword[] = {
      (movie_keyword_t){.movie_id = 10, .keyword_id = 1},
      (movie_keyword_t){.movie_id = 20, .keyword_id = 2}};
  int movie_keyword_len = sizeof(movie_keyword) / sizeof(movie_keyword[0]);
  title_t title[] = {(title_t){.id = 10,
                               .kind_id = 100,
                               .production_year = 2009,
                               .title = "Violent Western"},
                     (title_t){.id = 20,
                               .kind_id = 100,
                               .production_year = 2007,
                               .title = "Old Western"}};
  int title_len = sizeof(title) / sizeof(title[0]);
  rows_item_list_t tmp4 = rows_item_list_t_create(
      company_name.len * movie_companies.len * company_type.len * title.len *
      movie_keyword.len * keyword.len * movie_info.len * info_type.len *
      movie_info_idx.len * info_type.len * kind_type.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < company_name_len; tmp6++) {
    company_name_t cn = company_name[tmp6];
    for (int tmp7 = 0; tmp7 < movie_companies_len; tmp7++) {
      movie_companie_t mc = movie_companies[tmp7];
      if (!(cn.id == mc.company_id)) {
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
          for (int tmp10 = 0; tmp10 < movie_keyword_len; tmp10++) {
            movie_keyword_t mk = movie_keyword[tmp10];
            if (!(mk.movie_id == t.id)) {
              continue;
            }
            for (int tmp11 = 0; tmp11 < keyword_len; tmp11++) {
              keyword_t k = keyword[tmp11];
              if (!(k.id == mk.keyword_id)) {
                continue;
              }
              for (int tmp12 = 0; tmp12 < movie_info_len; tmp12++) {
                movie_info_t mi = movie_info[tmp12];
                if (!(mi.movie_id == t.id)) {
                  continue;
                }
                for (int tmp13 = 0; tmp13 < info_type_len; tmp13++) {
                  info_type_t it1 = info_type[tmp13];
                  if (!(it1.id == mi.info_type_id)) {
                    continue;
                  }
                  for (int tmp14 = 0; tmp14 < movie_info_idx_len; tmp14++) {
                    movie_info_idx_t mi_idx = movie_info_idx[tmp14];
                    if (!(mi_idx.movie_id == t.id)) {
                      continue;
                    }
                    for (int tmp15 = 0; tmp15 < info_type_len; tmp15++) {
                      info_type_t it2 = info_type[tmp15];
                      if (!(it2.id == mi_idx.info_type_id)) {
                        continue;
                      }
                      for (int tmp16 = 0; tmp16 < kind_type_len; tmp16++) {
                        kind_type_t kt = kind_type[tmp16];
                        if (!(kt.id == t.kind_id)) {
                          continue;
                        }
                        if (!(((strcmp(cn.country_code, "[us]") != 0) &&
                               it1.info == "countries" &&
                               it2.info == "rating" &&
                               ((strcmp(k.keyword, "murder") == 0) ||
                                k.keyword == "murder-in-title" ||
                                k.keyword == "blood" ||
                                k.keyword == "violence") &&
                               ((strcmp(kt.kind, "movie") == 0) ||
                                kt.kind == "episode") &&
                               contains_string(mc.note, "(USA)") == 0 &&
                               contains_string(mc.note, "(200") &&
                               ((strcmp(mi.info, "Germany") == 0) ||
                                mi.info == "German" || mi.info == "USA" ||
                                mi.info == "American") &&
                               mi_idx.info < 7.0 && t.production_year > 2008 &&
                               kt.id == t.kind_id && t.id == mi.movie_id &&
                               t.id == mk.movie_id && t.id == mi_idx.movie_id &&
                               t.id == mc.movie_id &&
                               mk.movie_id == mi.movie_id &&
                               mk.movie_id == mi_idx.movie_id &&
                               mk.movie_id == mc.movie_id &&
                               mi.movie_id == mi_idx.movie_id &&
                               mi.movie_id == mc.movie_id &&
                               mc.movie_id == mi_idx.movie_id &&
                               k.id == mk.keyword_id &&
                               it1.id == mi.info_type_id &&
                               it2.id == mi_idx.info_type_id &&
                               ct.id == mc.company_type_id &&
                               cn.id == mc.company_id))) {
                          continue;
                        }
                        tmp4.data[tmp5] = (rows_item_t){.company = cn.name,
                                                        .rating = mi_idx.info,
                                                        .title = t.title};
                        tmp5++;
                      }
                    }
                  }
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
  int tmp17 = int_create(rows.len);
  int tmp18 = 0;
  for (int tmp19 = 0; tmp19 < rows.len; tmp19++) {
    rows_item_t r = rows.data[tmp19];
    tmp17.data[tmp18] = r.company;
    tmp18++;
  }
  tmp17.len = tmp18;
  list_float tmp20 = list_float_create(rows.len);
  int tmp21 = 0;
  for (int tmp22 = 0; tmp22 < rows.len; tmp22++) {
    rows_item_t r = rows.data[tmp22];
    tmp20.data[tmp21] = r.rating;
    tmp21++;
  }
  tmp20.len = tmp21;
  int tmp23 = int_create(rows.len);
  int tmp24 = 0;
  for (int tmp25 = 0; tmp25 < rows.len; tmp25++) {
    rows_item_t r = rows.data[tmp25];
    tmp23.data[tmp24] = r.title;
    tmp24++;
  }
  tmp23.len = tmp24;
  result_t result[] = {(result_t){.movie_company = _min_string(tmp17),
                                  .rating = _min_float(tmp20),
                                  .western_violent_movie = _min_string(tmp23)}};
  int result_len = sizeof(result) / sizeof(result[0]);
  printf("[");
  for (int i26 = 0; i26 < result_len; i26++) {
    if (i26 > 0)
      printf(",");
    result_t it = result[i26];
    printf("{");
    _json_string("movie_company");
    printf(":");
    _json_string(it.movie_company);
    printf(",");
    _json_string("rating");
    printf(":");
    _json_float(it.rating);
    printf(",");
    _json_string("western_violent_movie");
    printf(":");
    _json_string(it.western_violent_movie);
    printf("}");
  }
  printf("]");
  test_Q22_finds_western_violent_movie_with_low_rating_result = result;
  test_Q22_finds_western_violent_movie_with_low_rating();
  free(tmp17.data);
  free(tmp20.data);
  free(tmp23.data);
  return 0;
}
