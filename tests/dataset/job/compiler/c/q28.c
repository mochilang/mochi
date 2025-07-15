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
  const char *complete_euro_dark_movie;
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
  const char *kind;
} comp_cast_type_t;
typedef struct {
  int len;
  comp_cast_type_t *data;
} comp_cast_type_list_t;
comp_cast_type_list_t create_comp_cast_type_list(int len) {
  comp_cast_type_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(comp_cast_type_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int movie_id;
  int subject_id;
  int status_id;
} complete_cast_t;
typedef struct {
  int len;
  complete_cast_t *data;
} complete_cast_list_t;
complete_cast_list_t create_complete_cast_list(int len) {
  complete_cast_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(complete_cast_t));
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
  int movie_company;
  int rating;
  int complete_euro_dark_movie;
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

static int test_Q28_finds_euro_dark_movie_with_minimal_values_result;
static void test_Q28_finds_euro_dark_movie_with_minimal_values() {
  if (!(test_Q28_finds_euro_dark_movie_with_minimal_values_result ==
        (tmp_item_t){.movie_company = "Euro Films Ltd.",
                     .rating = 7.2,
                     .complete_euro_dark_movie = "Dark Euro Film"})) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  comp_cast_type_t comp_cast_type[] = {
      (comp_cast_type_t){.id = 1, .kind = "crew"},
      (comp_cast_type_t){.id = 2, .kind = "complete+verified"},
      (comp_cast_type_t){.id = 3, .kind = "partial"}};
  int comp_cast_type_len = sizeof(comp_cast_type) / sizeof(comp_cast_type[0]);
  complete_cast_t complete_cast[] = {
      (complete_cast_t){.movie_id = 1, .subject_id = 1, .status_id = 3},
      (complete_cast_t){.movie_id = 2, .subject_id = 1, .status_id = 2}};
  int complete_cast_len = sizeof(complete_cast) / sizeof(complete_cast[0]);
  company_name_t company_name[] = {
      (company_name_t){
          .id = 1, .name = "Euro Films Ltd.", .country_code = "[gb]"},
      (company_name_t){.id = 2, .name = "US Studios", .country_code = "[us]"}};
  int company_name_len = sizeof(company_name) / sizeof(company_name[0]);
  company_type_t company_type[] = {(company_type_t){.id = 1},
                                   (company_type_t){.id = 2}};
  int company_type_len = sizeof(company_type) / sizeof(company_type[0]);
  movie_companie_t movie_companies[] = {
      (movie_companie_t){.movie_id = 1,
                         .company_id = 1,
                         .company_type_id = 1,
                         .note = "production (2005) (UK)"},
      (movie_companie_t){.movie_id = 2,
                         .company_id = 2,
                         .company_type_id = 1,
                         .note = "production (USA)"}};
  int movie_companies_len =
      sizeof(movie_companies) / sizeof(movie_companies[0]);
  info_type_t info_type[] = {(info_type_t){.id = 1, .info = "countries"},
                             (info_type_t){.id = 2, .info = "rating"}};
  int info_type_len = sizeof(info_type) / sizeof(info_type[0]);
  keyword_t keyword[] = {(keyword_t){.id = 1, .keyword = "blood"},
                         (keyword_t){.id = 2, .keyword = "romance"}};
  int keyword_len = sizeof(keyword) / sizeof(keyword[0]);
  kind_type_t kind_type[] = {(kind_type_t){.id = 1, .kind = "movie"},
                             (kind_type_t){.id = 2, .kind = "episode"}};
  int kind_type_len = sizeof(kind_type) / sizeof(kind_type[0]);
  movie_info_t movie_info[] = {
      (movie_info_t){.movie_id = 1, .info_type_id = 1, .info = "Germany"},
      (movie_info_t){.movie_id = 2, .info_type_id = 1, .info = "USA"}};
  int movie_info_len = sizeof(movie_info) / sizeof(movie_info[0]);
  movie_info_idx_t movie_info_idx[] = {
      (movie_info_idx_t){.movie_id = 1, .info_type_id = 2, .info = 7.2},
      (movie_info_idx_t){.movie_id = 2, .info_type_id = 2, .info = 9.0}};
  int movie_info_idx_len = sizeof(movie_info_idx) / sizeof(movie_info_idx[0]);
  movie_keyword_t movie_keyword[] = {
      (movie_keyword_t){.movie_id = 1, .keyword_id = 1},
      (movie_keyword_t){.movie_id = 2, .keyword_id = 2}};
  int movie_keyword_len = sizeof(movie_keyword) / sizeof(movie_keyword[0]);
  title_t title[] = {
      (title_t){.id = 1,
                .kind_id = 1,
                .production_year = 2005,
                .title = "Dark Euro Film"},
      (title_t){
          .id = 2, .kind_id = 1, .production_year = 2005, .title = "US Film"}};
  int title_len = sizeof(title) / sizeof(title[0]);
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
  allowed_countries.data[5] = "Danish";
  allowed_countries.data[6] = "Norwegian";
  allowed_countries.data[7] = "German";
  allowed_countries.data[8] = "USA";
  allowed_countries.data[9] = "American";
  int allowed_countries = allowed_countries;
  list_string matches = list_string_create(2);
  matches.data[0] = "movie";
  matches.data[1] = "episode";
  matches_item_list_t tmp1 = matches_item_list_t_create(
      complete_cast.len * comp_cast_type.len * comp_cast_type.len *
      movie_companies.len * company_name.len * company_type.len *
      movie_keyword.len * keyword.len * movie_info.len * info_type.len *
      movie_info_idx.len * info_type.len * title.len * kind_type.len);
  int tmp2 = 0;
  for (int tmp3 = 0; tmp3 < complete_cast_len; tmp3++) {
    complete_cast_t cc = complete_cast[tmp3];
    for (int tmp4 = 0; tmp4 < comp_cast_type_len; tmp4++) {
      comp_cast_type_t cct1 = comp_cast_type[tmp4];
      if (!(cct1.id == cc.subject_id)) {
        continue;
      }
      for (int tmp5 = 0; tmp5 < comp_cast_type_len; tmp5++) {
        comp_cast_type_t cct2 = comp_cast_type[tmp5];
        if (!(cct2.id == cc.status_id)) {
          continue;
        }
        for (int tmp6 = 0; tmp6 < movie_companies_len; tmp6++) {
          movie_companie_t mc = movie_companies[tmp6];
          if (!(mc.movie_id == cc.movie_id)) {
            continue;
          }
          for (int tmp7 = 0; tmp7 < company_name_len; tmp7++) {
            company_name_t cn = company_name[tmp7];
            if (!(cn.id == mc.company_id)) {
              continue;
            }
            for (int tmp8 = 0; tmp8 < company_type_len; tmp8++) {
              company_type_t ct = company_type[tmp8];
              if (!(ct.id == mc.company_type_id)) {
                continue;
              }
              for (int tmp9 = 0; tmp9 < movie_keyword_len; tmp9++) {
                movie_keyword_t mk = movie_keyword[tmp9];
                if (!(mk.movie_id == cc.movie_id)) {
                  continue;
                }
                for (int tmp10 = 0; tmp10 < keyword_len; tmp10++) {
                  keyword_t k = keyword[tmp10];
                  if (!(k.id == mk.keyword_id)) {
                    continue;
                  }
                  for (int tmp11 = 0; tmp11 < movie_info_len; tmp11++) {
                    movie_info_t mi = movie_info[tmp11];
                    if (!(mi.movie_id == cc.movie_id)) {
                      continue;
                    }
                    for (int tmp12 = 0; tmp12 < info_type_len; tmp12++) {
                      info_type_t it1 = info_type[tmp12];
                      if (!(it1.id == mi.info_type_id)) {
                        continue;
                      }
                      for (int tmp13 = 0; tmp13 < movie_info_idx_len; tmp13++) {
                        movie_info_idx_t mi_idx = movie_info_idx[tmp13];
                        if (!(mi_idx.movie_id == cc.movie_id)) {
                          continue;
                        }
                        for (int tmp14 = 0; tmp14 < info_type_len; tmp14++) {
                          info_type_t it2 = info_type[tmp14];
                          if (!(it2.id == mi_idx.info_type_id)) {
                            continue;
                          }
                          for (int tmp15 = 0; tmp15 < title_len; tmp15++) {
                            title_t t = title[tmp15];
                            if (!(t.id == cc.movie_id)) {
                              continue;
                            }
                            for (int tmp16 = 0; tmp16 < kind_type_len;
                                 tmp16++) {
                              kind_type_t kt = kind_type[tmp16];
                              if (!(kt.id == t.kind_id)) {
                                continue;
                              }
                              if (!(((strcmp(cct1.kind, "crew") == 0) &&
                                     cct2.kind != "complete+verified" &&
                                     cn.country_code != "[us]" &&
                                     it1.info == "countries" &&
                                     it2.info == "rating" &&
                                     (contains_list_string(allowed_keywords,
                                                           k.keyword)) &&
                                     (contains_list_string(matches, kt.kind)) &&
                                     contains_string(mc.note, "(USA)") == 0 &&
                                     contains_string(mc.note, "(200") &&
                                     (contains_list_string(allowed_countries,
                                                           mi.info)) &&
                                     mi_idx.info < 8.5 &&
                                     t.production_year > 2000))) {
                                continue;
                              }
                              tmp1.data[tmp2] =
                                  (matches_item_t){.company = cn.name,
                                                   .rating = mi_idx.info,
                                                   .title = t.title};
                              tmp2++;
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
      }
    }
  }
  tmp1.len = tmp2;
  matches_item_list_t matches = tmp1;
  int tmp17 = int_create(matches.len);
  int tmp18 = 0;
  for (int tmp19 = 0; tmp19 < matches.len; tmp19++) {
    matches_item_t x = matches.data[tmp19];
    tmp17.data[tmp18] = x.company;
    tmp18++;
  }
  tmp17.len = tmp18;
  list_float tmp20 = list_float_create(matches.len);
  int tmp21 = 0;
  for (int tmp22 = 0; tmp22 < matches.len; tmp22++) {
    matches_item_t x = matches.data[tmp22];
    tmp20.data[tmp21] = x.rating;
    tmp21++;
  }
  tmp20.len = tmp21;
  int tmp23 = int_create(matches.len);
  int tmp24 = 0;
  for (int tmp25 = 0; tmp25 < matches.len; tmp25++) {
    matches_item_t x = matches.data[tmp25];
    tmp23.data[tmp24] = x.title;
    tmp24++;
  }
  tmp23.len = tmp24;
  result_item_t result =
      (result_item_t){.movie_company = _min_string(tmp17),
                      .rating = _min_float(tmp20),
                      .complete_euro_dark_movie = _min_string(tmp23)};
  printf("{");
  _json_string("movie_company");
  printf(":");
  _json_int(result.movie_company);
  printf(",");
  _json_string("rating");
  printf(":");
  _json_int(result.rating);
  printf(",");
  _json_string("complete_euro_dark_movie");
  printf(":");
  _json_int(result.complete_euro_dark_movie);
  printf("}");
  test_Q28_finds_euro_dark_movie_with_minimal_values_result = result;
  test_Q28_finds_euro_dark_movie_with_minimal_values();
  free(tmp17.data);
  free(tmp20.data);
  free(tmp23.data);
  return 0;
}
