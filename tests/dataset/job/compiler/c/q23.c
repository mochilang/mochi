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
  const char *movie_kind;
  const char *complete_us_internet_movie;
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
  int movie_id;
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
  int id;
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
  const char *note;
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
  const char *movie_kind;
  const char *complete_us_internet_movie;
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
  const char *movie_kind;
  const char *complete_us_internet_movie;
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

static list_int test_Q23_finds_US_internet_movie_with_verified_cast_result;
static void test_Q23_finds_US_internet_movie_with_verified_cast() {
  tmp1_t tmp1[] = {(tmp1_t){.movie_kind = "movie",
                            .complete_us_internet_movie = "Web Movie"}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_Q23_finds_US_internet_movie_with_verified_cast_result.len !=
      tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0;
         i3 < test_Q23_finds_US_internet_movie_with_verified_cast_result.len;
         i3++) {
      if (test_Q23_finds_US_internet_movie_with_verified_cast_result.data[i3] !=
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
  complete_cast_t complete_cast[] = {
      (complete_cast_t){.movie_id = 1, .status_id = 1},
      (complete_cast_t){.movie_id = 2, .status_id = 2}};
  int complete_cast_len = sizeof(complete_cast) / sizeof(complete_cast[0]);
  comp_cast_type_t comp_cast_type[] = {
      (comp_cast_type_t){.id = 1, .kind = "complete+verified"},
      (comp_cast_type_t){.id = 2, .kind = "partial"}};
  int comp_cast_type_len = sizeof(comp_cast_type) / sizeof(comp_cast_type[0]);
  company_name_t company_name[] = {
      (company_name_t){.id = 1, .country_code = "[us]"},
      (company_name_t){.id = 2, .country_code = "[gb]"}};
  int company_name_len = sizeof(company_name) / sizeof(company_name[0]);
  company_type_t company_type[] = {(company_type_t){.id = 1},
                                   (company_type_t){.id = 2}};
  int company_type_len = sizeof(company_type) / sizeof(company_type[0]);
  info_type_t info_type[] = {(info_type_t){.id = 1, .info = "release dates"},
                             (info_type_t){.id = 2, .info = "other"}};
  int info_type_len = sizeof(info_type) / sizeof(info_type[0]);
  keyword_t keyword[] = {(keyword_t){.id = 1, .keyword = "internet"},
                         (keyword_t){.id = 2, .keyword = "other"}};
  int keyword_len = sizeof(keyword) / sizeof(keyword[0]);
  kind_type_t kind_type[] = {(kind_type_t){.id = 1, .kind = "movie"},
                             (kind_type_t){.id = 2, .kind = "series"}};
  int kind_type_len = sizeof(kind_type) / sizeof(kind_type[0]);
  movie_companie_t movie_companies[] = {
      (movie_companie_t){.movie_id = 1, .company_id = 1, .company_type_id = 1},
      (movie_companie_t){.movie_id = 2, .company_id = 2, .company_type_id = 2}};
  int movie_companies_len =
      sizeof(movie_companies) / sizeof(movie_companies[0]);
  movie_info_t movie_info[] = {(movie_info_t){.movie_id = 1,
                                              .info_type_id = 1,
                                              .note = "internet release",
                                              .info = "USA: May 2005"},
                               (movie_info_t){.movie_id = 2,
                                              .info_type_id = 1,
                                              .note = "theater",
                                              .info = "USA: April 1998"}};
  int movie_info_len = sizeof(movie_info) / sizeof(movie_info[0]);
  movie_keyword_t movie_keyword[] = {
      (movie_keyword_t){.movie_id = 1, .keyword_id = 1},
      (movie_keyword_t){.movie_id = 2, .keyword_id = 2}};
  int movie_keyword_len = sizeof(movie_keyword) / sizeof(movie_keyword[0]);
  title_t title[] = {
      (title_t){
          .id = 1, .kind_id = 1, .production_year = 2005, .title = "Web Movie"},
      (title_t){.id = 2,
                .kind_id = 1,
                .production_year = 1998,
                .title = "Old Movie"}};
  int title_len = sizeof(title) / sizeof(title[0]);
  matches_item_list_t tmp4 = matches_item_list_t_create(
      complete_cast.len * comp_cast_type.len * title.len * kind_type.len *
      movie_info.len * info_type.len * movie_keyword.len * keyword.len *
      movie_companies.len * company_name.len * company_type.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < complete_cast_len; tmp6++) {
    complete_cast_t cc = complete_cast[tmp6];
    for (int tmp7 = 0; tmp7 < comp_cast_type_len; tmp7++) {
      comp_cast_type_t cct1 = comp_cast_type[tmp7];
      if (!(cct1.id == cc.status_id)) {
        continue;
      }
      for (int tmp8 = 0; tmp8 < title_len; tmp8++) {
        title_t t = title[tmp8];
        if (!(t.id == cc.movie_id)) {
          continue;
        }
        for (int tmp9 = 0; tmp9 < kind_type_len; tmp9++) {
          kind_type_t kt = kind_type[tmp9];
          if (!(kt.id == t.kind_id)) {
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
              for (int tmp12 = 0; tmp12 < movie_keyword_len; tmp12++) {
                movie_keyword_t mk = movie_keyword[tmp12];
                if (!(mk.movie_id == t.id)) {
                  continue;
                }
                for (int tmp13 = 0; tmp13 < keyword_len; tmp13++) {
                  keyword_t k = keyword[tmp13];
                  if (!(k.id == mk.keyword_id)) {
                    continue;
                  }
                  for (int tmp14 = 0; tmp14 < movie_companies_len; tmp14++) {
                    movie_companie_t mc = movie_companies[tmp14];
                    if (!(mc.movie_id == t.id)) {
                      continue;
                    }
                    for (int tmp15 = 0; tmp15 < company_name_len; tmp15++) {
                      company_name_t cn = company_name[tmp15];
                      if (!(cn.id == mc.company_id)) {
                        continue;
                      }
                      for (int tmp16 = 0; tmp16 < company_type_len; tmp16++) {
                        company_type_t ct = company_type[tmp16];
                        if (!(ct.id == mc.company_type_id)) {
                          continue;
                        }
                        if (!((strcmp(cct1.kind, "complete+verified") == 0) &&
                              cn.country_code == "[us]" &&
                              it1.info == "release dates" &&
                              kt.kind == "movie" &&
                              contains_string(mi.note, "internet") &&
                              (contains_string(mi.info, "USA:") &&
                               (contains_string(mi.info, "199") ||
                                contains_string(mi.info, "200"))) &&
                              t.production_year > 2000)) {
                          continue;
                        }
                        tmp4.data[tmp5] = (matches_item_t){
                            .movie_kind = kt.kind,
                            .complete_us_internet_movie = t.title};
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
  matches_item_list_t matches = tmp4;
  int tmp17 = int_create(matches.len);
  int tmp18 = 0;
  for (int tmp19 = 0; tmp19 < matches.len; tmp19++) {
    matches_item_t r = matches.data[tmp19];
    tmp17.data[tmp18] = r.movie_kind;
    tmp18++;
  }
  tmp17.len = tmp18;
  int tmp20 = int_create(matches.len);
  int tmp21 = 0;
  for (int tmp22 = 0; tmp22 < matches.len; tmp22++) {
    matches_item_t r = matches.data[tmp22];
    tmp20.data[tmp21] = r.complete_us_internet_movie;
    tmp21++;
  }
  tmp20.len = tmp21;
  result_t result[] = {
      (result_t){.movie_kind = _min_string(tmp17),
                 .complete_us_internet_movie = _min_string(tmp20)}};
  int result_len = sizeof(result) / sizeof(result[0]);
  printf("[");
  for (int i23 = 0; i23 < result_len; i23++) {
    if (i23 > 0)
      printf(",");
    result_t it = result[i23];
    printf("{");
    _json_string("movie_kind");
    printf(":");
    _json_string(it.movie_kind);
    printf(",");
    _json_string("complete_us_internet_movie");
    printf(":");
    _json_string(it.complete_us_internet_movie);
    printf("}");
  }
  printf("]");
  test_Q23_finds_US_internet_movie_with_verified_cast_result = result;
  test_Q23_finds_US_internet_movie_with_verified_cast();
  free(tmp17.data);
  free(tmp20.data);
  return 0;
}
