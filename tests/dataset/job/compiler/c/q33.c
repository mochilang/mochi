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
  const char *first_company;
  const char *second_company;
  const char *first_rating;
  const char *second_rating;
  const char *first_movie;
  const char *second_movie;
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
  int movie_id;
  int company_id;
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
  const char *title;
  int kind_id;
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
  const char *first_company;
  const char *second_company;
  const char *first_rating;
  const char *second_rating;
  const char *first_movie;
  const char *second_movie;
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
  const char *first_company;
  const char *second_company;
  const char *first_rating;
  const char *second_rating;
  const char *first_movie;
  const char *second_movie;
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

static list_int test_Q33_finds_linked_TV_series_with_low_rated_sequel_result;
static void test_Q33_finds_linked_TV_series_with_low_rated_sequel() {
  tmp1_t tmp1[] = {(tmp1_t){.first_company = "US Studio",
                            .second_company = "GB Studio",
                            .first_rating = "7.0",
                            .second_rating = "2.5",
                            .first_movie = "Series A",
                            .second_movie = "Series B"}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_Q33_finds_linked_TV_series_with_low_rated_sequel_result.len !=
      tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0;
         i3 < test_Q33_finds_linked_TV_series_with_low_rated_sequel_result.len;
         i3++) {
      if (test_Q33_finds_linked_TV_series_with_low_rated_sequel_result
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
      (company_name_t){.id = 1, .name = "US Studio", .country_code = "[us]"},
      (company_name_t){.id = 2, .name = "GB Studio", .country_code = "[gb]"}};
  int company_name_len = sizeof(company_name) / sizeof(company_name[0]);
  info_type_t info_type[] = {(info_type_t){.id = 1, .info = "rating"},
                             (info_type_t){.id = 2, .info = "other"}};
  int info_type_len = sizeof(info_type) / sizeof(info_type[0]);
  kind_type_t kind_type[] = {(kind_type_t){.id = 1, .kind = "tv series"},
                             (kind_type_t){.id = 2, .kind = "movie"}};
  int kind_type_len = sizeof(kind_type) / sizeof(kind_type[0]);
  link_type_t link_type[] = {(link_type_t){.id = 1, .link = "follows"},
                             (link_type_t){.id = 2, .link = "remake of"}};
  int link_type_len = sizeof(link_type) / sizeof(link_type[0]);
  movie_companie_t movie_companies[] = {
      (movie_companie_t){.movie_id = 10, .company_id = 1},
      (movie_companie_t){.movie_id = 20, .company_id = 2}};
  int movie_companies_len =
      sizeof(movie_companies) / sizeof(movie_companies[0]);
  movie_info_idx_t movie_info_idx[] = {
      (movie_info_idx_t){.movie_id = 10, .info_type_id = 1, .info = "7.0"},
      (movie_info_idx_t){.movie_id = 20, .info_type_id = 1, .info = "2.5"}};
  int movie_info_idx_len = sizeof(movie_info_idx) / sizeof(movie_info_idx[0]);
  movie_link_t movie_link[] = {
      (movie_link_t){.movie_id = 10, .linked_movie_id = 20, .link_type_id = 1}};
  int movie_link_len = sizeof(movie_link) / sizeof(movie_link[0]);
  title_t title[] = {
      (title_t){
          .id = 10, .title = "Series A", .kind_id = 1, .production_year = 2004},
      (title_t){.id = 20,
                .title = "Series B",
                .kind_id = 1,
                .production_year = 2006}};
  int title_len = sizeof(title) / sizeof(title[0]);
  rows_item_list_t tmp4 = rows_item_list_t_create(
      company_name.len * movie_companies.len * title.len * movie_info_idx.len *
      info_type.len * kind_type.len * movie_link.len * title.len *
      movie_info_idx.len * info_type.len * kind_type.len * movie_companies.len *
      company_name.len * link_type.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < company_name_len; tmp6++) {
    company_name_t cn1 = company_name[tmp6];
    for (int tmp7 = 0; tmp7 < movie_companies_len; tmp7++) {
      movie_companie_t mc1 = movie_companies[tmp7];
      if (!(cn1.id == mc1.company_id)) {
        continue;
      }
      for (int tmp8 = 0; tmp8 < title_len; tmp8++) {
        title_t t1 = title[tmp8];
        if (!(t1.id == mc1.movie_id)) {
          continue;
        }
        for (int tmp9 = 0; tmp9 < movie_info_idx_len; tmp9++) {
          movie_info_idx_t mi_idx1 = movie_info_idx[tmp9];
          if (!(mi_idx1.movie_id == t1.id)) {
            continue;
          }
          for (int tmp10 = 0; tmp10 < info_type_len; tmp10++) {
            info_type_t it1 = info_type[tmp10];
            if (!(it1.id == mi_idx1.info_type_id)) {
              continue;
            }
            for (int tmp11 = 0; tmp11 < kind_type_len; tmp11++) {
              kind_type_t kt1 = kind_type[tmp11];
              if (!(kt1.id == t1.kind_id)) {
                continue;
              }
              for (int tmp12 = 0; tmp12 < movie_link_len; tmp12++) {
                movie_link_t ml = movie_link[tmp12];
                if (!(ml.movie_id == t1.id)) {
                  continue;
                }
                for (int tmp13 = 0; tmp13 < title_len; tmp13++) {
                  title_t t2 = title[tmp13];
                  if (!(t2.id == ml.linked_movie_id)) {
                    continue;
                  }
                  for (int tmp14 = 0; tmp14 < movie_info_idx_len; tmp14++) {
                    movie_info_idx_t mi_idx2 = movie_info_idx[tmp14];
                    if (!(mi_idx2.movie_id == t2.id)) {
                      continue;
                    }
                    for (int tmp15 = 0; tmp15 < info_type_len; tmp15++) {
                      info_type_t it2 = info_type[tmp15];
                      if (!(it2.id == mi_idx2.info_type_id)) {
                        continue;
                      }
                      for (int tmp16 = 0; tmp16 < kind_type_len; tmp16++) {
                        kind_type_t kt2 = kind_type[tmp16];
                        if (!(kt2.id == t2.kind_id)) {
                          continue;
                        }
                        for (int tmp17 = 0; tmp17 < movie_companies_len;
                             tmp17++) {
                          movie_companie_t mc2 = movie_companies[tmp17];
                          if (!(mc2.movie_id == t2.id)) {
                            continue;
                          }
                          for (int tmp18 = 0; tmp18 < company_name_len;
                               tmp18++) {
                            company_name_t cn2 = company_name[tmp18];
                            if (!(cn2.id == mc2.company_id)) {
                              continue;
                            }
                            for (int tmp19 = 0; tmp19 < link_type_len;
                                 tmp19++) {
                              link_type_t lt = link_type[tmp19];
                              if (!(lt.id == ml.link_type_id)) {
                                continue;
                              }
                              if (!((strcmp(cn1.country_code, "[us]") == 0) &&
                                    it1.info == "rating" &&
                                    it2.info == "rating" &&
                                    kt1.kind == "tv series" &&
                                    kt2.kind == "tv series" &&
                                    ((strcmp(lt.link, "sequel") == 0) ||
                                     lt.link == "follows" ||
                                     lt.link == "followed by") &&
                                    mi_idx2.info < "3.0" &&
                                    t2.production_year >= 2005 &&
                                    t2.production_year <= 2008)) {
                                continue;
                              }
                              tmp4.data[tmp5] =
                                  (rows_item_t){.first_company = cn1.name,
                                                .second_company = cn2.name,
                                                .first_rating = mi_idx1.info,
                                                .second_rating = mi_idx2.info,
                                                .first_movie = t1.title,
                                                .second_movie = t2.title};
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
      }
    }
  }
  tmp4.len = tmp5;
  rows_item_list_t rows = tmp4;
  int tmp20 = int_create(rows.len);
  int tmp21 = 0;
  for (int tmp22 = 0; tmp22 < rows.len; tmp22++) {
    rows_item_t r = rows.data[tmp22];
    tmp20.data[tmp21] = r.first_company;
    tmp21++;
  }
  tmp20.len = tmp21;
  int tmp23 = int_create(rows.len);
  int tmp24 = 0;
  for (int tmp25 = 0; tmp25 < rows.len; tmp25++) {
    rows_item_t r = rows.data[tmp25];
    tmp23.data[tmp24] = r.second_company;
    tmp24++;
  }
  tmp23.len = tmp24;
  int tmp26 = int_create(rows.len);
  int tmp27 = 0;
  for (int tmp28 = 0; tmp28 < rows.len; tmp28++) {
    rows_item_t r = rows.data[tmp28];
    tmp26.data[tmp27] = r.first_rating;
    tmp27++;
  }
  tmp26.len = tmp27;
  int tmp29 = int_create(rows.len);
  int tmp30 = 0;
  for (int tmp31 = 0; tmp31 < rows.len; tmp31++) {
    rows_item_t r = rows.data[tmp31];
    tmp29.data[tmp30] = r.second_rating;
    tmp30++;
  }
  tmp29.len = tmp30;
  int tmp32 = int_create(rows.len);
  int tmp33 = 0;
  for (int tmp34 = 0; tmp34 < rows.len; tmp34++) {
    rows_item_t r = rows.data[tmp34];
    tmp32.data[tmp33] = r.first_movie;
    tmp33++;
  }
  tmp32.len = tmp33;
  int tmp35 = int_create(rows.len);
  int tmp36 = 0;
  for (int tmp37 = 0; tmp37 < rows.len; tmp37++) {
    rows_item_t r = rows.data[tmp37];
    tmp35.data[tmp36] = r.second_movie;
    tmp36++;
  }
  tmp35.len = tmp36;
  result_t result[] = {(result_t){.first_company = _min_string(tmp20),
                                  .second_company = _min_string(tmp23),
                                  .first_rating = _min_string(tmp26),
                                  .second_rating = _min_string(tmp29),
                                  .first_movie = _min_string(tmp32),
                                  .second_movie = _min_string(tmp35)}};
  int result_len = sizeof(result) / sizeof(result[0]);
  printf("[");
  for (int i38 = 0; i38 < result_len; i38++) {
    if (i38 > 0)
      printf(",");
    result_t it = result[i38];
    printf("{");
    _json_string("first_company");
    printf(":");
    _json_string(it.first_company);
    printf(",");
    _json_string("second_company");
    printf(":");
    _json_string(it.second_company);
    printf(",");
    _json_string("first_rating");
    printf(":");
    _json_string(it.first_rating);
    printf(",");
    _json_string("second_rating");
    printf(":");
    _json_string(it.second_rating);
    printf(",");
    _json_string("first_movie");
    printf(":");
    _json_string(it.first_movie);
    printf(",");
    _json_string("second_movie");
    printf(":");
    _json_string(it.second_movie);
    printf("}");
  }
  printf("]");
  test_Q33_finds_linked_TV_series_with_low_rated_sequel_result = result;
  test_Q33_finds_linked_TV_series_with_low_rated_sequel();
  free(tmp20.data);
  free(tmp23.data);
  free(tmp26.data);
  free(tmp29.data);
  free(tmp32.data);
  free(tmp35.data);
  return 0;
}
