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
  const char *release_date;
  const char *internet_movie;
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
} aka_title_t;
typedef struct {
  int len;
  aka_title_t *data;
} aka_title_list_t;
aka_title_list_t create_aka_title_list(int len) {
  aka_title_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(aka_title_t));
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
  const char *release_date;
  const char *internet_movie;
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
  const char *release_date;
  const char *internet_movie;
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

static list_int
    test_Q15_finds_the_earliest_US_internet_movie_release_after_2000_result;
static void test_Q15_finds_the_earliest_US_internet_movie_release_after_2000() {
  tmp1_t tmp1[] = {(tmp1_t){.release_date = "USA: March 2005",
                            .internet_movie = "Example Movie"}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_Q15_finds_the_earliest_US_internet_movie_release_after_2000_result
          .len != tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0;
         i3 <
         test_Q15_finds_the_earliest_US_internet_movie_release_after_2000_result
             .len;
         i3++) {
      if (test_Q15_finds_the_earliest_US_internet_movie_release_after_2000_result
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
  aka_title_t aka_title[] = {(aka_title_t){.movie_id = 1},
                             (aka_title_t){.movie_id = 2}};
  int aka_title_len = sizeof(aka_title) / sizeof(aka_title[0]);
  company_name_t company_name[] = {
      (company_name_t){.id = 1, .country_code = "[us]"},
      (company_name_t){.id = 2, .country_code = "[gb]"}};
  int company_name_len = sizeof(company_name) / sizeof(company_name[0]);
  company_type_t company_type[] = {(company_type_t){.id = 10},
                                   (company_type_t){.id = 20}};
  int company_type_len = sizeof(company_type) / sizeof(company_type[0]);
  info_type_t info_type[] = {(info_type_t){.id = 5, .info = "release dates"},
                             (info_type_t){.id = 6, .info = "other"}};
  int info_type_len = sizeof(info_type) / sizeof(info_type[0]);
  keyword_t keyword[] = {(keyword_t){.id = 100}, (keyword_t){.id = 200}};
  int keyword_len = sizeof(keyword) / sizeof(keyword[0]);
  movie_companie_t movie_companies[] = {
      (movie_companie_t){.movie_id = 1,
                         .company_id = 1,
                         .company_type_id = 10,
                         .note = "release (2005) (worldwide)"},
      (movie_companie_t){.movie_id = 2,
                         .company_id = 2,
                         .company_type_id = 20,
                         .note = "release (1999) (worldwide)"}};
  int movie_companies_len =
      sizeof(movie_companies) / sizeof(movie_companies[0]);
  movie_info_t movie_info[] = {(movie_info_t){.movie_id = 1,
                                              .info_type_id = 5,
                                              .note = "internet",
                                              .info = "USA: March 2005"},
                               (movie_info_t){.movie_id = 2,
                                              .info_type_id = 5,
                                              .note = "theater",
                                              .info = "USA: May 1999"}};
  int movie_info_len = sizeof(movie_info) / sizeof(movie_info[0]);
  movie_keyword_t movie_keyword[] = {
      (movie_keyword_t){.movie_id = 1, .keyword_id = 100},
      (movie_keyword_t){.movie_id = 2, .keyword_id = 200}};
  int movie_keyword_len = sizeof(movie_keyword) / sizeof(movie_keyword[0]);
  title_t title[] = {
      (title_t){.id = 1, .title = "Example Movie", .production_year = 2005},
      (title_t){.id = 2, .title = "Old Movie", .production_year = 1999}};
  int title_len = sizeof(title) / sizeof(title[0]);
  rows_item_list_t tmp4 = rows_item_list_t_create(
      title.len * aka_title.len * movie_info.len * movie_keyword.len *
      movie_companies.len * keyword.len * info_type.len * company_name.len *
      company_type.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < title_len; tmp6++) {
    title_t t = title[tmp6];
    for (int tmp7 = 0; tmp7 < aka_title_len; tmp7++) {
      aka_title_t at = aka_title[tmp7];
      if (!(at.movie_id == t.id)) {
        continue;
      }
      for (int tmp8 = 0; tmp8 < movie_info_len; tmp8++) {
        movie_info_t mi = movie_info[tmp8];
        if (!(mi.movie_id == t.id)) {
          continue;
        }
        for (int tmp9 = 0; tmp9 < movie_keyword_len; tmp9++) {
          movie_keyword_t mk = movie_keyword[tmp9];
          if (!(mk.movie_id == t.id)) {
            continue;
          }
          for (int tmp10 = 0; tmp10 < movie_companies_len; tmp10++) {
            movie_companie_t mc = movie_companies[tmp10];
            if (!(mc.movie_id == t.id)) {
              continue;
            }
            for (int tmp11 = 0; tmp11 < keyword_len; tmp11++) {
              keyword_t k = keyword[tmp11];
              if (!(k.id == mk.keyword_id)) {
                continue;
              }
              for (int tmp12 = 0; tmp12 < info_type_len; tmp12++) {
                info_type_t it1 = info_type[tmp12];
                if (!(it1.id == mi.info_type_id)) {
                  continue;
                }
                for (int tmp13 = 0; tmp13 < company_name_len; tmp13++) {
                  company_name_t cn = company_name[tmp13];
                  if (!(cn.id == mc.company_id)) {
                    continue;
                  }
                  for (int tmp14 = 0; tmp14 < company_type_len; tmp14++) {
                    company_type_t ct = company_type[tmp14];
                    if (!(ct.id == mc.company_type_id)) {
                      continue;
                    }
                    if (!((strcmp(cn.country_code, "[us]") == 0) &&
                          it1.info == "release dates" &&
                          contains_string(mc.note, "200") &&
                          contains_string(mc.note, "worldwide") &&
                          contains_string(mi.note, "internet") &&
                          contains_string(mi.info, "USA:") &&
                          contains_string(mi.info, "200") &&
                          t.production_year > 2000)) {
                      continue;
                    }
                    tmp4.data[tmp5] = (rows_item_t){.release_date = mi.info,
                                                    .internet_movie = t.title};
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
  tmp4.len = tmp5;
  rows_item_list_t rows = tmp4;
  int tmp15 = int_create(rows.len);
  int tmp16 = 0;
  for (int tmp17 = 0; tmp17 < rows.len; tmp17++) {
    rows_item_t r = rows.data[tmp17];
    tmp15.data[tmp16] = r.release_date;
    tmp16++;
  }
  tmp15.len = tmp16;
  int tmp18 = int_create(rows.len);
  int tmp19 = 0;
  for (int tmp20 = 0; tmp20 < rows.len; tmp20++) {
    rows_item_t r = rows.data[tmp20];
    tmp18.data[tmp19] = r.internet_movie;
    tmp19++;
  }
  tmp18.len = tmp19;
  result_t result[] = {(result_t){.release_date = _min_string(tmp15),
                                  .internet_movie = _min_string(tmp18)}};
  int result_len = sizeof(result) / sizeof(result[0]);
  printf("[");
  for (int i21 = 0; i21 < result_len; i21++) {
    if (i21 > 0)
      printf(",");
    result_t it = result[i21];
    printf("{");
    _json_string("release_date");
    printf(":");
    _json_string(it.release_date);
    printf(",");
    _json_string("internet_movie");
    printf(":");
    _json_string(it.internet_movie);
    printf("}");
  }
  printf("]");
  test_Q15_finds_the_earliest_US_internet_movie_release_after_2000_result =
      result;
  test_Q15_finds_the_earliest_US_internet_movie_release_after_2000();
  free(tmp15.data);
  free(tmp18.data);
  return 0;
}
