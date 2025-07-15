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
  const char *production_note;
  const char *movie_title;
  int movie_year;
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
  int movie_id;
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
  const char *note;
  const char *title;
  int year;
} filtered_item_t;
typedef struct {
  int len;
  filtered_item_t *data;
} filtered_item_list_t;
filtered_item_list_t create_filtered_item_list(int len) {
  filtered_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(filtered_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int production_note;
  int movie_title;
  int movie_year;
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
    test_Q1_returns_min_note__title_and_year_for_top_ranked_co_production_result;
static void
test_Q1_returns_min_note__title_and_year_for_top_ranked_co_production() {
  if (!(test_Q1_returns_min_note__title_and_year_for_top_ranked_co_production_result ==
        (tmp_item_t){.production_note = "ACME (co-production)",
                     .movie_title = "Good Movie",
                     .movie_year = 1995})) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  company_type_t company_type[] = {
      (company_type_t){.id = 1, .kind = "production companies"},
      (company_type_t){.id = 2, .kind = "distributors"}};
  int company_type_len = sizeof(company_type) / sizeof(company_type[0]);
  info_type_t info_type[] = {(info_type_t){.id = 10, .info = "top 250 rank"},
                             (info_type_t){.id = 20, .info = "bottom 10 rank"}};
  int info_type_len = sizeof(info_type) / sizeof(info_type[0]);
  title_t title[] = {
      (title_t){.id = 100, .title = "Good Movie", .production_year = 1995},
      (title_t){.id = 200, .title = "Bad Movie", .production_year = 2000}};
  int title_len = sizeof(title) / sizeof(title[0]);
  movie_companie_t movie_companies[] = {
      (movie_companie_t){.movie_id = 100,
                         .company_type_id = 1,
                         .note = "ACME (co-production)"},
      (movie_companie_t){.movie_id = 200,
                         .company_type_id = 1,
                         .note = "MGM (as Metro-Goldwyn-Mayer Pictures)"}};
  int movie_companies_len =
      sizeof(movie_companies) / sizeof(movie_companies[0]);
  movie_info_idx_t movie_info_idx[] = {
      (movie_info_idx_t){.movie_id = 100, .info_type_id = 10},
      (movie_info_idx_t){.movie_id = 200, .info_type_id = 20}};
  int movie_info_idx_len = sizeof(movie_info_idx) / sizeof(movie_info_idx[0]);
  filtered_item_list_t tmp1 = filtered_item_list_t_create(
      company_type.len * movie_companies.len * title.len * movie_info_idx.len *
      info_type.len);
  int tmp2 = 0;
  for (int tmp3 = 0; tmp3 < company_type_len; tmp3++) {
    company_type_t ct = company_type[tmp3];
    for (int tmp4 = 0; tmp4 < movie_companies_len; tmp4++) {
      movie_companie_t mc = movie_companies[tmp4];
      if (!(ct.id == mc.company_type_id)) {
        continue;
      }
      for (int tmp5 = 0; tmp5 < title_len; tmp5++) {
        title_t t = title[tmp5];
        if (!(t.id == mc.movie_id)) {
          continue;
        }
        for (int tmp6 = 0; tmp6 < movie_info_idx_len; tmp6++) {
          movie_info_idx_t mi = movie_info_idx[tmp6];
          if (!(mi.movie_id == t.id)) {
            continue;
          }
          for (int tmp7 = 0; tmp7 < info_type_len; tmp7++) {
            info_type_t it = info_type[tmp7];
            if (!(it.id == mi.info_type_id)) {
              continue;
            }
            if (!((strcmp(ct.kind, "production companies") == 0) &&
                  it.info == "top 250 rank" &&
                  ((!contains_string(mc.note,
                                     "(as Metro-Goldwyn-Mayer Pictures)"))) &&
                  (contains_string(mc.note, "(co-production)") ||
                   contains_string(mc.note, "(presents)")))) {
              continue;
            }
            tmp1.data[tmp2] = (filtered_item_t){
                .note = mc.note, .title = t.title, .year = t.production_year};
            tmp2++;
          }
        }
      }
    }
  }
  tmp1.len = tmp2;
  filtered_item_list_t filtered = tmp1;
  int tmp8 = int_create(filtered.len);
  int tmp9 = 0;
  for (int tmp10 = 0; tmp10 < filtered.len; tmp10++) {
    filtered_item_t r = filtered.data[tmp10];
    tmp8.data[tmp9] = r.note;
    tmp9++;
  }
  tmp8.len = tmp9;
  int tmp11 = int_create(filtered.len);
  int tmp12 = 0;
  for (int tmp13 = 0; tmp13 < filtered.len; tmp13++) {
    filtered_item_t r = filtered.data[tmp13];
    tmp11.data[tmp12] = r.title;
    tmp12++;
  }
  tmp11.len = tmp12;
  list_int tmp14 = list_int_create(filtered.len);
  int tmp15 = 0;
  for (int tmp16 = 0; tmp16 < filtered.len; tmp16++) {
    filtered_item_t r = filtered.data[tmp16];
    tmp14.data[tmp15] = r.year;
    tmp15++;
  }
  tmp14.len = tmp15;
  result_item_t result = (result_item_t){.production_note = _min_string(tmp8),
                                         .movie_title = _min_string(tmp11),
                                         .movie_year = _min_int(tmp14)};
  list_int tmp17 = list_int_create(1);
  tmp17.data[0] = result;
  printf("[");
  for (int i18 = 0; i18 < 1; i18++) {
    if (i18 > 0)
      printf(",");
    result_item_t it = tmp17.data[i18];
    printf("{");
    _json_string("production_note");
    printf(":");
    _json_int(it.production_note);
    printf(",");
    _json_string("movie_title");
    printf(":");
    _json_int(it.movie_title);
    printf(",");
    _json_string("movie_year");
    printf(":");
    _json_int(it.movie_year);
    printf("}");
  }
  printf("]");
  test_Q1_returns_min_note__title_and_year_for_top_ranked_co_production_result =
      result;
  test_Q1_returns_min_note__title_and_year_for_top_ranked_co_production();
  free(tmp8.data);
  free(tmp11.data);
  free(tmp14.data);
  return 0;
}
