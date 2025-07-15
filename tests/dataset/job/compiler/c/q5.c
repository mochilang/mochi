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
  const char *typical_european_movie;
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
  int ct_id;
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
  int it_id;
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
  int t_id;
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
  const char *info;
  int info_type_id;
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
  const char *typical_european_movie;
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
    test_Q5_finds_the_lexicographically_first_qualifying_title_result;
static void test_Q5_finds_the_lexicographically_first_qualifying_title() {
  tmp1_t tmp1[] = {(tmp1_t){.typical_european_movie = "A Film"}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_Q5_finds_the_lexicographically_first_qualifying_title_result.len !=
      tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0;
         i3 <
         test_Q5_finds_the_lexicographically_first_qualifying_title_result.len;
         i3++) {
      if (test_Q5_finds_the_lexicographically_first_qualifying_title_result
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
  company_type_t company_type[] = {
      (company_type_t){.ct_id = 1, .kind = "production companies"},
      (company_type_t){.ct_id = 2, .kind = "other"}};
  int company_type_len = sizeof(company_type) / sizeof(company_type[0]);
  info_type_t info_type[] = {(info_type_t){.it_id = 10, .info = "languages"}};
  int info_type_len = sizeof(info_type) / sizeof(info_type[0]);
  title_t title[] = {
      (title_t){.t_id = 100, .title = "B Movie", .production_year = 2010},
      (title_t){.t_id = 200, .title = "A Film", .production_year = 2012},
      (title_t){.t_id = 300, .title = "Old Movie", .production_year = 2000}};
  int title_len = sizeof(title) / sizeof(title[0]);
  movie_companie_t movie_companies[] = {
      (movie_companie_t){.movie_id = 100,
                         .company_type_id = 1,
                         .note = "ACME (France) (theatrical)"},
      (movie_companie_t){.movie_id = 200,
                         .company_type_id = 1,
                         .note = "ACME (France) (theatrical)"},
      (movie_companie_t){.movie_id = 300,
                         .company_type_id = 1,
                         .note = "ACME (France) (theatrical)"}};
  int movie_companies_len =
      sizeof(movie_companies) / sizeof(movie_companies[0]);
  movie_info_t movie_info[] = {
      (movie_info_t){.movie_id = 100, .info = "German", .info_type_id = 10},
      (movie_info_t){.movie_id = 200, .info = "Swedish", .info_type_id = 10},
      (movie_info_t){.movie_id = 300, .info = "German", .info_type_id = 10}};
  int movie_info_len = sizeof(movie_info) / sizeof(movie_info[0]);
  list_string candidate_titles = list_string_create(8);
  candidate_titles.data[0] = "Sweden";
  candidate_titles.data[1] = "Norway";
  candidate_titles.data[2] = "Germany";
  candidate_titles.data[3] = "Denmark";
  candidate_titles.data[4] = "Swedish";
  candidate_titles.data[5] = "Denish";
  candidate_titles.data[6] = "Norwegian";
  candidate_titles.data[7] = "German";
  int tmp4 = int_create(company_type.len * movie_companies.len *
                        movie_info.len * info_type.len * title.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < company_type_len; tmp6++) {
    company_type_t ct = company_type[tmp6];
    for (int tmp7 = 0; tmp7 < movie_companies_len; tmp7++) {
      movie_companie_t mc = movie_companies[tmp7];
      if (!(mc.company_type_id == ct.ct_id)) {
        continue;
      }
      for (int tmp8 = 0; tmp8 < movie_info_len; tmp8++) {
        movie_info_t mi = movie_info[tmp8];
        if (!(mi.movie_id == mc.movie_id)) {
          continue;
        }
        for (int tmp9 = 0; tmp9 < info_type_len; tmp9++) {
          info_type_t it = info_type[tmp9];
          if (!(it.it_id == mi.info_type_id)) {
            continue;
          }
          for (int tmp10 = 0; tmp10 < title_len; tmp10++) {
            title_t t = title[tmp10];
            if (!(t.t_id == mc.movie_id)) {
              continue;
            }
            if (!((strcmp(ct.kind, "production companies") == 0) &&
                  "(theatrical)" in mc.note && "(France)" in mc.note &&
                  t.production_year > 2005 &&
                  (contains_list_string(candidate_titles, mi.info)))) {
              continue;
            }
            tmp4.data[tmp5] = t.title;
            tmp5++;
          }
        }
      }
    }
  }
  tmp4.len = tmp5;
  int candidate_titles = tmp4;
  result_t result[] = {
      (result_t){.typical_european_movie = _min_string(candidate_titles)}};
  int result_len = sizeof(result) / sizeof(result[0]);
  printf("[");
  for (int i11 = 0; i11 < result_len; i11++) {
    if (i11 > 0)
      printf(",");
    result_t it = result[i11];
    printf("{");
    _json_string("typical_european_movie");
    printf(":");
    _json_string(it.typical_european_movie);
    printf("}");
  }
  printf("]");
  test_Q5_finds_the_lexicographically_first_qualifying_title_result = result;
  test_Q5_finds_the_lexicographically_first_qualifying_title();
  return 0;
}
