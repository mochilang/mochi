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

static int
    test_Q2_finds_earliest_title_for_German_companies_with_character_keyword_result;
static void
test_Q2_finds_earliest_title_for_German_companies_with_character_keyword() {
  if (!((test_Q2_finds_earliest_title_for_German_companies_with_character_keyword_result ==
         "Der Film"))) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  company_name_t company_name[] = {
      (company_name_t){.id = 1, .country_code = "[de]"},
      (company_name_t){.id = 2, .country_code = "[us]"}};
  int company_name_len = sizeof(company_name) / sizeof(company_name[0]);
  keyword_t keyword[] = {
      (keyword_t){.id = 1, .keyword = "character-name-in-title"},
      (keyword_t){.id = 2, .keyword = "other"}};
  int keyword_len = sizeof(keyword) / sizeof(keyword[0]);
  movie_companie_t movie_companies[] = {
      (movie_companie_t){.movie_id = 100, .company_id = 1},
      (movie_companie_t){.movie_id = 200, .company_id = 2}};
  int movie_companies_len =
      sizeof(movie_companies) / sizeof(movie_companies[0]);
  movie_keyword_t movie_keyword[] = {
      (movie_keyword_t){.movie_id = 100, .keyword_id = 1},
      (movie_keyword_t){.movie_id = 200, .keyword_id = 2}};
  int movie_keyword_len = sizeof(movie_keyword) / sizeof(movie_keyword[0]);
  title_t title[] = {(title_t){.id = 100, .title = "Der Film"},
                     (title_t){.id = 200, .title = "Other Movie"}};
  int title_len = sizeof(title) / sizeof(title[0]);
  int tmp1 = int_create(company_name.len * movie_companies.len * title.len *
                        movie_keyword.len * keyword.len);
  int tmp2 = 0;
  for (int tmp3 = 0; tmp3 < company_name_len; tmp3++) {
    company_name_t cn = company_name[tmp3];
    for (int tmp4 = 0; tmp4 < movie_companies_len; tmp4++) {
      movie_companie_t mc = movie_companies[tmp4];
      if (!(mc.company_id == cn.id)) {
        continue;
      }
      for (int tmp5 = 0; tmp5 < title_len; tmp5++) {
        title_t t = title[tmp5];
        if (!(mc.movie_id == t.id)) {
          continue;
        }
        for (int tmp6 = 0; tmp6 < movie_keyword_len; tmp6++) {
          movie_keyword_t mk = movie_keyword[tmp6];
          if (!(mk.movie_id == t.id)) {
            continue;
          }
          for (int tmp7 = 0; tmp7 < keyword_len; tmp7++) {
            keyword_t k = keyword[tmp7];
            if (!(mk.keyword_id == k.id)) {
              continue;
            }
            if (!((strcmp(cn.country_code, "[de]") == 0) &&
                  k.keyword == "character-name-in-title" &&
                  mc.movie_id == mk.movie_id)) {
              continue;
            }
            tmp1.data[tmp2] = t.title;
            tmp2++;
          }
        }
      }
    }
  }
  tmp1.len = tmp2;
  int titles = tmp1;
  int result = _min_string(titles);
  _json_int(result);
  test_Q2_finds_earliest_title_for_German_companies_with_character_keyword_result =
      result;
  test_Q2_finds_earliest_title_for_German_companies_with_character_keyword();
  return 0;
}
