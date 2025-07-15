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
  const char *movie_title;
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
  const char *movie_title;
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

static list_int test_Q3_returns_lexicographically_smallest_sequel_title_result;
static void test_Q3_returns_lexicographically_smallest_sequel_title() {
  tmp1_t tmp1[] = {(tmp1_t){.movie_title = "Alpha"}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_Q3_returns_lexicographically_smallest_sequel_title_result.len !=
      tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0;
         i3 <
         test_Q3_returns_lexicographically_smallest_sequel_title_result.len;
         i3++) {
      if (test_Q3_returns_lexicographically_smallest_sequel_title_result
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
  keyword_t keyword[] = {(keyword_t){.id = 1, .keyword = "amazing sequel"},
                         (keyword_t){.id = 2, .keyword = "prequel"}};
  int keyword_len = sizeof(keyword) / sizeof(keyword[0]);
  movie_info_t movie_info[] = {
      (movie_info_t){.movie_id = 10, .info = "Germany"},
      (movie_info_t){.movie_id = 30, .info = "Sweden"},
      (movie_info_t){.movie_id = 20, .info = "France"}};
  int movie_info_len = sizeof(movie_info) / sizeof(movie_info[0]);
  movie_keyword_t movie_keyword[] = {
      (movie_keyword_t){.movie_id = 10, .keyword_id = 1},
      (movie_keyword_t){.movie_id = 30, .keyword_id = 1},
      (movie_keyword_t){.movie_id = 20, .keyword_id = 1},
      (movie_keyword_t){.movie_id = 10, .keyword_id = 2}};
  int movie_keyword_len = sizeof(movie_keyword) / sizeof(movie_keyword[0]);
  title_t title[] = {
      (title_t){.id = 10, .title = "Alpha", .production_year = 2006},
      (title_t){.id = 30, .title = "Beta", .production_year = 2008},
      (title_t){.id = 20, .title = "Gamma", .production_year = 2009}};
  int title_len = sizeof(title) / sizeof(title[0]);
  list_string allowed_infos = list_string_create(8);
  allowed_infos.data[0] = "Sweden";
  allowed_infos.data[1] = "Norway";
  allowed_infos.data[2] = "Germany";
  allowed_infos.data[3] = "Denmark";
  allowed_infos.data[4] = "Swedish";
  allowed_infos.data[5] = "Denish";
  allowed_infos.data[6] = "Norwegian";
  allowed_infos.data[7] = "German";
  int allowed_infos = allowed_infos;
  int tmp4 =
      int_create(keyword.len * movie_keyword.len * movie_info.len * title.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < keyword_len; tmp6++) {
    keyword_t k = keyword[tmp6];
    for (int tmp7 = 0; tmp7 < movie_keyword_len; tmp7++) {
      movie_keyword_t mk = movie_keyword[tmp7];
      if (!(mk.keyword_id == k.id)) {
        continue;
      }
      for (int tmp8 = 0; tmp8 < movie_info_len; tmp8++) {
        movie_info_t mi = movie_info[tmp8];
        if (!(mi.movie_id == mk.movie_id)) {
          continue;
        }
        for (int tmp9 = 0; tmp9 < title_len; tmp9++) {
          title_t t = title[tmp9];
          if (!(t.id == mi.movie_id)) {
            continue;
          }
          if (!(contains_list_string(allowed_infos,
                                     contains_string(k.keyword, "sequel") &&
                                         mi.info) &&
                t.production_year > 2005 && mk.movie_id == mi.movie_id)) {
            continue;
          }
          tmp4.data[tmp5] = t.title;
          tmp5++;
        }
      }
    }
  }
  tmp4.len = tmp5;
  int candidate_titles = tmp4;
  result_t result[] = {
      (result_t){.movie_title = _min_string(candidate_titles)}};
  int result_len = sizeof(result) / sizeof(result[0]);
  printf("[");
  for (int i10 = 0; i10 < result_len; i10++) {
    if (i10 > 0)
      printf(",");
    result_t it = result[i10];
    printf("{");
    _json_string("movie_title");
    printf(":");
    _json_string(it.movie_title);
    printf("}");
  }
  printf("]");
  test_Q3_returns_lexicographically_smallest_sequel_title_result = result;
  test_Q3_returns_lexicographically_smallest_sequel_title();
  return 0;
}
