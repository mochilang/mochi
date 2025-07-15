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
  const char *movie_keyword;
  const char *actor_name;
  const char *marvel_movie;
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
  int person_id;
} cast_info_t;
typedef struct {
  int len;
  cast_info_t *data;
} cast_info_list_t;
cast_info_list_t create_cast_info_list(int len) {
  cast_info_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(cast_info_t));
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
  const char *name;
} name_t;
typedef struct {
  int len;
  name_t *data;
} name_list_t;
name_list_t create_name_list(int len) {
  name_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(name_t));
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
  const char *movie_keyword;
  const char *actor_name;
  const char *marvel_movie;
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

static list_int test_Q6_finds_marvel_movie_with_Robert_Downey_result;
static void test_Q6_finds_marvel_movie_with_Robert_Downey() {
  tmp1_t tmp1[] = {(tmp1_t){.movie_keyword = "marvel-cinematic-universe",
                            .actor_name = "Downey Robert Jr.",
                            .marvel_movie = "Iron Man 3"}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_Q6_finds_marvel_movie_with_Robert_Downey_result.len != tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0;
         i3 < test_Q6_finds_marvel_movie_with_Robert_Downey_result.len; i3++) {
      if (test_Q6_finds_marvel_movie_with_Robert_Downey_result.data[i3] !=
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
  cast_info_t cast_info[] = {(cast_info_t){.movie_id = 1, .person_id = 101},
                             (cast_info_t){.movie_id = 2, .person_id = 102}};
  int cast_info_len = sizeof(cast_info) / sizeof(cast_info[0]);
  keyword_t keyword[] = {
      (keyword_t){.id = 100, .keyword = "marvel-cinematic-universe"},
      (keyword_t){.id = 200, .keyword = "other"}};
  int keyword_len = sizeof(keyword) / sizeof(keyword[0]);
  movie_keyword_t movie_keyword[] = {
      (movie_keyword_t){.movie_id = 1, .keyword_id = 100},
      (movie_keyword_t){.movie_id = 2, .keyword_id = 200}};
  int movie_keyword_len = sizeof(movie_keyword) / sizeof(movie_keyword[0]);
  name_t name[] = {(name_t){.id = 101, .name = "Downey Robert Jr."},
                   (name_t){.id = 102, .name = "Chris Evans"}};
  int name_len = sizeof(name) / sizeof(name[0]);
  title_t title[] = {
      (title_t){.id = 1, .title = "Iron Man 3", .production_year = 2013},
      (title_t){.id = 2, .title = "Old Movie", .production_year = 2000}};
  int title_len = sizeof(title) / sizeof(title[0]);
  result_item_list_t tmp4 = result_item_list_t_create(
      cast_info.len * movie_keyword.len * keyword.len * name.len * title.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < cast_info_len; tmp6++) {
    cast_info_t ci = cast_info[tmp6];
    for (int tmp7 = 0; tmp7 < movie_keyword_len; tmp7++) {
      movie_keyword_t mk = movie_keyword[tmp7];
      if (!(ci.movie_id == mk.movie_id)) {
        continue;
      }
      for (int tmp8 = 0; tmp8 < keyword_len; tmp8++) {
        keyword_t k = keyword[tmp8];
        if (!(mk.keyword_id == k.id)) {
          continue;
        }
        for (int tmp9 = 0; tmp9 < name_len; tmp9++) {
          name_t n = name[tmp9];
          if (!(ci.person_id == n.id)) {
            continue;
          }
          for (int tmp10 = 0; tmp10 < title_len; tmp10++) {
            title_t t = title[tmp10];
            if (!(ci.movie_id == t.id)) {
              continue;
            }
            if (!((strcmp(k.keyword, "marvel-cinematic-universe") == 0) &&
                  contains_string(n.name, "Downey") &&
                  contains_string(n.name, "Robert") &&
                  t.production_year > 2010)) {
              continue;
            }
            tmp4.data[tmp5] = (result_item_t){.movie_keyword = k.keyword,
                                              .actor_name = n.name,
                                              .marvel_movie = t.title};
            tmp5++;
          }
        }
      }
    }
  }
  tmp4.len = tmp5;
  result_item_list_t result = tmp4;
  printf("[");
  for (int i11 = 0; i11 < result.len; i11++) {
    if (i11 > 0)
      printf(",");
    result_item_t it = result.data[i11];
    printf("{");
    _json_string("movie_keyword");
    printf(":");
    _json_string(it.movie_keyword);
    printf(",");
    _json_string("actor_name");
    printf(":");
    _json_string(it.actor_name);
    printf(",");
    _json_string("marvel_movie");
    printf(":");
    _json_string(it.marvel_movie);
    printf("}");
  }
  printf("]");
  test_Q6_finds_marvel_movie_with_Robert_Downey_result = result;
  test_Q6_finds_marvel_movie_with_Robert_Downey();
  return 0;
}
