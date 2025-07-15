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
  const char *link_type;
  const char *first_movie;
  const char *second_movie;
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
  const char *link_type;
  const char *first_movie;
  const char *second_movie;
} joined_item_t;
typedef struct {
  int len;
  joined_item_t *data;
} joined_item_list_t;
joined_item_list_t create_joined_item_list(int len) {
  joined_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(joined_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int link_type;
  int first_movie;
  int second_movie;
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

static int test_Q32_finds_movie_link_for_10_000_mile_club_result;
static void test_Q32_finds_movie_link_for_10_000_mile_club() {
  if (!(test_Q32_finds_movie_link_for_10_000_mile_club_result ==
        (tmp_item_t){.link_type = "sequel",
                     .first_movie = "Movie A",
                     .second_movie = "Movie C"})) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  keyword_t keyword[] = {
      (keyword_t){.id = 1, .keyword = "10,000-mile-club"},
      (keyword_t){.id = 2, .keyword = "character-name-in-title"}};
  int keyword_len = sizeof(keyword) / sizeof(keyword[0]);
  link_type_t link_type[] = {(link_type_t){.id = 1, .link = "sequel"},
                             (link_type_t){.id = 2, .link = "remake"}};
  int link_type_len = sizeof(link_type) / sizeof(link_type[0]);
  movie_keyword_t movie_keyword[] = {
      (movie_keyword_t){.movie_id = 100, .keyword_id = 1},
      (movie_keyword_t){.movie_id = 200, .keyword_id = 2}};
  int movie_keyword_len = sizeof(movie_keyword) / sizeof(movie_keyword[0]);
  movie_link_t movie_link[] = {
      (movie_link_t){
          .movie_id = 100, .linked_movie_id = 300, .link_type_id = 1},
      (movie_link_t){
          .movie_id = 200, .linked_movie_id = 400, .link_type_id = 2}};
  int movie_link_len = sizeof(movie_link) / sizeof(movie_link[0]);
  title_t title[] = {(title_t){.id = 100, .title = "Movie A"},
                     (title_t){.id = 200, .title = "Movie B"},
                     (title_t){.id = 300, .title = "Movie C"},
                     (title_t){.id = 400, .title = "Movie D"}};
  int title_len = sizeof(title) / sizeof(title[0]);
  joined_item_list_t tmp1 =
      joined_item_list_t_create(keyword.len * movie_keyword.len * title.len *
                                movie_link.len * title.len * link_type.len);
  int tmp2 = 0;
  for (int tmp3 = 0; tmp3 < keyword_len; tmp3++) {
    keyword_t k = keyword[tmp3];
    for (int tmp4 = 0; tmp4 < movie_keyword_len; tmp4++) {
      movie_keyword_t mk = movie_keyword[tmp4];
      if (!(mk.keyword_id == k.id)) {
        continue;
      }
      for (int tmp5 = 0; tmp5 < title_len; tmp5++) {
        title_t t1 = title[tmp5];
        if (!(t1.id == mk.movie_id)) {
          continue;
        }
        for (int tmp6 = 0; tmp6 < movie_link_len; tmp6++) {
          movie_link_t ml = movie_link[tmp6];
          if (!(ml.movie_id == t1.id)) {
            continue;
          }
          for (int tmp7 = 0; tmp7 < title_len; tmp7++) {
            title_t t2 = title[tmp7];
            if (!(t2.id == ml.linked_movie_id)) {
              continue;
            }
            for (int tmp8 = 0; tmp8 < link_type_len; tmp8++) {
              link_type_t lt = link_type[tmp8];
              if (!(lt.id == ml.link_type_id)) {
                continue;
              }
              if (!((strcmp(k.keyword, "10,000-mile-club") == 0))) {
                continue;
              }
              tmp1.data[tmp2] = (joined_item_t){.link_type = lt.link,
                                                .first_movie = t1.title,
                                                .second_movie = t2.title};
              tmp2++;
            }
          }
        }
      }
    }
  }
  tmp1.len = tmp2;
  joined_item_list_t joined = tmp1;
  int tmp9 = int_create(joined.len);
  int tmp10 = 0;
  for (int tmp11 = 0; tmp11 < joined.len; tmp11++) {
    joined_item_t r = joined.data[tmp11];
    tmp9.data[tmp10] = r.link_type;
    tmp10++;
  }
  tmp9.len = tmp10;
  int tmp12 = int_create(joined.len);
  int tmp13 = 0;
  for (int tmp14 = 0; tmp14 < joined.len; tmp14++) {
    joined_item_t r = joined.data[tmp14];
    tmp12.data[tmp13] = r.first_movie;
    tmp13++;
  }
  tmp12.len = tmp13;
  int tmp15 = int_create(joined.len);
  int tmp16 = 0;
  for (int tmp17 = 0; tmp17 < joined.len; tmp17++) {
    joined_item_t r = joined.data[tmp17];
    tmp15.data[tmp16] = r.second_movie;
    tmp16++;
  }
  tmp15.len = tmp16;
  result_item_t result = (result_item_t){.link_type = _min_string(tmp9),
                                         .first_movie = _min_string(tmp12),
                                         .second_movie = _min_string(tmp15)};
  list_int tmp18 = list_int_create(1);
  tmp18.data[0] = result;
  printf("[");
  for (int i19 = 0; i19 < 1; i19++) {
    if (i19 > 0)
      printf(",");
    result_item_t it = tmp18.data[i19];
    printf("{");
    _json_string("link_type");
    printf(":");
    _json_int(it.link_type);
    printf(",");
    _json_string("first_movie");
    printf(":");
    _json_int(it.first_movie);
    printf(",");
    _json_string("second_movie");
    printf(":");
    _json_int(it.second_movie);
    printf("}");
  }
  printf("]");
  test_Q32_finds_movie_link_for_10_000_mile_club_result = result;
  test_Q32_finds_movie_link_for_10_000_mile_club();
  free(tmp9.data);
  free(tmp12.data);
  free(tmp15.data);
  return 0;
}
