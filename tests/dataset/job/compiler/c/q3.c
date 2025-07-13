
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
  char *movie_title;
} TmpItem;
typedef struct {
  int len;
  TmpItem *data;
} list_TmpItem;
static list_TmpItem list_TmpItem_create(int len) {
  list_TmpItem l;
  l.len = len;
  l.data = calloc(len, sizeof(TmpItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  char *keyword;
} KeywordItem;
typedef struct {
  int len;
  KeywordItem *data;
} list_KeywordItem;
static list_KeywordItem list_KeywordItem_create(int len) {
  list_KeywordItem l;
  l.len = len;
  l.data = calloc(len, sizeof(KeywordItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int movie_id;
  char *info;
} Movie_infoItem;
typedef struct {
  int len;
  Movie_infoItem *data;
} list_Movie_infoItem;
static list_Movie_infoItem list_Movie_infoItem_create(int len) {
  list_Movie_infoItem l;
  l.len = len;
  l.data = calloc(len, sizeof(Movie_infoItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int movie_id;
  int keyword_id;
} Movie_keywordItem;
typedef struct {
  int len;
  Movie_keywordItem *data;
} list_Movie_keywordItem;
static list_Movie_keywordItem list_Movie_keywordItem_create(int len) {
  list_Movie_keywordItem l;
  l.len = len;
  l.data = calloc(len, sizeof(Movie_keywordItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  char *title;
  int production_year;
} TitleItem;
typedef struct {
  int len;
  TitleItem *data;
} list_TitleItem;
static list_TitleItem list_TitleItem_create(int len) {
  list_TitleItem l;
  l.len = len;
  l.data = calloc(len, sizeof(TitleItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  char *movie_title;
} ResultItem;
typedef struct {
  int len;
  ResultItem *data;
} list_ResultItem;
static list_ResultItem list_ResultItem_create(int len) {
  list_ResultItem l;
  l.len = len;
  l.data = calloc(len, sizeof(ResultItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

static list_int test_Q3_returns_lexicographically_smallest_sequel_title_result;
static void test_Q3_returns_lexicographically_smallest_sequel_title() {
  list_int tmp1 = list_int_create(1);
  tmp1.data[0] = (TmpItem){.movie_title = "Alpha"};
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
  KeywordItem tmp4_data[] = {
      (KeywordItem){.id = 1, .keyword = "amazing sequel"},
      (KeywordItem){.id = 2, .keyword = "prequel"}};
  list_KeywordItem tmp4 = {2, tmp4_data};
  list_KeywordItem keyword = tmp4;
  Movie_infoItem tmp5_data[] = {
      (Movie_infoItem){.movie_id = 10, .info = "Germany"},
      (Movie_infoItem){.movie_id = 30, .info = "Sweden"},
      (Movie_infoItem){.movie_id = 20, .info = "France"}};
  list_Movie_infoItem tmp5 = {3, tmp5_data};
  list_Movie_infoItem movie_info = tmp5;
  Movie_keywordItem tmp6_data[] = {
      (Movie_keywordItem){.movie_id = 10, .keyword_id = 1},
      (Movie_keywordItem){.movie_id = 30, .keyword_id = 1},
      (Movie_keywordItem){.movie_id = 20, .keyword_id = 1},
      (Movie_keywordItem){.movie_id = 10, .keyword_id = 2}};
  list_Movie_keywordItem tmp6 = {4, tmp6_data};
  list_Movie_keywordItem movie_keyword = tmp6;
  TitleItem tmp7_data[] = {
      (TitleItem){.id = 10, .title = "Alpha", .production_year = 2006},
      (TitleItem){.id = 30, .title = "Beta", .production_year = 2008},
      (TitleItem){.id = 20, .title = "Gamma", .production_year = 2009}};
  list_TitleItem tmp7 = {3, tmp7_data};
  list_TitleItem title = tmp7;
  list_string tmp8 = list_string_create(8);
  tmp8.data[0] = "Sweden";
  tmp8.data[1] = "Norway";
  tmp8.data[2] = "Germany";
  tmp8.data[3] = "Denmark";
  tmp8.data[4] = "Swedish";
  tmp8.data[5] = "Denish";
  tmp8.data[6] = "Norwegian";
  tmp8.data[7] = "German";
  list_string allowed_infos = tmp8;
  list_string tmp9 = list_string_create(keyword.len * movie_keyword.len *
                                        movie_info.len * title.len);
  int tmp10 = 0;
  for (int tmp11 = 0; tmp11 < keyword.len; tmp11++) {
    KeywordItem k = keyword.data[tmp11];
    for (int tmp12 = 0; tmp12 < movie_keyword.len; tmp12++) {
      Movie_keywordItem mk = movie_keyword.data[tmp12];
      if (!(mk.keyword_id == k.id)) {
        continue;
      }
      for (int tmp13 = 0; tmp13 < movie_info.len; tmp13++) {
        Movie_infoItem mi = movie_info.data[tmp13];
        if (!(mi.movie_id == mk.movie_id)) {
          continue;
        }
        for (int tmp14 = 0; tmp14 < title.len; tmp14++) {
          TitleItem t = title.data[tmp14];
          if (!(t.id == mi.movie_id)) {
            continue;
          }
          if (!(contains_list_string(allowed_infos,
                                     contains_string(k.keyword, "sequel") &&
                                         mi.info) &&
                t.production_year > 2005 && mk.movie_id == mi.movie_id)) {
            continue;
          }
          tmp9.data[tmp10] = t.title;
          tmp10++;
        }
      }
    }
  }
  tmp9.len = tmp10;
  list_string candidate_titles = tmp9;
  ResultItem tmp15_data[] = {
      (ResultItem){.movie_title = _min_string(candidate_titles)}};
  list_ResultItem tmp15 = {1, tmp15_data};
  list_ResultItem result = tmp15;
  printf("[");
  for (int i16 = 0; i16 < result.len; i16++) {
    if (i16 > 0)
      printf(",");
    ResultItem it = result.data[i16];
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
