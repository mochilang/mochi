
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
  char *movie_keyword;
  char *actor_name;
  char *marvel_movie;
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
  int movie_id;
  int person_id;
} Cast_infoItem;
typedef struct {
  int len;
  Cast_infoItem *data;
} list_Cast_infoItem;
static list_Cast_infoItem list_Cast_infoItem_create(int len) {
  list_Cast_infoItem l;
  l.len = len;
  l.data = calloc(len, sizeof(Cast_infoItem));
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
  char *name;
} NameItem;
typedef struct {
  int len;
  NameItem *data;
} list_NameItem;
static list_NameItem list_NameItem_create(int len) {
  list_NameItem l;
  l.len = len;
  l.data = calloc(len, sizeof(NameItem));
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
  char *movie_keyword;
  char *actor_name;
  char *marvel_movie;
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

static list_int test_Q6_finds_marvel_movie_with_Robert_Downey_result;
static void test_Q6_finds_marvel_movie_with_Robert_Downey() {
  list_int tmp1 = list_int_create(1);
  tmp1.data[0] = (TmpItem){.movie_keyword = "marvel-cinematic-universe",
                           .actor_name = "Downey Robert Jr.",
                           .marvel_movie = "Iron Man 3"};
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
  Cast_infoItem tmp4_data[] = {
      (Cast_infoItem){.movie_id = 1, .person_id = 101},
      (Cast_infoItem){.movie_id = 2, .person_id = 102}};
  list_Cast_infoItem tmp4 = {2, tmp4_data};
  list_Cast_infoItem cast_info = tmp4;
  KeywordItem tmp5_data[] = {
      (KeywordItem){.id = 100, .keyword = "marvel-cinematic-universe"},
      (KeywordItem){.id = 200, .keyword = "other"}};
  list_KeywordItem tmp5 = {2, tmp5_data};
  list_KeywordItem keyword = tmp5;
  Movie_keywordItem tmp6_data[] = {
      (Movie_keywordItem){.movie_id = 1, .keyword_id = 100},
      (Movie_keywordItem){.movie_id = 2, .keyword_id = 200}};
  list_Movie_keywordItem tmp6 = {2, tmp6_data};
  list_Movie_keywordItem movie_keyword = tmp6;
  NameItem tmp7_data[] = {(NameItem){.id = 101, .name = "Downey Robert Jr."},
                          (NameItem){.id = 102, .name = "Chris Evans"}};
  list_NameItem tmp7 = {2, tmp7_data};
  list_NameItem name = tmp7;
  TitleItem tmp8_data[] = {
      (TitleItem){.id = 1, .title = "Iron Man 3", .production_year = 2013},
      (TitleItem){.id = 2, .title = "Old Movie", .production_year = 2000}};
  list_TitleItem tmp8 = {2, tmp8_data};
  list_TitleItem title = tmp8;
  list_ResultItem tmp9 = list_ResultItem_create(
      cast_info.len * movie_keyword.len * keyword.len * name.len * title.len);
  int tmp10 = 0;
  for (int tmp11 = 0; tmp11 < cast_info.len; tmp11++) {
    Cast_infoItem ci = cast_info.data[tmp11];
    for (int tmp12 = 0; tmp12 < movie_keyword.len; tmp12++) {
      Movie_keywordItem mk = movie_keyword.data[tmp12];
      if (!(ci.movie_id == mk.movie_id)) {
        continue;
      }
      for (int tmp13 = 0; tmp13 < keyword.len; tmp13++) {
        KeywordItem k = keyword.data[tmp13];
        if (!(mk.keyword_id == k.id)) {
          continue;
        }
        for (int tmp14 = 0; tmp14 < name.len; tmp14++) {
          NameItem n = name.data[tmp14];
          if (!(ci.person_id == n.id)) {
            continue;
          }
          for (int tmp15 = 0; tmp15 < title.len; tmp15++) {
            TitleItem t = title.data[tmp15];
            if (!(ci.movie_id == t.id)) {
              continue;
            }
            if (!((strcmp(k.keyword, "marvel-cinematic-universe") == 0) &&
                  contains_string(n.name, "Downey") &&
                  contains_string(n.name, "Robert") &&
                  t.production_year > 2010)) {
              continue;
            }
            tmp9.data[tmp10] = (ResultItem){.movie_keyword = k.keyword,
                                            .actor_name = n.name,
                                            .marvel_movie = t.title};
            tmp10++;
          }
        }
      }
    }
  }
  tmp9.len = tmp10;
  list_ResultItem result = tmp9;
  printf("[");
  for (int i16 = 0; i16 < result.len; i16++) {
    if (i16 > 0)
      printf(",");
    ResultItem it = result.data[i16];
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
