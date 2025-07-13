
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
  char *rating;
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
  char *info;
} Info_typeItem;
typedef struct {
  int len;
  Info_typeItem *data;
} list_Info_typeItem;
static list_Info_typeItem list_Info_typeItem_create(int len) {
  list_Info_typeItem l;
  l.len = len;
  l.data = calloc(len, sizeof(Info_typeItem));
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
  int movie_id;
  int info_type_id;
  char *info;
} Movie_info_idxItem;
typedef struct {
  int len;
  Movie_info_idxItem *data;
} list_Movie_info_idxItem;
static list_Movie_info_idxItem list_Movie_info_idxItem_create(int len) {
  list_Movie_info_idxItem l;
  l.len = len;
  l.data = calloc(len, sizeof(Movie_info_idxItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  char *rating;
  char *title;
} RowsItem;
typedef struct {
  int len;
  RowsItem *data;
} list_RowsItem;
static list_RowsItem list_RowsItem_create(int len) {
  list_RowsItem l;
  l.len = len;
  l.data = calloc(len, sizeof(RowsItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  char *rating;
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

static list_int test_Q4_returns_minimum_rating_and_title_for_sequels_result;
static void test_Q4_returns_minimum_rating_and_title_for_sequels() {
  list_int tmp1 = list_int_create(1);
  tmp1.data[0] = (TmpItem){.rating = "6.2", .movie_title = "Alpha Movie"};
  int tmp2 = 1;
  if (test_Q4_returns_minimum_rating_and_title_for_sequels_result.len !=
      tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0;
         i3 < test_Q4_returns_minimum_rating_and_title_for_sequels_result.len;
         i3++) {
      if (test_Q4_returns_minimum_rating_and_title_for_sequels_result
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
  Info_typeItem tmp4_data[] = {(Info_typeItem){.id = 1, .info = "rating"},
                               (Info_typeItem){.id = 2, .info = "other"}};
  list_Info_typeItem tmp4 = {2, tmp4_data};
  list_Info_typeItem info_type = tmp4;
  KeywordItem tmp5_data[] = {(KeywordItem){.id = 1, .keyword = "great sequel"},
                             (KeywordItem){.id = 2, .keyword = "prequel"}};
  list_KeywordItem tmp5 = {2, tmp5_data};
  list_KeywordItem keyword = tmp5;
  TitleItem tmp6_data[] = {
      (TitleItem){.id = 10, .title = "Alpha Movie", .production_year = 2006},
      (TitleItem){.id = 20, .title = "Beta Film", .production_year = 2007},
      (TitleItem){.id = 30, .title = "Old Film", .production_year = 2004}};
  list_TitleItem tmp6 = {3, tmp6_data};
  list_TitleItem title = tmp6;
  Movie_keywordItem tmp7_data[] = {
      (Movie_keywordItem){.movie_id = 10, .keyword_id = 1},
      (Movie_keywordItem){.movie_id = 20, .keyword_id = 1},
      (Movie_keywordItem){.movie_id = 30, .keyword_id = 1}};
  list_Movie_keywordItem tmp7 = {3, tmp7_data};
  list_Movie_keywordItem movie_keyword = tmp7;
  Movie_info_idxItem tmp8_data[] = {
      (Movie_info_idxItem){.movie_id = 10, .info_type_id = 1, .info = "6.2"},
      (Movie_info_idxItem){.movie_id = 20, .info_type_id = 1, .info = "7.8"},
      (Movie_info_idxItem){.movie_id = 30, .info_type_id = 1, .info = "4.5"}};
  list_Movie_info_idxItem tmp8 = {3, tmp8_data};
  list_Movie_info_idxItem movie_info_idx = tmp8;
  list_RowsItem tmp9 =
      list_RowsItem_create(info_type.len * movie_info_idx.len * title.len *
                           movie_keyword.len * keyword.len);
  int tmp10 = 0;
  for (int tmp11 = 0; tmp11 < info_type.len; tmp11++) {
    Info_typeItem it = info_type.data[tmp11];
    for (int tmp12 = 0; tmp12 < movie_info_idx.len; tmp12++) {
      Movie_info_idxItem mi = movie_info_idx.data[tmp12];
      if (!(it.id == mi.info_type_id)) {
        continue;
      }
      for (int tmp13 = 0; tmp13 < title.len; tmp13++) {
        TitleItem t = title.data[tmp13];
        if (!(t.id == mi.movie_id)) {
          continue;
        }
        for (int tmp14 = 0; tmp14 < movie_keyword.len; tmp14++) {
          Movie_keywordItem mk = movie_keyword.data[tmp14];
          if (!(mk.movie_id == t.id)) {
            continue;
          }
          for (int tmp15 = 0; tmp15 < keyword.len; tmp15++) {
            KeywordItem k = keyword.data[tmp15];
            if (!(k.id == mk.keyword_id)) {
              continue;
            }
            if (!((strcmp(it.info, "rating") == 0) &&
                  contains_string(k.keyword, "sequel") && mi.info > "5.0" &&
                  t.production_year > 2005 && mk.movie_id == mi.movie_id)) {
              continue;
            }
            tmp9.data[tmp10] = (RowsItem){.rating = mi.info, .title = t.title};
            tmp10++;
          }
        }
      }
    }
  }
  tmp9.len = tmp10;
  list_RowsItem rows = tmp9;
  list_string tmp17 = list_string_create(rows.len);
  int tmp18 = 0;
  for (int tmp19 = 0; tmp19 < rows.len; tmp19++) {
    RowsItem r = rows.data[tmp19];
    tmp17.data[tmp18] = r.rating;
    tmp18++;
  }
  tmp17.len = tmp18;
  list_string tmp20 = list_string_create(rows.len);
  int tmp21 = 0;
  for (int tmp22 = 0; tmp22 < rows.len; tmp22++) {
    RowsItem r = rows.data[tmp22];
    tmp20.data[tmp21] = r.title;
    tmp21++;
  }
  tmp20.len = tmp21;
  ResultItem tmp16_data[] = {(ResultItem){.rating = _min_string(tmp17),
                                          .movie_title = _min_string(tmp20)}};
  list_ResultItem tmp16 = {1, tmp16_data};
  list_ResultItem result = tmp16;
  printf("[");
  for (int i23 = 0; i23 < result.len; i23++) {
    if (i23 > 0)
      printf(",");
    ResultItem it = result.data[i23];
    printf("{");
    _json_string("rating");
    printf(":");
    _json_string(it.rating);
    printf(",");
    _json_string("movie_title");
    printf(":");
    _json_string(it.movie_title);
    printf("}");
  }
  printf("]");
  test_Q4_returns_minimum_rating_and_title_for_sequels_result = result;
  test_Q4_returns_minimum_rating_and_title_for_sequels();
  return 0;
}
