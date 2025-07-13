
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
  char *country_code;
} Company_nameItem;
typedef struct {
  int len;
  Company_nameItem *data;
} list_Company_nameItem;
static list_Company_nameItem list_Company_nameItem_create(int len) {
  list_Company_nameItem l;
  l.len = len;
  l.data = calloc(len, sizeof(Company_nameItem));
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
  int company_id;
} Movie_companiesItem;
typedef struct {
  int len;
  Movie_companiesItem *data;
} list_Movie_companiesItem;
static list_Movie_companiesItem list_Movie_companiesItem_create(int len) {
  list_Movie_companiesItem l;
  l.len = len;
  l.data = calloc(len, sizeof(Movie_companiesItem));
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
  Company_nameItem tmp1_data[] = {
      (Company_nameItem){.id = 1, .country_code = "[de]"},
      (Company_nameItem){.id = 2, .country_code = "[us]"}};
  list_Company_nameItem tmp1 = {2, tmp1_data};
  list_Company_nameItem company_name = tmp1;
  KeywordItem tmp2_data[] = {
      (KeywordItem){.id = 1, .keyword = "character-name-in-title"},
      (KeywordItem){.id = 2, .keyword = "other"}};
  list_KeywordItem tmp2 = {2, tmp2_data};
  list_KeywordItem keyword = tmp2;
  Movie_companiesItem tmp3_data[] = {
      (Movie_companiesItem){.movie_id = 100, .company_id = 1},
      (Movie_companiesItem){.movie_id = 200, .company_id = 2}};
  list_Movie_companiesItem tmp3 = {2, tmp3_data};
  list_Movie_companiesItem movie_companies = tmp3;
  Movie_keywordItem tmp4_data[] = {
      (Movie_keywordItem){.movie_id = 100, .keyword_id = 1},
      (Movie_keywordItem){.movie_id = 200, .keyword_id = 2}};
  list_Movie_keywordItem tmp4 = {2, tmp4_data};
  list_Movie_keywordItem movie_keyword = tmp4;
  TitleItem tmp5_data[] = {(TitleItem){.id = 100, .title = "Der Film"},
                           (TitleItem){.id = 200, .title = "Other Movie"}};
  list_TitleItem tmp5 = {2, tmp5_data};
  list_TitleItem title = tmp5;
  list_string tmp6 =
      list_string_create(company_name.len * movie_companies.len * title.len *
                         movie_keyword.len * keyword.len);
  int tmp7 = 0;
  for (int tmp8 = 0; tmp8 < company_name.len; tmp8++) {
    Company_nameItem cn = company_name.data[tmp8];
    for (int tmp9 = 0; tmp9 < movie_companies.len; tmp9++) {
      Movie_companiesItem mc = movie_companies.data[tmp9];
      if (!(mc.company_id == cn.id)) {
        continue;
      }
      for (int tmp10 = 0; tmp10 < title.len; tmp10++) {
        TitleItem t = title.data[tmp10];
        if (!(mc.movie_id == t.id)) {
          continue;
        }
        for (int tmp11 = 0; tmp11 < movie_keyword.len; tmp11++) {
          Movie_keywordItem mk = movie_keyword.data[tmp11];
          if (!(mk.movie_id == t.id)) {
            continue;
          }
          for (int tmp12 = 0; tmp12 < keyword.len; tmp12++) {
            KeywordItem k = keyword.data[tmp12];
            if (!(mk.keyword_id == k.id)) {
              continue;
            }
            if (!((strcmp(cn.country_code, "[de]") == 0) &&
                  k.keyword == "character-name-in-title" &&
                  mc.movie_id == mk.movie_id)) {
              continue;
            }
            tmp6.data[tmp7] = t.title;
            tmp7++;
          }
        }
      }
    }
  }
  tmp6.len = tmp7;
  list_string titles = tmp6;
  int result = _min_string(titles);
  _json_int(result);
  test_Q2_finds_earliest_title_for_German_companies_with_character_keyword_result =
      result;
  test_Q2_finds_earliest_title_for_German_companies_with_character_keyword();
  return 0;
}
