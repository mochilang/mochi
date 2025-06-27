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
  l.data = (int *)malloc(sizeof(int) * len);
  return l;
}
typedef struct {
  int len;
  double *data;
} list_float;
static list_float list_float_create(int len) {
  list_float l;
  l.len = len;
  l.data = (double *)malloc(sizeof(double) * len);
  return l;
}
typedef struct {
  int len;
  char **data;
} list_string;
static list_string list_string_create(int len) {
  list_string l;
  l.len = len;
  l.data = (char **)malloc(sizeof(char *) * len);
  return l;
}
typedef struct {
  int len;
  list_int *data;
} list_list_int;
static list_list_int list_list_int_create(int len) {
  list_list_int l;
  l.len = len;
  l.data = (list_int *)malloc(sizeof(list_int) * len);
  return l;
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
static void
test_Q2_finds_earliest_title_for_German_companies_with_character_keyword() {
  if (!((result == "Der Film"))) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  typedef struct {
    int id;
    char *country_code;
  } company_nameItem;
  typedef struct {
    int len;
    company_nameItem *data;
  } list_company_nameItem;
  static list_company_nameItem list_company_nameItem_create(int len) {
    list_company_nameItem l;
    l.len = len;
    l.data = (company_nameItem *)malloc(sizeof(company_nameItem) * len);
    return l;
  }
  list_company_nameItem _t1 = list_company_nameItem_create(2);
  _t1.data[0] = (company_nameItem){.id = 1, .country_code = "[de]"};
  _t1.data[1] = (company_nameItem){.id = 2, .country_code = "[us]"};
  int company_name = _t1;
  typedef struct {
    int id;
    char *keyword;
  } keywordItem;
  typedef struct {
    int len;
    keywordItem *data;
  } list_keywordItem;
  static list_keywordItem list_keywordItem_create(int len) {
    list_keywordItem l;
    l.len = len;
    l.data = (keywordItem *)malloc(sizeof(keywordItem) * len);
    return l;
  }
  list_keywordItem _t2 = list_keywordItem_create(2);
  _t2.data[0] = (keywordItem){.id = 1, .keyword = "character-name-in-title"};
  _t2.data[1] = (keywordItem){.id = 2, .keyword = "other"};
  int keyword = _t2;
  typedef struct {
    int movie_id;
    int company_id;
  } movie_companiesItem;
  typedef struct {
    int len;
    movie_companiesItem *data;
  } list_movie_companiesItem;
  static list_movie_companiesItem list_movie_companiesItem_create(int len) {
    list_movie_companiesItem l;
    l.len = len;
    l.data = (movie_companiesItem *)malloc(sizeof(movie_companiesItem) * len);
    return l;
  }
  list_movie_companiesItem _t3 = list_movie_companiesItem_create(2);
  _t3.data[0] = (movie_companiesItem){.movie_id = 100, .company_id = 1};
  _t3.data[1] = (movie_companiesItem){.movie_id = 200, .company_id = 2};
  int movie_companies = _t3;
  typedef struct {
    int movie_id;
    int keyword_id;
  } movie_keywordItem;
  typedef struct {
    int len;
    movie_keywordItem *data;
  } list_movie_keywordItem;
  static list_movie_keywordItem list_movie_keywordItem_create(int len) {
    list_movie_keywordItem l;
    l.len = len;
    l.data = (movie_keywordItem *)malloc(sizeof(movie_keywordItem) * len);
    return l;
  }
  list_movie_keywordItem _t4 = list_movie_keywordItem_create(2);
  _t4.data[0] = (movie_keywordItem){.movie_id = 100, .keyword_id = 1};
  _t4.data[1] = (movie_keywordItem){.movie_id = 200, .keyword_id = 2};
  int movie_keyword = _t4;
  typedef struct {
    int id;
    char *title;
  } titleItem;
  typedef struct {
    int len;
    titleItem *data;
  } list_titleItem;
  static list_titleItem list_titleItem_create(int len) {
    list_titleItem l;
    l.len = len;
    l.data = (titleItem *)malloc(sizeof(titleItem) * len);
    return l;
  }
  list_titleItem _t5 = list_titleItem_create(2);
  _t5.data[0] = (titleItem){.id = 100, .title = "Der Film"};
  _t5.data[1] = (titleItem){.id = 200, .title = "Other Movie"};
  int title = _t5;
  list_string titles = 0;
  int result = min(titles);
  _json_int(result);
  test_Q2_finds_earliest_title_for_German_companies_with_character_keyword();
  return 0;
}
