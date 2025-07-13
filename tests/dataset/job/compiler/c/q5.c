
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
  char *typical_european_movie;
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
  int ct_id;
  char *kind;
} Company_typeItem;
typedef struct {
  int len;
  Company_typeItem *data;
} list_Company_typeItem;
static list_Company_typeItem list_Company_typeItem_create(int len) {
  list_Company_typeItem l;
  l.len = len;
  l.data = calloc(len, sizeof(Company_typeItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int it_id;
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
  int t_id;
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
  int company_type_id;
  char *note;
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
  char *info;
  int info_type_id;
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
  char *typical_european_movie;
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

static list_int
    test_Q5_finds_the_lexicographically_first_qualifying_title_result;
static void test_Q5_finds_the_lexicographically_first_qualifying_title() {
  list_int tmp1 = list_int_create(1);
  tmp1.data[0] = (TmpItem){.typical_european_movie = "A Film"};
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
  Company_typeItem tmp4_data[] = {
      (Company_typeItem){.ct_id = 1, .kind = "production companies"},
      (Company_typeItem){.ct_id = 2, .kind = "other"}};
  list_Company_typeItem tmp4 = {2, tmp4_data};
  list_Company_typeItem company_type = tmp4;
  Info_typeItem tmp5_data[] = {
      (Info_typeItem){.it_id = 10, .info = "languages"}};
  list_Info_typeItem tmp5 = {1, tmp5_data};
  list_Info_typeItem info_type = tmp5;
  TitleItem tmp6_data[] = {
      (TitleItem){.t_id = 100, .title = "B Movie", .production_year = 2010},
      (TitleItem){.t_id = 200, .title = "A Film", .production_year = 2012},
      (TitleItem){.t_id = 300, .title = "Old Movie", .production_year = 2000}};
  list_TitleItem tmp6 = {3, tmp6_data};
  list_TitleItem title = tmp6;
  Movie_companiesItem tmp7_data[] = {
      (Movie_companiesItem){.movie_id = 100,
                            .company_type_id = 1,
                            .note = "ACME (France) (theatrical)"},
      (Movie_companiesItem){.movie_id = 200,
                            .company_type_id = 1,
                            .note = "ACME (France) (theatrical)"},
      (Movie_companiesItem){.movie_id = 300,
                            .company_type_id = 1,
                            .note = "ACME (France) (theatrical)"}};
  list_Movie_companiesItem tmp7 = {3, tmp7_data};
  list_Movie_companiesItem movie_companies = tmp7;
  Movie_infoItem tmp8_data[] = {
      (Movie_infoItem){.movie_id = 100, .info = "German", .info_type_id = 10},
      (Movie_infoItem){.movie_id = 200, .info = "Swedish", .info_type_id = 10},
      (Movie_infoItem){.movie_id = 300, .info = "German", .info_type_id = 10}};
  list_Movie_infoItem tmp8 = {3, tmp8_data};
  list_Movie_infoItem movie_info = tmp8;
  list_string tmp9 = list_string_create(8);
  tmp9.data[0] = "Sweden";
  tmp9.data[1] = "Norway";
  tmp9.data[2] = "Germany";
  tmp9.data[3] = "Denmark";
  tmp9.data[4] = "Swedish";
  tmp9.data[5] = "Denish";
  tmp9.data[6] = "Norwegian";
  tmp9.data[7] = "German";
  list_string tmp10 =
      list_string_create(company_type.len * movie_companies.len *
                         movie_info.len * info_type.len * title.len);
  int tmp11 = 0;
  for (int tmp12 = 0; tmp12 < company_type.len; tmp12++) {
    Company_typeItem ct = company_type.data[tmp12];
    for (int tmp13 = 0; tmp13 < movie_companies.len; tmp13++) {
      Movie_companiesItem mc = movie_companies.data[tmp13];
      if (!(mc.company_type_id == ct.ct_id)) {
        continue;
      }
      for (int tmp14 = 0; tmp14 < movie_info.len; tmp14++) {
        Movie_infoItem mi = movie_info.data[tmp14];
        if (!(mi.movie_id == mc.movie_id)) {
          continue;
        }
        for (int tmp15 = 0; tmp15 < info_type.len; tmp15++) {
          Info_typeItem it = info_type.data[tmp15];
          if (!(it.it_id == mi.info_type_id)) {
            continue;
          }
          for (int tmp16 = 0; tmp16 < title.len; tmp16++) {
            TitleItem t = title.data[tmp16];
            if (!(t.t_id == mc.movie_id)) {
              continue;
            }
            if (!((strcmp(ct.kind, "production companies") == 0) &&
                  "(theatrical)" in mc.note && "(France)" in mc.note &&
                  t.production_year > 2005 &&
                  (contains_list_string(tmp9, mi.info)))) {
              continue;
            }
            tmp10.data[tmp11] = t.title;
            tmp11++;
          }
        }
      }
    }
  }
  tmp10.len = tmp11;
  list_string candidate_titles = tmp10;
  ResultItem tmp17_data[] = {
      (ResultItem){.typical_european_movie = _min_string(candidate_titles)}};
  list_ResultItem tmp17 = {1, tmp17_data};
  list_ResultItem result = tmp17;
  printf("[");
  for (int i18 = 0; i18 < result.len; i18++) {
    if (i18 > 0)
      printf(",");
    ResultItem it = result.data[i18];
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
