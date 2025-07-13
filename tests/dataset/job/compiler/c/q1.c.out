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
static int _min_int(list_int v) {
  if (v.len == 0)
    return 0;
  int m = v.data[0];
  for (int i = 1; i < v.len; i++)
    if (v.data[i] < m)
      m = v.data[i];
  return m;
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
  char *production_note;
  char *movie_title;
  int movie_year;
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
  int info_type_id;
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
  char *note;
  char *title;
  int year;
} FilteredItem;
typedef struct {
  int len;
  FilteredItem *data;
} list_FilteredItem;
static list_FilteredItem list_FilteredItem_create(int len) {
  list_FilteredItem l;
  l.len = len;
  l.data = calloc(len, sizeof(FilteredItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int production_note;
  int movie_title;
  int movie_year;
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

static int
    test_Q1_returns_min_note__title_and_year_for_top_ranked_co_production_result;
static void
test_Q1_returns_min_note__title_and_year_for_top_ranked_co_production() {
  if (!(test_Q1_returns_min_note__title_and_year_for_top_ranked_co_production_result ==
        (TmpItem){.production_note = "ACME (co-production)",
                  .movie_title = "Good Movie",
                  .movie_year = 1995})) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  Company_typeItem tmp1_data[] = {
      (Company_typeItem){.id = 1, .kind = "production companies"},
      (Company_typeItem){.id = 2, .kind = "distributors"}};
  list_Company_typeItem tmp1 = {2, tmp1_data};
  list_Company_typeItem company_type = tmp1;
  Info_typeItem tmp2_data[] = {
      (Info_typeItem){.id = 10, .info = "top 250 rank"},
      (Info_typeItem){.id = 20, .info = "bottom 10 rank"}};
  list_Info_typeItem tmp2 = {2, tmp2_data};
  list_Info_typeItem info_type = tmp2;
  TitleItem tmp3_data[] = {
      (TitleItem){.id = 100, .title = "Good Movie", .production_year = 1995},
      (TitleItem){.id = 200, .title = "Bad Movie", .production_year = 2000}};
  list_TitleItem tmp3 = {2, tmp3_data};
  list_TitleItem title = tmp3;
  Movie_companiesItem tmp4_data[] = {
      (Movie_companiesItem){.movie_id = 100,
                            .company_type_id = 1,
                            .note = "ACME (co-production)"},
      (Movie_companiesItem){.movie_id = 200,
                            .company_type_id = 1,
                            .note = "MGM (as Metro-Goldwyn-Mayer Pictures)"}};
  list_Movie_companiesItem tmp4 = {2, tmp4_data};
  list_Movie_companiesItem movie_companies = tmp4;
  Movie_info_idxItem tmp5_data[] = {
      (Movie_info_idxItem){.movie_id = 100, .info_type_id = 10},
      (Movie_info_idxItem){.movie_id = 200, .info_type_id = 20}};
  list_Movie_info_idxItem tmp5 = {2, tmp5_data};
  list_Movie_info_idxItem movie_info_idx = tmp5;
  list_FilteredItem tmp6 =
      list_FilteredItem_create(company_type.len * movie_companies.len *
                               title.len * movie_info_idx.len * info_type.len);
  int tmp7 = 0;
  for (int tmp8 = 0; tmp8 < company_type.len; tmp8++) {
    Company_typeItem ct = company_type.data[tmp8];
    for (int tmp9 = 0; tmp9 < movie_companies.len; tmp9++) {
      Movie_companiesItem mc = movie_companies.data[tmp9];
      if (!(ct.id == mc.company_type_id)) {
        continue;
      }
      for (int tmp10 = 0; tmp10 < title.len; tmp10++) {
        TitleItem t = title.data[tmp10];
        if (!(t.id == mc.movie_id)) {
          continue;
        }
        for (int tmp11 = 0; tmp11 < movie_info_idx.len; tmp11++) {
          Movie_info_idxItem mi = movie_info_idx.data[tmp11];
          if (!(mi.movie_id == t.id)) {
            continue;
          }
          for (int tmp12 = 0; tmp12 < info_type.len; tmp12++) {
            Info_typeItem it = info_type.data[tmp12];
            if (!(it.id == mi.info_type_id)) {
              continue;
            }
            if (!((strcmp(ct.kind, "production companies") == 0) &&
                  it.info == "top 250 rank" &&
                  ((!contains_string(mc.note,
                                     "(as Metro-Goldwyn-Mayer Pictures)"))) &&
                  (contains_string(mc.note, "(co-production)") ||
                   contains_string(mc.note, "(presents)")))) {
              continue;
            }
            tmp6.data[tmp7] = (FilteredItem){
                .note = mc.note, .title = t.title, .year = t.production_year};
            tmp7++;
          }
        }
      }
    }
  }
  tmp6.len = tmp7;
  list_FilteredItem filtered = tmp6;
  list_string tmp13 = list_string_create(filtered.len);
  int tmp14 = 0;
  for (int tmp15 = 0; tmp15 < filtered.len; tmp15++) {
    FilteredItem r = filtered.data[tmp15];
    tmp13.data[tmp14] = r.note;
    tmp14++;
  }
  tmp13.len = tmp14;
  list_string tmp16 = list_string_create(filtered.len);
  int tmp17 = 0;
  for (int tmp18 = 0; tmp18 < filtered.len; tmp18++) {
    FilteredItem r = filtered.data[tmp18];
    tmp16.data[tmp17] = r.title;
    tmp17++;
  }
  tmp16.len = tmp17;
  list_int tmp19 = list_int_create(filtered.len);
  int tmp20 = 0;
  for (int tmp21 = 0; tmp21 < filtered.len; tmp21++) {
    FilteredItem r = filtered.data[tmp21];
    tmp19.data[tmp20] = r.year;
    tmp20++;
  }
  tmp19.len = tmp20;
  ResultItem result = (ResultItem){.production_note = _min_string(tmp13),
                                   .movie_title = _min_string(tmp16),
                                   .movie_year = _min_int(tmp19)};
  list_int tmp22 = list_int_create(1);
  tmp22.data[0] = result;
  printf("[");
  for (int i23 = 0; i23 < 1; i23++) {
    if (i23 > 0)
      printf(",");
    ResultItem it = tmp22.data[i23];
    printf("{");
    _json_string("production_note");
    printf(":");
    _json_int(it.production_note);
    printf(",");
    _json_string("movie_title");
    printf(":");
    _json_int(it.movie_title);
    printf(",");
    _json_string("movie_year");
    printf(":");
    _json_int(it.movie_year);
    printf("}");
  }
  printf("]");
  test_Q1_returns_min_note__title_and_year_for_top_ranked_co_production_result =
      result;
  test_Q1_returns_min_note__title_and_year_for_top_ranked_co_production();
  return 0;
}
