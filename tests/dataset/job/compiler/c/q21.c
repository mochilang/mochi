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
  const char *company_name;
  const char *link_type;
  const char *western_follow_up;
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
  const char *name;
  const char *country_code;
} company_name_t;
typedef struct {
  int len;
  company_name_t *data;
} company_name_list_t;
company_name_list_t create_company_name_list(int len) {
  company_name_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(company_name_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  const char *kind;
} company_type_t;
typedef struct {
  int len;
  company_type_t *data;
} company_type_list_t;
company_type_list_t create_company_type_list(int len) {
  company_type_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(company_type_t));
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
  int movie_id;
  int company_id;
  int company_type_id;
  int note;
} movie_companie_t;
typedef struct {
  int len;
  movie_companie_t *data;
} movie_companie_list_t;
movie_companie_list_t create_movie_companie_list(int len) {
  movie_companie_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(movie_companie_t));
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
  int movie_id;
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
  const char *company_name;
  const char *link_type;
  const char *western_follow_up;
} rows_item_t;
typedef struct {
  int len;
  rows_item_t *data;
} rows_item_list_t;
rows_item_list_t create_rows_item_list(int len) {
  rows_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(rows_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  const char *company_name;
  const char *link_type;
  const char *western_follow_up;
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

static list_int test_Q21_finds_western_follow_up_sequels_result;
static void test_Q21_finds_western_follow_up_sequels() {
  tmp1_t tmp1[] = {(tmp1_t){.company_name = "ACME Film Works",
                            .link_type = "is follow up",
                            .western_follow_up = "Western Return"}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_Q21_finds_western_follow_up_sequels_result.len != tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0; i3 < test_Q21_finds_western_follow_up_sequels_result.len;
         i3++) {
      if (test_Q21_finds_western_follow_up_sequels_result.data[i3] !=
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
  company_name_t company_name[] = {
      (company_name_t){
          .id = 1, .name = "ACME Film Works", .country_code = "[us]"},
      (company_name_t){
          .id = 2, .name = "Polish Warner", .country_code = "[pl]"}};
  int company_name_len = sizeof(company_name) / sizeof(company_name[0]);
  company_type_t company_type[] = {
      (company_type_t){.id = 1, .kind = "production companies"},
      (company_type_t){.id = 2, .kind = "other"}};
  int company_type_len = sizeof(company_type) / sizeof(company_type[0]);
  keyword_t keyword[] = {(keyword_t){.id = 1, .keyword = "sequel"},
                         (keyword_t){.id = 2, .keyword = "drama"}};
  int keyword_len = sizeof(keyword) / sizeof(keyword[0]);
  link_type_t link_type[] = {(link_type_t){.id = 1, .link = "is follow up"},
                             (link_type_t){.id = 2, .link = "references"}};
  int link_type_len = sizeof(link_type) / sizeof(link_type[0]);
  title_t title[] = {
      (title_t){.id = 10, .title = "Western Return", .production_year = 1975},
      (title_t){.id = 20, .title = "Other Movie", .production_year = 2015}};
  int title_len = sizeof(title) / sizeof(title[0]);
  movie_companie_t movie_companies[] = {
      (movie_companie_t){
          .movie_id = 10, .company_id = 1, .company_type_id = 1, .note = 0},
      (movie_companie_t){
          .movie_id = 20, .company_id = 2, .company_type_id = 1, .note = 0}};
  int movie_companies_len =
      sizeof(movie_companies) / sizeof(movie_companies[0]);
  movie_info_t movie_info[] = {(movie_info_t){.movie_id = 10, .info = "Sweden"},
                               (movie_info_t){.movie_id = 20, .info = "USA"}};
  int movie_info_len = sizeof(movie_info) / sizeof(movie_info[0]);
  movie_keyword_t movie_keyword[] = {
      (movie_keyword_t){.movie_id = 10, .keyword_id = 1},
      (movie_keyword_t){.movie_id = 20, .keyword_id = 2}};
  int movie_keyword_len = sizeof(movie_keyword) / sizeof(movie_keyword[0]);
  movie_link_t movie_link[] = {
      (movie_link_t){.movie_id = 10, .link_type_id = 1},
      (movie_link_t){.movie_id = 20, .link_type_id = 2}};
  int movie_link_len = sizeof(movie_link) / sizeof(movie_link[0]);
  list_string allowed_countries = list_string_create(8);
  allowed_countries.data[0] = "Sweden";
  allowed_countries.data[1] = "Norway";
  allowed_countries.data[2] = "Germany";
  allowed_countries.data[3] = "Denmark";
  allowed_countries.data[4] = "Swedish";
  allowed_countries.data[5] = "Denish";
  allowed_countries.data[6] = "Norwegian";
  allowed_countries.data[7] = "German";
  int allowed_countries = allowed_countries;
  rows_item_list_t tmp4 = rows_item_list_t_create(
      company_name.len * movie_companies.len * company_type.len * title.len *
      movie_keyword.len * keyword.len * movie_link.len * link_type.len *
      movie_info.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < company_name_len; tmp6++) {
    company_name_t cn = company_name[tmp6];
    for (int tmp7 = 0; tmp7 < movie_companies_len; tmp7++) {
      movie_companie_t mc = movie_companies[tmp7];
      if (!(mc.company_id == cn.id)) {
        continue;
      }
      for (int tmp8 = 0; tmp8 < company_type_len; tmp8++) {
        company_type_t ct = company_type[tmp8];
        if (!(ct.id == mc.company_type_id)) {
          continue;
        }
        for (int tmp9 = 0; tmp9 < title_len; tmp9++) {
          title_t t = title[tmp9];
          if (!(t.id == mc.movie_id)) {
            continue;
          }
          for (int tmp10 = 0; tmp10 < movie_keyword_len; tmp10++) {
            movie_keyword_t mk = movie_keyword[tmp10];
            if (!(mk.movie_id == t.id)) {
              continue;
            }
            for (int tmp11 = 0; tmp11 < keyword_len; tmp11++) {
              keyword_t k = keyword[tmp11];
              if (!(k.id == mk.keyword_id)) {
                continue;
              }
              for (int tmp12 = 0; tmp12 < movie_link_len; tmp12++) {
                movie_link_t ml = movie_link[tmp12];
                if (!(ml.movie_id == t.id)) {
                  continue;
                }
                for (int tmp13 = 0; tmp13 < link_type_len; tmp13++) {
                  link_type_t lt = link_type[tmp13];
                  if (!(lt.id == ml.link_type_id)) {
                    continue;
                  }
                  for (int tmp14 = 0; tmp14 < movie_info_len; tmp14++) {
                    movie_info_t mi = movie_info[tmp14];
                    if (!(mi.movie_id == t.id)) {
                      continue;
                    }
                    if (!((strcmp(cn.country_code, "[pl]") != 0) &&
                          (contains_string(cn.name, "Film") ||
                           contains_string(cn.name, "Warner")) &&
                          ct.kind == "production companies" &&
                          k.keyword == "sequel" &&
                          contains_string(lt.link, "follow") && mc.note == 0 &&
                          (contains_list_string(allowed_countries, mi.info)) &&
                          t.production_year >= 1950 &&
                          t.production_year <= 2000)) {
                      continue;
                    }
                    tmp4.data[tmp5] =
                        (rows_item_t){.company_name = cn.name,
                                      .link_type = lt.link,
                                      .western_follow_up = t.title};
                    tmp5++;
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  tmp4.len = tmp5;
  rows_item_list_t rows = tmp4;
  int tmp15 = int_create(rows.len);
  int tmp16 = 0;
  for (int tmp17 = 0; tmp17 < rows.len; tmp17++) {
    rows_item_t r = rows.data[tmp17];
    tmp15.data[tmp16] = r.company_name;
    tmp16++;
  }
  tmp15.len = tmp16;
  int tmp18 = int_create(rows.len);
  int tmp19 = 0;
  for (int tmp20 = 0; tmp20 < rows.len; tmp20++) {
    rows_item_t r = rows.data[tmp20];
    tmp18.data[tmp19] = r.link_type;
    tmp19++;
  }
  tmp18.len = tmp19;
  int tmp21 = int_create(rows.len);
  int tmp22 = 0;
  for (int tmp23 = 0; tmp23 < rows.len; tmp23++) {
    rows_item_t r = rows.data[tmp23];
    tmp21.data[tmp22] = r.western_follow_up;
    tmp22++;
  }
  tmp21.len = tmp22;
  result_t result[] = {(result_t){.company_name = _min_string(tmp15),
                                  .link_type = _min_string(tmp18),
                                  .western_follow_up = _min_string(tmp21)}};
  int result_len = sizeof(result) / sizeof(result[0]);
  printf("[");
  for (int i24 = 0; i24 < result_len; i24++) {
    if (i24 > 0)
      printf(",");
    result_t it = result[i24];
    printf("{");
    _json_string("company_name");
    printf(":");
    _json_string(it.company_name);
    printf(",");
    _json_string("link_type");
    printf(":");
    _json_string(it.link_type);
    printf(",");
    _json_string("western_follow_up");
    printf(":");
    _json_string(it.western_follow_up);
    printf("}");
  }
  printf("]");
  test_Q21_finds_western_follow_up_sequels_result = result;
  test_Q21_finds_western_follow_up_sequels();
  free(tmp15.data);
  free(tmp18.data);
  free(tmp21.data);
  return 0;
}
