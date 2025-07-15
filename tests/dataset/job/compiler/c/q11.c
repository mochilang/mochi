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
  const char *from_company;
  const char *movie_link_type;
  const char *non_polish_sequel_movie;
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
  int id;
  int production_year;
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
  const char *company;
  const char *link;
  const char *title;
} matches_item_t;
typedef struct {
  int len;
  matches_item_t *data;
} matches_item_list_t;
matches_item_list_t create_matches_item_list(int len) {
  matches_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(matches_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  const char *from_company;
  const char *movie_link_type;
  const char *non_polish_sequel_movie;
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

static list_int test_Q11_returns_min_company__link_type_and_title_result;
static void test_Q11_returns_min_company__link_type_and_title() {
  tmp1_t tmp1[] = {(tmp1_t){.from_company = "Best Film Co",
                            .movie_link_type = "follow-up",
                            .non_polish_sequel_movie = "Alpha"}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_Q11_returns_min_company__link_type_and_title_result.len !=
      tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0;
         i3 < test_Q11_returns_min_company__link_type_and_title_result.len;
         i3++) {
      if (test_Q11_returns_min_company__link_type_and_title_result.data[i3] !=
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
      (company_name_t){.id = 1, .name = "Best Film Co", .country_code = "[us]"},
      (company_name_t){
          .id = 2, .name = "Warner Studios", .country_code = "[de]"},
      (company_name_t){
          .id = 3, .name = "Polish Films", .country_code = "[pl]"}};
  int company_name_len = sizeof(company_name) / sizeof(company_name[0]);
  company_type_t company_type[] = {
      (company_type_t){.id = 1, .kind = "production companies"},
      (company_type_t){.id = 2, .kind = "distributors"}};
  int company_type_len = sizeof(company_type) / sizeof(company_type[0]);
  keyword_t keyword[] = {(keyword_t){.id = 1, .keyword = "sequel"},
                         (keyword_t){.id = 2, .keyword = "thriller"}};
  int keyword_len = sizeof(keyword) / sizeof(keyword[0]);
  link_type_t link_type[] = {(link_type_t){.id = 1, .link = "follow-up"},
                             (link_type_t){.id = 2, .link = "follows from"},
                             (link_type_t){.id = 3, .link = "remake"}};
  int link_type_len = sizeof(link_type) / sizeof(link_type[0]);
  movie_companie_t movie_companies[] = {
      (movie_companie_t){
          .movie_id = 10, .company_id = 1, .company_type_id = 1, .note = 0},
      (movie_companie_t){
          .movie_id = 20, .company_id = 2, .company_type_id = 1, .note = 0},
      (movie_companie_t){
          .movie_id = 30, .company_id = 3, .company_type_id = 1, .note = 0}};
  int movie_companies_len =
      sizeof(movie_companies) / sizeof(movie_companies[0]);
  movie_keyword_t movie_keyword[] = {
      (movie_keyword_t){.movie_id = 10, .keyword_id = 1},
      (movie_keyword_t){.movie_id = 20, .keyword_id = 1},
      (movie_keyword_t){.movie_id = 20, .keyword_id = 2},
      (movie_keyword_t){.movie_id = 30, .keyword_id = 1}};
  int movie_keyword_len = sizeof(movie_keyword) / sizeof(movie_keyword[0]);
  movie_link_t movie_link[] = {
      (movie_link_t){.movie_id = 10, .link_type_id = 1},
      (movie_link_t){.movie_id = 20, .link_type_id = 2},
      (movie_link_t){.movie_id = 30, .link_type_id = 3}};
  int movie_link_len = sizeof(movie_link) / sizeof(movie_link[0]);
  title_t title[] = {
      (title_t){.id = 10, .production_year = 1960, .title = "Alpha"},
      (title_t){.id = 20, .production_year = 1970, .title = "Beta"},
      (title_t){.id = 30, .production_year = 1985, .title = "Polish Movie"}};
  int title_len = sizeof(title) / sizeof(title[0]);
  matches_item_list_t tmp4 = matches_item_list_t_create(
      company_name.len * movie_companies.len * company_type.len * title.len *
      movie_keyword.len * keyword.len * movie_link.len * link_type.len);
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
                  if (!((strcmp(cn.country_code, "[pl]") != 0) &&
                        (contains_string(cn.name, "Film") ||
                         contains_string(cn.name, "Warner")) &&
                        ct.kind == "production companies" &&
                        k.keyword == "sequel" &&
                        contains_string(lt.link, "follow") && mc.note == 0 &&
                        t.production_year >= 1950 &&
                        t.production_year <= 2000 &&
                        ml.movie_id == mk.movie_id &&
                        ml.movie_id == mc.movie_id &&
                        mk.movie_id == mc.movie_id)) {
                    continue;
                  }
                  tmp4.data[tmp5] = (matches_item_t){
                      .company = cn.name, .link = lt.link, .title = t.title};
                  tmp5++;
                }
              }
            }
          }
        }
      }
    }
  }
  tmp4.len = tmp5;
  matches_item_list_t matches = tmp4;
  int tmp14 = int_create(matches.len);
  int tmp15 = 0;
  for (int tmp16 = 0; tmp16 < matches.len; tmp16++) {
    matches_item_t x = matches.data[tmp16];
    tmp14.data[tmp15] = x.company;
    tmp15++;
  }
  tmp14.len = tmp15;
  int tmp17 = int_create(matches.len);
  int tmp18 = 0;
  for (int tmp19 = 0; tmp19 < matches.len; tmp19++) {
    matches_item_t x = matches.data[tmp19];
    tmp17.data[tmp18] = x.link;
    tmp18++;
  }
  tmp17.len = tmp18;
  int tmp20 = int_create(matches.len);
  int tmp21 = 0;
  for (int tmp22 = 0; tmp22 < matches.len; tmp22++) {
    matches_item_t x = matches.data[tmp22];
    tmp20.data[tmp21] = x.title;
    tmp21++;
  }
  tmp20.len = tmp21;
  result_t result[] = {
      (result_t){.from_company = _min_string(tmp14),
                 .movie_link_type = _min_string(tmp17),
                 .non_polish_sequel_movie = _min_string(tmp20)}};
  int result_len = sizeof(result) / sizeof(result[0]);
  printf("[");
  for (int i23 = 0; i23 < result_len; i23++) {
    if (i23 > 0)
      printf(",");
    result_t it = result[i23];
    printf("{");
    _json_string("from_company");
    printf(":");
    _json_string(it.from_company);
    printf(",");
    _json_string("movie_link_type");
    printf(":");
    _json_string(it.movie_link_type);
    printf(",");
    _json_string("non_polish_sequel_movie");
    printf(":");
    _json_string(it.non_polish_sequel_movie);
    printf("}");
  }
  printf("]");
  test_Q11_returns_min_company__link_type_and_title_result = result;
  test_Q11_returns_min_company__link_type_and_title();
  free(tmp14.data);
  free(tmp17.data);
  free(tmp20.data);
  return 0;
}
