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
  const char *producing_company;
  const char *link_type;
  const char *complete_western_sequel;
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
  const char *kind;
} comp_cast_type_t;
typedef struct {
  int len;
  comp_cast_type_t *data;
} comp_cast_type_list_t;
comp_cast_type_list_t create_comp_cast_type_list(int len) {
  comp_cast_type_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(comp_cast_type_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int movie_id;
  int subject_id;
  int status_id;
} complete_cast_t;
typedef struct {
  int len;
  complete_cast_t *data;
} complete_cast_list_t;
complete_cast_list_t create_complete_cast_list(int len) {
  complete_cast_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(complete_cast_t));
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
  int producing_company;
  int link_type;
  int complete_western_sequel;
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

static int test_Q27_selects_minimal_company__link_and_title_result;
static void test_Q27_selects_minimal_company__link_and_title() {
  if (!(test_Q27_selects_minimal_company__link_and_title_result ==
        (tmp_item_t){.producing_company = "Best Film",
                     .link_type = "follows",
                     .complete_western_sequel = "Western Sequel"})) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  comp_cast_type_t comp_cast_type[] = {
      (comp_cast_type_t){.id = 1, .kind = "cast"},
      (comp_cast_type_t){.id = 2, .kind = "crew"},
      (comp_cast_type_t){.id = 3, .kind = "complete"}};
  int comp_cast_type_len = sizeof(comp_cast_type) / sizeof(comp_cast_type[0]);
  complete_cast_t complete_cast[] = {
      (complete_cast_t){.movie_id = 1, .subject_id = 1, .status_id = 3},
      (complete_cast_t){.movie_id = 2, .subject_id = 2, .status_id = 3}};
  int complete_cast_len = sizeof(complete_cast) / sizeof(complete_cast[0]);
  company_name_t company_name[] = {
      (company_name_t){.id = 1, .name = "Best Film", .country_code = "[se]"},
      (company_name_t){.id = 2, .name = "Polish Film", .country_code = "[pl]"}};
  int company_name_len = sizeof(company_name) / sizeof(company_name[0]);
  company_type_t company_type[] = {
      (company_type_t){.id = 1, .kind = "production companies"},
      (company_type_t){.id = 2, .kind = "other"}};
  int company_type_len = sizeof(company_type) / sizeof(company_type[0]);
  keyword_t keyword[] = {(keyword_t){.id = 1, .keyword = "sequel"},
                         (keyword_t){.id = 2, .keyword = "remake"}};
  int keyword_len = sizeof(keyword) / sizeof(keyword[0]);
  link_type_t link_type[] = {(link_type_t){.id = 1, .link = "follows"},
                             (link_type_t){.id = 2, .link = "related"}};
  int link_type_len = sizeof(link_type) / sizeof(link_type[0]);
  movie_companie_t movie_companies[] = {
      (movie_companie_t){
          .movie_id = 1, .company_id = 1, .company_type_id = 1, .note = 0},
      (movie_companie_t){.movie_id = 2,
                         .company_id = 2,
                         .company_type_id = 1,
                         .note = "extra"}};
  int movie_companies_len =
      sizeof(movie_companies) / sizeof(movie_companies[0]);
  movie_info_t movie_info[] = {(movie_info_t){.movie_id = 1, .info = "Sweden"},
                               (movie_info_t){.movie_id = 2, .info = "USA"}};
  int movie_info_len = sizeof(movie_info) / sizeof(movie_info[0]);
  movie_keyword_t movie_keyword[] = {
      (movie_keyword_t){.movie_id = 1, .keyword_id = 1},
      (movie_keyword_t){.movie_id = 2, .keyword_id = 2}};
  int movie_keyword_len = sizeof(movie_keyword) / sizeof(movie_keyword[0]);
  movie_link_t movie_link[] = {
      (movie_link_t){.movie_id = 1, .link_type_id = 1},
      (movie_link_t){.movie_id = 2, .link_type_id = 2}};
  int movie_link_len = sizeof(movie_link) / sizeof(movie_link[0]);
  title_t title[] = {
      (title_t){.id = 1, .production_year = 1980, .title = "Western Sequel"},
      (title_t){.id = 2, .production_year = 1999, .title = "Another Movie"}};
  int title_len = sizeof(title) / sizeof(title[0]);
  matches_item_list_t tmp1 = matches_item_list_t_create(
      complete_cast.len * comp_cast_type.len * comp_cast_type.len * title.len *
      movie_link.len * link_type.len * movie_keyword.len * keyword.len *
      movie_companies.len * company_type.len * company_name.len *
      movie_info.len);
  int tmp2 = 0;
  for (int tmp3 = 0; tmp3 < complete_cast_len; tmp3++) {
    complete_cast_t cc = complete_cast[tmp3];
    for (int tmp4 = 0; tmp4 < comp_cast_type_len; tmp4++) {
      comp_cast_type_t cct1 = comp_cast_type[tmp4];
      if (!(cct1.id == cc.subject_id)) {
        continue;
      }
      for (int tmp5 = 0; tmp5 < comp_cast_type_len; tmp5++) {
        comp_cast_type_t cct2 = comp_cast_type[tmp5];
        if (!(cct2.id == cc.status_id)) {
          continue;
        }
        for (int tmp6 = 0; tmp6 < title_len; tmp6++) {
          title_t t = title[tmp6];
          if (!(t.id == cc.movie_id)) {
            continue;
          }
          for (int tmp7 = 0; tmp7 < movie_link_len; tmp7++) {
            movie_link_t ml = movie_link[tmp7];
            if (!(ml.movie_id == t.id)) {
              continue;
            }
            for (int tmp8 = 0; tmp8 < link_type_len; tmp8++) {
              link_type_t lt = link_type[tmp8];
              if (!(lt.id == ml.link_type_id)) {
                continue;
              }
              for (int tmp9 = 0; tmp9 < movie_keyword_len; tmp9++) {
                movie_keyword_t mk = movie_keyword[tmp9];
                if (!(mk.movie_id == t.id)) {
                  continue;
                }
                for (int tmp10 = 0; tmp10 < keyword_len; tmp10++) {
                  keyword_t k = keyword[tmp10];
                  if (!(k.id == mk.keyword_id)) {
                    continue;
                  }
                  for (int tmp11 = 0; tmp11 < movie_companies_len; tmp11++) {
                    movie_companie_t mc = movie_companies[tmp11];
                    if (!(mc.movie_id == t.id)) {
                      continue;
                    }
                    for (int tmp12 = 0; tmp12 < company_type_len; tmp12++) {
                      company_type_t ct = company_type[tmp12];
                      if (!(ct.id == mc.company_type_id)) {
                        continue;
                      }
                      for (int tmp13 = 0; tmp13 < company_name_len; tmp13++) {
                        company_name_t cn = company_name[tmp13];
                        if (!(cn.id == mc.company_id)) {
                          continue;
                        }
                        for (int tmp14 = 0; tmp14 < movie_info_len; tmp14++) {
                          movie_info_t mi = movie_info[tmp14];
                          if (!(mi.movie_id == t.id)) {
                            continue;
                          }
                          if (!((((strcmp(cct1.kind, "cast") == 0) ||
                                  cct1.kind == "crew") &&
                                 cct2.kind == "complete" &&
                                 cn.country_code != "[pl]" &&
                                 (contains_string(cn.name, "Film") ||
                                  contains_string(cn.name, "Warner")) &&
                                 ct.kind == "production companies" &&
                                 k.keyword == "sequel" &&
                                 contains_string(lt.link, "follow") &&
                                 mc.note == 0 &&
                                 ((strcmp(mi.info, "Sweden") == 0) ||
                                  mi.info == "Germany" ||
                                  mi.info == "Swedish" ||
                                  mi.info == "German") &&
                                 t.production_year >= 1950 &&
                                 t.production_year <= 2000 &&
                                 ml.movie_id == mk.movie_id &&
                                 ml.movie_id == mc.movie_id &&
                                 mk.movie_id == mc.movie_id &&
                                 ml.movie_id == mi.movie_id &&
                                 mk.movie_id == mi.movie_id &&
                                 mc.movie_id == mi.movie_id &&
                                 ml.movie_id == cc.movie_id &&
                                 mk.movie_id == cc.movie_id &&
                                 mc.movie_id == cc.movie_id &&
                                 mi.movie_id == cc.movie_id))) {
                            continue;
                          }
                          tmp1.data[tmp2] = (matches_item_t){.company = cn.name,
                                                             .link = lt.link,
                                                             .title = t.title};
                          tmp2++;
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  tmp1.len = tmp2;
  matches_item_list_t matches = tmp1;
  int tmp15 = int_create(matches.len);
  int tmp16 = 0;
  for (int tmp17 = 0; tmp17 < matches.len; tmp17++) {
    matches_item_t x = matches.data[tmp17];
    tmp15.data[tmp16] = x.company;
    tmp16++;
  }
  tmp15.len = tmp16;
  int tmp18 = int_create(matches.len);
  int tmp19 = 0;
  for (int tmp20 = 0; tmp20 < matches.len; tmp20++) {
    matches_item_t x = matches.data[tmp20];
    tmp18.data[tmp19] = x.link;
    tmp19++;
  }
  tmp18.len = tmp19;
  int tmp21 = int_create(matches.len);
  int tmp22 = 0;
  for (int tmp23 = 0; tmp23 < matches.len; tmp23++) {
    matches_item_t x = matches.data[tmp23];
    tmp21.data[tmp22] = x.title;
    tmp22++;
  }
  tmp21.len = tmp22;
  result_item_t result =
      (result_item_t){.producing_company = _min_string(tmp15),
                      .link_type = _min_string(tmp18),
                      .complete_western_sequel = _min_string(tmp21)};
  printf("{");
  _json_string("producing_company");
  printf(":");
  _json_int(result.producing_company);
  printf(",");
  _json_string("link_type");
  printf(":");
  _json_int(result.link_type);
  printf(",");
  _json_string("complete_western_sequel");
  printf(":");
  _json_int(result.complete_western_sequel);
  printf("}");
  test_Q27_selects_minimal_company__link_and_title_result = result;
  test_Q27_selects_minimal_company__link_and_title();
  free(tmp15.data);
  free(tmp18.data);
  free(tmp21.data);
  return 0;
}
