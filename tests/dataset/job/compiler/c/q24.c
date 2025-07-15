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
  const char *voiced_char_name;
  const char *voicing_actress_name;
  const char *voiced_action_movie_jap_eng;
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
  int person_id;
} aka_name_t;
typedef struct {
  int len;
  aka_name_t *data;
} aka_name_list_t;
aka_name_list_t create_aka_name_list(int len) {
  aka_name_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(aka_name_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  const char *name;
} char_name_t;
typedef struct {
  int len;
  char_name_t *data;
} char_name_list_t;
char_name_list_t create_char_name_list(int len) {
  char_name_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(char_name_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int movie_id;
  int person_id;
  int person_role_id;
  int role_id;
  const char *note;
} cast_info_t;
typedef struct {
  int len;
  cast_info_t *data;
} cast_info_list_t;
cast_info_list_t create_cast_info_list(int len) {
  cast_info_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(cast_info_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
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
  const char *info;
} info_type_t;
typedef struct {
  int len;
  info_type_t *data;
} info_type_list_t;
info_type_list_t create_info_type_list(int len) {
  info_type_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(info_type_t));
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
  int movie_id;
  int company_id;
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
  int info_type_id;
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
  int id;
  const char *name;
  const char *gender;
} name_t;
typedef struct {
  int len;
  name_t *data;
} name_list_t;
name_list_t create_name_list(int len) {
  name_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(name_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  const char *role;
} role_type_t;
typedef struct {
  int len;
  role_type_t *data;
} role_type_list_t;
role_type_list_t create_role_type_list(int len) {
  role_type_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(role_type_t));
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
  const char *voiced_char_name;
  const char *voicing_actress_name;
  const char *voiced_action_movie_jap_eng;
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
  const char *voiced_char_name;
  const char *voicing_actress_name;
  const char *voiced_action_movie_jap_eng;
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

static list_int test_Q24_finds_voiced_action_movie_with_actress_named_An_result;
static void test_Q24_finds_voiced_action_movie_with_actress_named_An() {
  tmp1_t tmp1[] = {(tmp1_t){.voiced_char_name = "Hero Character",
                            .voicing_actress_name = "Ann Actress",
                            .voiced_action_movie_jap_eng = "Heroic Adventure"}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_Q24_finds_voiced_action_movie_with_actress_named_An_result.len !=
      tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0;
         i3 <
         test_Q24_finds_voiced_action_movie_with_actress_named_An_result.len;
         i3++) {
      if (test_Q24_finds_voiced_action_movie_with_actress_named_An_result
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
  aka_name_t aka_name[] = {(aka_name_t){.person_id = 1}};
  int aka_name_len = sizeof(aka_name) / sizeof(aka_name[0]);
  char_name_t char_name[] = {(char_name_t){.id = 1, .name = "Hero Character"}};
  int char_name_len = sizeof(char_name) / sizeof(char_name[0]);
  cast_info_t cast_info[] = {(cast_info_t){.movie_id = 1,
                                           .person_id = 1,
                                           .person_role_id = 1,
                                           .role_id = 1,
                                           .note = "(voice)"}};
  int cast_info_len = sizeof(cast_info) / sizeof(cast_info[0]);
  company_name_t company_name[] = {
      (company_name_t){.id = 1, .country_code = "[us]"}};
  int company_name_len = sizeof(company_name) / sizeof(company_name[0]);
  info_type_t info_type[] = {(info_type_t){.id = 1, .info = "release dates"}};
  int info_type_len = sizeof(info_type) / sizeof(info_type[0]);
  keyword_t keyword[] = {(keyword_t){.id = 1, .keyword = "hero"}};
  int keyword_len = sizeof(keyword) / sizeof(keyword[0]);
  movie_companie_t movie_companies[] = {
      (movie_companie_t){.movie_id = 1, .company_id = 1}};
  int movie_companies_len =
      sizeof(movie_companies) / sizeof(movie_companies[0]);
  movie_info_t movie_info[] = {(movie_info_t){
      .movie_id = 1, .info_type_id = 1, .info = "Japan: Feb 2015"}};
  int movie_info_len = sizeof(movie_info) / sizeof(movie_info[0]);
  movie_keyword_t movie_keyword[] = {
      (movie_keyword_t){.movie_id = 1, .keyword_id = 1}};
  int movie_keyword_len = sizeof(movie_keyword) / sizeof(movie_keyword[0]);
  name_t name[] = {(name_t){.id = 1, .name = "Ann Actress", .gender = "f"}};
  int name_len = sizeof(name) / sizeof(name[0]);
  role_type_t role_type[] = {(role_type_t){.id = 1, .role = "actress"}};
  int role_type_len = sizeof(role_type) / sizeof(role_type[0]);
  title_t title[] = {
      (title_t){.id = 1, .title = "Heroic Adventure", .production_year = 2015}};
  int title_len = sizeof(title) / sizeof(title[0]);
  list_string matches = list_string_create(4);
  matches.data[0] = "(voice)";
  matches.data[1] = "(voice: Japanese version)";
  matches.data[2] = "(voice) (uncredited)";
  matches.data[3] = "(voice: English version)";
  list_string matches = list_string_create(3);
  matches.data[0] = "hero";
  matches.data[1] = "martial-arts";
  matches.data[2] = "hand-to-hand-combat";
  matches_item_list_t tmp4 = create_matches_item_list(
      aka_name_len * char_name_len * cast_info_len * company_name_len *
      info_type_len * keyword_len * movie_companies_len * movie_info_len *
      movie_keyword_len * name_len * role_type_len * title_len);
  int tmp5 = 0;
  for (int an_idx = 0; an_idx < aka_name_len; an_idx++) {
    aka_name_t an = aka_name[an_idx];
    for (int chn_idx = 0; chn_idx < char_name_len; chn_idx++) {
      char_name_t chn = char_name[chn_idx];
      for (int ci_idx = 0; ci_idx < cast_info_len; ci_idx++) {
        cast_info_t ci = cast_info[ci_idx];
        for (int cn_idx = 0; cn_idx < company_name_len; cn_idx++) {
          company_name_t cn = company_name[cn_idx];
          for (int it_idx = 0; it_idx < info_type_len; it_idx++) {
            info_type_t it = info_type[it_idx];
            for (int k_idx = 0; k_idx < keyword_len; k_idx++) {
              keyword_t k = keyword[k_idx];
              for (int mc_idx = 0; mc_idx < movie_companies_len; mc_idx++) {
                movie_companie_t mc = movie_companies[mc_idx];
                for (int mi_idx = 0; mi_idx < movie_info_len; mi_idx++) {
                  movie_info_t mi = movie_info[mi_idx];
                  for (int mk_idx = 0; mk_idx < movie_keyword_len; mk_idx++) {
                    movie_keyword_t mk = movie_keyword[mk_idx];
                    for (int n_idx = 0; n_idx < name_len; n_idx++) {
                      name_t n = name[n_idx];
                      for (int rt_idx = 0; rt_idx < role_type_len; rt_idx++) {
                        role_type_t rt = role_type[rt_idx];
                        for (int t_idx = 0; t_idx < title_len; t_idx++) {
                          title_t t = title[t_idx];
                          if (!((contains_list_string(
                                     matches,
                                     contains_list_string(matches, ci.note) &&
                                         cn.country_code == "[us]" &&
                                         it.info == "release dates" &&
                                         k.keyword) &&
                                 mi.info != 0 &&
                                 (mi.info.starts_with("Japan:") &&
                                      contains_string(mi.info, "201") ||
                                  mi.info.starts_with("USA:") &&
                                      contains_string(mi.info, "201")) &&
                                 n.gender == "f" &&
                                 contains_string(n.name, "An") &&
                                 rt.role == "actress" &&
                                 t.production_year > 2010 &&
                                 t.id == mi.movie_id && t.id == mc.movie_id &&
                                 t.id == ci.movie_id && t.id == mk.movie_id &&
                                 mc.movie_id == ci.movie_id &&
                                 mc.movie_id == mi.movie_id &&
                                 mc.movie_id == mk.movie_id &&
                                 mi.movie_id == ci.movie_id &&
                                 mi.movie_id == mk.movie_id &&
                                 ci.movie_id == mk.movie_id &&
                                 cn.id == mc.company_id &&
                                 it.id == mi.info_type_id &&
                                 n.id == ci.person_id && rt.id == ci.role_id &&
                                 n.id == an.person_id &&
                                 ci.person_id == an.person_id &&
                                 chn.id == ci.person_role_id &&
                                 k.id == mk.keyword_id))) {
                            continue;
                          }
                          tmp4.data[tmp5] = (matches_item_t){
                              .voiced_char_name = chn.name,
                              .voicing_actress_name = n.name,
                              .voiced_action_movie_jap_eng = t.title};
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
      }
    }
  }
  tmp4.len = tmp5;
  matches_item_list_t matches = tmp4;
  int tmp6 = int_create(matches.len);
  int tmp7 = 0;
  for (int tmp8 = 0; tmp8 < matches.len; tmp8++) {
    matches_item_t x = matches.data[tmp8];
    tmp6.data[tmp7] = x.voiced_char_name;
    tmp7++;
  }
  tmp6.len = tmp7;
  int tmp9 = int_create(matches.len);
  int tmp10 = 0;
  for (int tmp11 = 0; tmp11 < matches.len; tmp11++) {
    matches_item_t x = matches.data[tmp11];
    tmp9.data[tmp10] = x.voicing_actress_name;
    tmp10++;
  }
  tmp9.len = tmp10;
  int tmp12 = int_create(matches.len);
  int tmp13 = 0;
  for (int tmp14 = 0; tmp14 < matches.len; tmp14++) {
    matches_item_t x = matches.data[tmp14];
    tmp12.data[tmp13] = x.voiced_action_movie_jap_eng;
    tmp13++;
  }
  tmp12.len = tmp13;
  result_t result[] = {
      (result_t){.voiced_char_name = _min_string(tmp6),
                 .voicing_actress_name = _min_string(tmp9),
                 .voiced_action_movie_jap_eng = _min_string(tmp12)}};
  int result_len = sizeof(result) / sizeof(result[0]);
  printf("[");
  for (int i15 = 0; i15 < result_len; i15++) {
    if (i15 > 0)
      printf(",");
    result_t it = result[i15];
    printf("{");
    _json_string("voiced_char_name");
    printf(":");
    _json_string(it.voiced_char_name);
    printf(",");
    _json_string("voicing_actress_name");
    printf(":");
    _json_string(it.voicing_actress_name);
    printf(",");
    _json_string("voiced_action_movie_jap_eng");
    printf(":");
    _json_string(it.voiced_action_movie_jap_eng);
    printf("}");
  }
  printf("]");
  test_Q24_finds_voiced_action_movie_with_actress_named_An_result = result;
  test_Q24_finds_voiced_action_movie_with_actress_named_An();
  free(matches.data);
  free(tmp6.data);
  free(tmp9.data);
  free(tmp12.data);
  return 0;
}
