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
  const char *movie_budget;
  int movie_votes;
  const char *writer;
  const char *violent_liongate_movie;
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
  int movie_id;
  int person_id;
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
  const char *name;
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
  int info_type_id;
  int info;
} movie_info_idx_t;
typedef struct {
  int len;
  movie_info_idx_t *data;
} movie_info_idx_list_t;
movie_info_idx_list_t create_movie_info_idx_list(int len) {
  movie_info_idx_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(movie_info_idx_t));
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
  const char *movie_budget;
  int movie_votes;
  const char *writer;
  const char *violent_liongate_movie;
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
  const char *movie_budget;
  int movie_votes;
  const char *writer;
  const char *violent_liongate_movie;
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

static list_int test_Q31_finds_minimal_budget__votes__writer_and_title_result;
static void test_Q31_finds_minimal_budget__votes__writer_and_title() {
  tmp1_t tmp1[] = {(tmp1_t){.movie_budget = "Horror",
                            .movie_votes = 800,
                            .writer = "Arthur",
                            .violent_liongate_movie = "Alpha Horror"}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_Q31_finds_minimal_budget__votes__writer_and_title_result.len !=
      tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0;
         i3 < test_Q31_finds_minimal_budget__votes__writer_and_title_result.len;
         i3++) {
      if (test_Q31_finds_minimal_budget__votes__writer_and_title_result
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
  cast_info_t cast_info[] = {
      (cast_info_t){.movie_id = 1, .person_id = 1, .note = "(writer)"},
      (cast_info_t){.movie_id = 2, .person_id = 2, .note = "(story)"},
      (cast_info_t){.movie_id = 3, .person_id = 3, .note = "(writer)"}};
  int cast_info_len = sizeof(cast_info) / sizeof(cast_info[0]);
  company_name_t company_name[] = {
      (company_name_t){.id = 1, .name = "Lionsgate Pictures"},
      (company_name_t){.id = 2, .name = "Other Studio"}};
  int company_name_len = sizeof(company_name) / sizeof(company_name[0]);
  info_type_t info_type[] = {(info_type_t){.id = 10, .info = "genres"},
                             (info_type_t){.id = 20, .info = "votes"}};
  int info_type_len = sizeof(info_type) / sizeof(info_type[0]);
  keyword_t keyword[] = {(keyword_t){.id = 100, .keyword = "murder"},
                         (keyword_t){.id = 200, .keyword = "comedy"}};
  int keyword_len = sizeof(keyword) / sizeof(keyword[0]);
  movie_companie_t movie_companies[] = {
      (movie_companie_t){.movie_id = 1, .company_id = 1},
      (movie_companie_t){.movie_id = 2, .company_id = 1},
      (movie_companie_t){.movie_id = 3, .company_id = 2}};
  int movie_companies_len =
      sizeof(movie_companies) / sizeof(movie_companies[0]);
  movie_info_t movie_info[] = {
      (movie_info_t){.movie_id = 1, .info_type_id = 10, .info = "Horror"},
      (movie_info_t){.movie_id = 2, .info_type_id = 10, .info = "Thriller"},
      (movie_info_t){.movie_id = 3, .info_type_id = 10, .info = "Comedy"}};
  int movie_info_len = sizeof(movie_info) / sizeof(movie_info[0]);
  movie_info_idx_t movie_info_idx[] = {
      (movie_info_idx_t){.movie_id = 1, .info_type_id = 20, .info = 1000},
      (movie_info_idx_t){.movie_id = 2, .info_type_id = 20, .info = 800},
      (movie_info_idx_t){.movie_id = 3, .info_type_id = 20, .info = 500}};
  int movie_info_idx_len = sizeof(movie_info_idx) / sizeof(movie_info_idx[0]);
  movie_keyword_t movie_keyword[] = {
      (movie_keyword_t){.movie_id = 1, .keyword_id = 100},
      (movie_keyword_t){.movie_id = 2, .keyword_id = 100},
      (movie_keyword_t){.movie_id = 3, .keyword_id = 200}};
  int movie_keyword_len = sizeof(movie_keyword) / sizeof(movie_keyword[0]);
  name_t name[] = {(name_t){.id = 1, .name = "Arthur", .gender = "m"},
                   (name_t){.id = 2, .name = "Bob", .gender = "m"},
                   (name_t){.id = 3, .name = "Carla", .gender = "f"}};
  int name_len = sizeof(name) / sizeof(name[0]);
  title_t title[] = {(title_t){.id = 1, .title = "Alpha Horror"},
                     (title_t){.id = 2, .title = "Beta Blood"},
                     (title_t){.id = 3, .title = "Gamma Comedy"}};
  int title_len = sizeof(title) / sizeof(title[0]);
  list_string matches = list_string_create(5);
  matches.data[0] = "(writer)";
  matches.data[1] = "(head writer)";
  matches.data[2] = "(written by)";
  matches.data[3] = "(story)";
  matches.data[4] = "(story editor)";
  list_string matches = list_string_create(7);
  matches.data[0] = "murder";
  matches.data[1] = "violence";
  matches.data[2] = "blood";
  matches.data[3] = "gore";
  matches.data[4] = "death";
  matches.data[5] = "female-nudity";
  matches.data[6] = "hospital";
  list_string matches = list_string_create(2);
  matches.data[0] = "Horror";
  matches.data[1] = "Thriller";
  matches_item_list_t tmp4 = matches_item_list_t_create(
      cast_info.len * name.len * title.len * movie_info.len *
      movie_info_idx.len * movie_keyword.len * keyword.len *
      movie_companies.len * company_name.len * info_type.len * info_type.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < cast_info_len; tmp6++) {
    cast_info_t ci = cast_info[tmp6];
    for (int tmp7 = 0; tmp7 < name_len; tmp7++) {
      name_t n = name[tmp7];
      if (!(n.id == ci.person_id)) {
        continue;
      }
      for (int tmp8 = 0; tmp8 < title_len; tmp8++) {
        title_t t = title[tmp8];
        if (!(t.id == ci.movie_id)) {
          continue;
        }
        for (int tmp9 = 0; tmp9 < movie_info_len; tmp9++) {
          movie_info_t mi = movie_info[tmp9];
          if (!(mi.movie_id == t.id)) {
            continue;
          }
          for (int tmp10 = 0; tmp10 < movie_info_idx_len; tmp10++) {
            movie_info_idx_t mi_idx = movie_info_idx[tmp10];
            if (!(mi_idx.movie_id == t.id)) {
              continue;
            }
            for (int tmp11 = 0; tmp11 < movie_keyword_len; tmp11++) {
              movie_keyword_t mk = movie_keyword[tmp11];
              if (!(mk.movie_id == t.id)) {
                continue;
              }
              for (int tmp12 = 0; tmp12 < keyword_len; tmp12++) {
                keyword_t k = keyword[tmp12];
                if (!(k.id == mk.keyword_id)) {
                  continue;
                }
                for (int tmp13 = 0; tmp13 < movie_companies_len; tmp13++) {
                  movie_companie_t mc = movie_companies[tmp13];
                  if (!(mc.movie_id == t.id)) {
                    continue;
                  }
                  for (int tmp14 = 0; tmp14 < company_name_len; tmp14++) {
                    company_name_t cn = company_name[tmp14];
                    if (!(cn.id == mc.company_id)) {
                      continue;
                    }
                    for (int tmp15 = 0; tmp15 < info_type_len; tmp15++) {
                      info_type_t it1 = info_type[tmp15];
                      if (!(it1.id == mi.info_type_id)) {
                        continue;
                      }
                      for (int tmp16 = 0; tmp16 < info_type_len; tmp16++) {
                        info_type_t it2 = info_type[tmp16];
                        if (!(it2.id == mi_idx.info_type_id)) {
                          continue;
                        }
                        if (!(contains_list_string(
                                  matches,
                                  contains_list_string(
                                      matches,
                                      contains_list_string(matches, ci.note) &&
                                          cn.name.starts_with("Lionsgate") &&
                                          it1.info == "genres" &&
                                          it2.info == "votes" && k.keyword) &&
                                      mi.info) &&
                              n.gender == "m")) {
                          continue;
                        }
                        tmp4.data[tmp5] =
                            (matches_item_t){.movie_budget = mi.info,
                                             .movie_votes = mi_idx.info,
                                             .writer = n.name,
                                             .violent_liongate_movie = t.title};
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
  tmp4.len = tmp5;
  matches_item_list_t matches = tmp4;
  int tmp17 = int_create(matches.len);
  int tmp18 = 0;
  for (int tmp19 = 0; tmp19 < matches.len; tmp19++) {
    matches_item_t r = matches.data[tmp19];
    tmp17.data[tmp18] = r.movie_budget;
    tmp18++;
  }
  tmp17.len = tmp18;
  list_int tmp20 = list_int_create(matches.len);
  int tmp21 = 0;
  for (int tmp22 = 0; tmp22 < matches.len; tmp22++) {
    matches_item_t r = matches.data[tmp22];
    tmp20.data[tmp21] = r.movie_votes;
    tmp21++;
  }
  tmp20.len = tmp21;
  int tmp23 = int_create(matches.len);
  int tmp24 = 0;
  for (int tmp25 = 0; tmp25 < matches.len; tmp25++) {
    matches_item_t r = matches.data[tmp25];
    tmp23.data[tmp24] = r.writer;
    tmp24++;
  }
  tmp23.len = tmp24;
  int tmp26 = int_create(matches.len);
  int tmp27 = 0;
  for (int tmp28 = 0; tmp28 < matches.len; tmp28++) {
    matches_item_t r = matches.data[tmp28];
    tmp26.data[tmp27] = r.violent_liongate_movie;
    tmp27++;
  }
  tmp26.len = tmp27;
  result_t result[] = {
      (result_t){.movie_budget = _min_string(tmp17),
                 .movie_votes = _min_int(tmp20),
                 .writer = _min_string(tmp23),
                 .violent_liongate_movie = _min_string(tmp26)}};
  int result_len = sizeof(result) / sizeof(result[0]);
  printf("[");
  for (int i29 = 0; i29 < result_len; i29++) {
    if (i29 > 0)
      printf(",");
    result_t it = result[i29];
    printf("{");
    _json_string("movie_budget");
    printf(":");
    _json_string(it.movie_budget);
    printf(",");
    _json_string("movie_votes");
    printf(":");
    _json_int(it.movie_votes);
    printf(",");
    _json_string("writer");
    printf(":");
    _json_string(it.writer);
    printf(",");
    _json_string("violent_liongate_movie");
    printf(":");
    _json_string(it.violent_liongate_movie);
    printf("}");
  }
  printf("]");
  test_Q31_finds_minimal_budget__votes__writer_and_title_result = result;
  test_Q31_finds_minimal_budget__votes__writer_and_title();
  free(tmp17.data);
  free(tmp20.data);
  free(tmp23.data);
  free(tmp26.data);
  return 0;
}
