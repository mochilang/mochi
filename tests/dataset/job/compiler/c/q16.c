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
  const char *cool_actor_pseudonym;
  const char *series_named_after_char;
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
  const char *name;
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
  int person_id;
  int movie_id;
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
  int episode_nr;
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
  const char *pseudonym;
  const char *series;
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
  const char *cool_actor_pseudonym;
  const char *series_named_after_char;
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

static list_int
    test_Q16_finds_series_named_after_a_character_between_episodes_50_and_99_result;
static void
test_Q16_finds_series_named_after_a_character_between_episodes_50_and_99() {
  tmp1_t tmp1[] = {(tmp1_t){.cool_actor_pseudonym = "Alpha",
                            .series_named_after_char = "Hero Bob"}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_Q16_finds_series_named_after_a_character_between_episodes_50_and_99_result
          .len != tmp1.len) {
    tmp2 = 0;
  } else {
    for (
        int i3 = 0;
        i3 <
        test_Q16_finds_series_named_after_a_character_between_episodes_50_and_99_result
            .len;
        i3++) {
      if (test_Q16_finds_series_named_after_a_character_between_episodes_50_and_99_result
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
  aka_name_t aka_name[] = {(aka_name_t){.person_id = 1, .name = "Alpha"},
                           (aka_name_t){.person_id = 2, .name = "Beta"}};
  int aka_name_len = sizeof(aka_name) / sizeof(aka_name[0]);
  cast_info_t cast_info[] = {(cast_info_t){.person_id = 1, .movie_id = 101},
                             (cast_info_t){.person_id = 2, .movie_id = 102}};
  int cast_info_len = sizeof(cast_info) / sizeof(cast_info[0]);
  company_name_t company_name[] = {
      (company_name_t){.id = 1, .country_code = "[us]"},
      (company_name_t){.id = 2, .country_code = "[de]"}};
  int company_name_len = sizeof(company_name) / sizeof(company_name[0]);
  keyword_t keyword[] = {
      (keyword_t){.id = 1, .keyword = "character-name-in-title"},
      (keyword_t){.id = 2, .keyword = "other"}};
  int keyword_len = sizeof(keyword) / sizeof(keyword[0]);
  movie_companie_t movie_companies[] = {
      (movie_companie_t){.movie_id = 101, .company_id = 1},
      (movie_companie_t){.movie_id = 102, .company_id = 2}};
  int movie_companies_len =
      sizeof(movie_companies) / sizeof(movie_companies[0]);
  movie_keyword_t movie_keyword[] = {
      (movie_keyword_t){.movie_id = 101, .keyword_id = 1},
      (movie_keyword_t){.movie_id = 102, .keyword_id = 2}};
  int movie_keyword_len = sizeof(movie_keyword) / sizeof(movie_keyword[0]);
  name_t name[] = {(name_t){.id = 1}, (name_t){.id = 2}};
  int name_len = sizeof(name) / sizeof(name[0]);
  title_t title[] = {
      (title_t){.id = 101, .title = "Hero Bob", .episode_nr = 60},
      (title_t){.id = 102, .title = "Other Show", .episode_nr = 40}};
  int title_len = sizeof(title) / sizeof(title[0]);
  rows_item_list_t tmp4 = rows_item_list_t_create(
      aka_name.len * name.len * cast_info.len * title.len * movie_keyword.len *
      keyword.len * movie_companies.len * company_name.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < aka_name_len; tmp6++) {
    aka_name_t an = aka_name[tmp6];
    for (int tmp7 = 0; tmp7 < name_len; tmp7++) {
      name_t n = name[tmp7];
      if (!(n.id == an.person_id)) {
        continue;
      }
      for (int tmp8 = 0; tmp8 < cast_info_len; tmp8++) {
        cast_info_t ci = cast_info[tmp8];
        if (!(ci.person_id == n.id)) {
          continue;
        }
        for (int tmp9 = 0; tmp9 < title_len; tmp9++) {
          title_t t = title[tmp9];
          if (!(t.id == ci.movie_id)) {
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
              for (int tmp12 = 0; tmp12 < movie_companies_len; tmp12++) {
                movie_companie_t mc = movie_companies[tmp12];
                if (!(mc.movie_id == t.id)) {
                  continue;
                }
                for (int tmp13 = 0; tmp13 < company_name_len; tmp13++) {
                  company_name_t cn = company_name[tmp13];
                  if (!(cn.id == mc.company_id)) {
                    continue;
                  }
                  if (!((strcmp(cn.country_code, "[us]") == 0) &&
                        k.keyword == "character-name-in-title" &&
                        t.episode_nr >= 50 && t.episode_nr < 100)) {
                    continue;
                  }
                  tmp4.data[tmp5] =
                      (rows_item_t){.pseudonym = an.name, .series = t.title};
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
  rows_item_list_t rows = tmp4;
  int tmp14 = int_create(rows.len);
  int tmp15 = 0;
  for (int tmp16 = 0; tmp16 < rows.len; tmp16++) {
    rows_item_t r = rows.data[tmp16];
    tmp14.data[tmp15] = r.pseudonym;
    tmp15++;
  }
  tmp14.len = tmp15;
  int tmp17 = int_create(rows.len);
  int tmp18 = 0;
  for (int tmp19 = 0; tmp19 < rows.len; tmp19++) {
    rows_item_t r = rows.data[tmp19];
    tmp17.data[tmp18] = r.series;
    tmp18++;
  }
  tmp17.len = tmp18;
  result_t result[] = {
      (result_t){.cool_actor_pseudonym = _min_string(tmp14),
                 .series_named_after_char = _min_string(tmp17)}};
  int result_len = sizeof(result) / sizeof(result[0]);
  printf("[");
  for (int i20 = 0; i20 < result_len; i20++) {
    if (i20 > 0)
      printf(",");
    result_t it = result[i20];
    printf("{");
    _json_string("cool_actor_pseudonym");
    printf(":");
    _json_string(it.cool_actor_pseudonym);
    printf(",");
    _json_string("series_named_after_char");
    printf(":");
    _json_string(it.series_named_after_char);
    printf("}");
  }
  printf("]");
  test_Q16_finds_series_named_after_a_character_between_episodes_50_and_99_result =
      result;
  test_Q16_finds_series_named_after_a_character_between_episodes_50_and_99();
  free(tmp14.data);
  free(tmp17.data);
  return 0;
}
