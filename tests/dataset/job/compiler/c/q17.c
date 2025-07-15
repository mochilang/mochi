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
  const char *member_in_charnamed_american_movie;
  const char *a1;
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
  const char *name;
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
  int member_in_charnamed_american_movie;
  int a1;
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
    test_Q17_finds_US_character_name_movie_with_actor_starting_with_B_result;
static void
test_Q17_finds_US_character_name_movie_with_actor_starting_with_B() {
  tmp1_t tmp1[] = {(tmp1_t){.member_in_charnamed_american_movie = "Bob Smith",
                            .a1 = "Bob Smith"}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_Q17_finds_US_character_name_movie_with_actor_starting_with_B_result
          .len != tmp1.len) {
    tmp2 = 0;
  } else {
    for (
        int i3 = 0;
        i3 <
        test_Q17_finds_US_character_name_movie_with_actor_starting_with_B_result
            .len;
        i3++) {
      if (test_Q17_finds_US_character_name_movie_with_actor_starting_with_B_result
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
  cast_info_t cast_info[] = {(cast_info_t){.movie_id = 1, .person_id = 1},
                             (cast_info_t){.movie_id = 2, .person_id = 2}};
  int cast_info_len = sizeof(cast_info) / sizeof(cast_info[0]);
  company_name_t company_name[] = {
      (company_name_t){.id = 1, .country_code = "[us]"},
      (company_name_t){.id = 2, .country_code = "[ca]"}};
  int company_name_len = sizeof(company_name) / sizeof(company_name[0]);
  keyword_t keyword[] = {
      (keyword_t){.id = 10, .keyword = "character-name-in-title"},
      (keyword_t){.id = 20, .keyword = "other"}};
  int keyword_len = sizeof(keyword) / sizeof(keyword[0]);
  movie_companie_t movie_companies[] = {
      (movie_companie_t){.movie_id = 1, .company_id = 1},
      (movie_companie_t){.movie_id = 2, .company_id = 2}};
  int movie_companies_len =
      sizeof(movie_companies) / sizeof(movie_companies[0]);
  movie_keyword_t movie_keyword[] = {
      (movie_keyword_t){.movie_id = 1, .keyword_id = 10},
      (movie_keyword_t){.movie_id = 2, .keyword_id = 20}};
  int movie_keyword_len = sizeof(movie_keyword) / sizeof(movie_keyword[0]);
  name_t name[] = {(name_t){.id = 1, .name = "Bob Smith"},
                   (name_t){.id = 2, .name = "Alice Jones"}};
  int name_len = sizeof(name) / sizeof(name[0]);
  title_t title[] = {(title_t){.id = 1, .title = "Bob's Journey"},
                     (title_t){.id = 2, .title = "Foreign Film"}};
  int title_len = sizeof(title) / sizeof(title[0]);
  int tmp4 =
      int_create(name.len * cast_info.len * title.len * movie_keyword.len *
                 keyword.len * movie_companies.len * company_name.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < name_len; tmp6++) {
    name_t n = name[tmp6];
    for (int tmp7 = 0; tmp7 < cast_info_len; tmp7++) {
      cast_info_t ci = cast_info[tmp7];
      if (!(ci.person_id == n.id)) {
        continue;
      }
      for (int tmp8 = 0; tmp8 < title_len; tmp8++) {
        title_t t = title[tmp8];
        if (!(t.id == ci.movie_id)) {
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
              for (int tmp12 = 0; tmp12 < company_name_len; tmp12++) {
                company_name_t cn = company_name[tmp12];
                if (!(cn.id == mc.company_id)) {
                  continue;
                }
                if (!((strcmp(cn.country_code, "[us]") == 0) &&
                      k.keyword == "character-name-in-title" &&
                      n.name.starts_with("B") && ci.movie_id == mk.movie_id &&
                      ci.movie_id == mc.movie_id &&
                      mc.movie_id == mk.movie_id)) {
                  continue;
                }
                tmp4.data[tmp5] = n.name;
                tmp5++;
              }
            }
          }
        }
      }
    }
  }
  tmp4.len = tmp5;
  int matches = tmp4;
  result_t result[] = {
      (result_t){.member_in_charnamed_american_movie = _min_int(matches),
                 .a1 = _min_int(matches)}};
  int result_len = sizeof(result) / sizeof(result[0]);
  printf("[");
  for (int i13 = 0; i13 < result_len; i13++) {
    if (i13 > 0)
      printf(",");
    result_t it = result[i13];
    printf("{");
    _json_string("member_in_charnamed_american_movie");
    printf(":");
    printf(",");
    _json_string("a1");
    printf(":");
    printf("}");
  }
  printf("]");
  test_Q17_finds_US_character_name_movie_with_actor_starting_with_B_result =
      result;
  test_Q17_finds_US_character_name_movie_with_actor_starting_with_B();
  return 0;
}
