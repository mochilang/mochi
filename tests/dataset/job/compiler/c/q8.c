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
  const char *actress_pseudonym;
  const char *japanese_movie_dubbed;
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
  const char *note;
  int role_id;
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
  int movie_id;
  int company_id;
  const char *note;
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
  const char *movie_title;
} eligible_item_t;
typedef struct {
  int len;
  eligible_item_t *data;
} eligible_item_list_t;
eligible_item_list_t create_eligible_item_list(int len) {
  eligible_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(eligible_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  const char *actress_pseudonym;
  const char *japanese_movie_dubbed;
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
    test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing_result;
static void
test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing() {
  tmp1_t tmp1[] = {(tmp1_t){.actress_pseudonym = "Y. S.",
                            .japanese_movie_dubbed = "Dubbed Film"}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing_result
          .len != tmp1.len) {
    tmp2 = 0;
  } else {
    for (
        int i3 = 0;
        i3 <
        test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing_result
            .len;
        i3++) {
      if (test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing_result
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
  aka_name_t aka_name[] = {(aka_name_t){.person_id = 1, .name = "Y. S."}};
  int aka_name_len = sizeof(aka_name) / sizeof(aka_name[0]);
  cast_info_t cast_info[] = {(cast_info_t){.person_id = 1,
                                           .movie_id = 10,
                                           .note = "(voice: English version)",
                                           .role_id = 1000}};
  int cast_info_len = sizeof(cast_info) / sizeof(cast_info[0]);
  company_name_t company_name[] = {
      (company_name_t){.id = 50, .country_code = "[jp]"}};
  int company_name_len = sizeof(company_name) / sizeof(company_name[0]);
  movie_companie_t movie_companies[] = {(movie_companie_t){
      .movie_id = 10, .company_id = 50, .note = "Studio (Japan)"}};
  int movie_companies_len =
      sizeof(movie_companies) / sizeof(movie_companies[0]);
  name_t name[] = {(name_t){.id = 1, .name = "Yoko Ono"},
                   (name_t){.id = 2, .name = "Yuichi"}};
  int name_len = sizeof(name) / sizeof(name[0]);
  role_type_t role_type[] = {(role_type_t){.id = 1000, .role = "actress"}};
  int role_type_len = sizeof(role_type) / sizeof(role_type[0]);
  title_t title[] = {(title_t){.id = 10, .title = "Dubbed Film"}};
  int title_len = sizeof(title) / sizeof(title[0]);
  eligible_item_list_t tmp4 = eligible_item_list_t_create(
      aka_name.len * name.len * cast_info.len * title.len *
      movie_companies.len * company_name.len * role_type.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < aka_name_len; tmp6++) {
    aka_name_t an1 = aka_name[tmp6];
    for (int tmp7 = 0; tmp7 < name_len; tmp7++) {
      name_t n1 = name[tmp7];
      if (!(n1.id == an1.person_id)) {
        continue;
      }
      for (int tmp8 = 0; tmp8 < cast_info_len; tmp8++) {
        cast_info_t ci = cast_info[tmp8];
        if (!(ci.person_id == an1.person_id)) {
          continue;
        }
        for (int tmp9 = 0; tmp9 < title_len; tmp9++) {
          title_t t = title[tmp9];
          if (!(t.id == ci.movie_id)) {
            continue;
          }
          for (int tmp10 = 0; tmp10 < movie_companies_len; tmp10++) {
            movie_companie_t mc = movie_companies[tmp10];
            if (!(mc.movie_id == ci.movie_id)) {
              continue;
            }
            for (int tmp11 = 0; tmp11 < company_name_len; tmp11++) {
              company_name_t cn = company_name[tmp11];
              if (!(cn.id == mc.company_id)) {
                continue;
              }
              for (int tmp12 = 0; tmp12 < role_type_len; tmp12++) {
                role_type_t rt = role_type[tmp12];
                if (!(rt.id == ci.role_id)) {
                  continue;
                }
                if (!((strcmp(ci.note, "(voice: English version)") == 0) &&
                      cn.country_code == "[jp]" &&
                      contains_string(mc.note, "(Japan)") &&
                      ((!contains_string(mc.note, "(USA)"))) &&
                      contains_string(n1.name, "Yo") &&
                      ((!contains_string(n1.name, "Yu"))) &&
                      rt.role == "actress")) {
                  continue;
                }
                tmp4.data[tmp5] = (eligible_item_t){.pseudonym = an1.name,
                                                    .movie_title = t.title};
                tmp5++;
              }
            }
          }
        }
      }
    }
  }
  tmp4.len = tmp5;
  eligible_item_list_t eligible = tmp4;
  int tmp13 = int_create(eligible.len);
  int tmp14 = 0;
  for (int tmp15 = 0; tmp15 < eligible.len; tmp15++) {
    eligible_item_t x = eligible.data[tmp15];
    tmp13.data[tmp14] = x.pseudonym;
    tmp14++;
  }
  tmp13.len = tmp14;
  int tmp16 = int_create(eligible.len);
  int tmp17 = 0;
  for (int tmp18 = 0; tmp18 < eligible.len; tmp18++) {
    eligible_item_t x = eligible.data[tmp18];
    tmp16.data[tmp17] = x.movie_title;
    tmp17++;
  }
  tmp16.len = tmp17;
  result_t result[] = {(result_t){.actress_pseudonym = _min_string(tmp13),
                                  .japanese_movie_dubbed = _min_string(tmp16)}};
  int result_len = sizeof(result) / sizeof(result[0]);
  printf("[");
  for (int i19 = 0; i19 < result_len; i19++) {
    if (i19 > 0)
      printf(",");
    result_t it = result[i19];
    printf("{");
    _json_string("actress_pseudonym");
    printf(":");
    _json_string(it.actress_pseudonym);
    printf(",");
    _json_string("japanese_movie_dubbed");
    printf(":");
    _json_string(it.japanese_movie_dubbed);
    printf("}");
  }
  printf("]");
  test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing_result =
      result;
  test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing();
  free(tmp13.data);
  free(tmp16.data);
  return 0;
}
