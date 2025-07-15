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
  const char *male_writer;
  const char *violent_movie_title;
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
  const char *budget;
  int votes;
  const char *writer;
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
  const char *movie_budget;
  int movie_votes;
  const char *male_writer;
  const char *violent_movie_title;
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

static list_int test_Q25_finds_male_horror_writer_with_violent_keywords_result;
static void test_Q25_finds_male_horror_writer_with_violent_keywords() {
  tmp1_t tmp1[] = {(tmp1_t){.movie_budget = "Horror",
                            .movie_votes = 100,
                            .male_writer = "Mike",
                            .violent_movie_title = "Scary Movie"}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_Q25_finds_male_horror_writer_with_violent_keywords_result.len !=
      tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0;
         i3 <
         test_Q25_finds_male_horror_writer_with_violent_keywords_result.len;
         i3++) {
      if (test_Q25_finds_male_horror_writer_with_violent_keywords_result
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
      (cast_info_t){.movie_id = 2, .person_id = 2, .note = "(writer)"}};
  int cast_info_len = sizeof(cast_info) / sizeof(cast_info[0]);
  info_type_t info_type[] = {(info_type_t){.id = 1, .info = "genres"},
                             (info_type_t){.id = 2, .info = "votes"}};
  int info_type_len = sizeof(info_type) / sizeof(info_type[0]);
  keyword_t keyword[] = {(keyword_t){.id = 1, .keyword = "murder"},
                         (keyword_t){.id = 2, .keyword = "romance"}};
  int keyword_len = sizeof(keyword) / sizeof(keyword[0]);
  movie_info_t movie_info[] = {
      (movie_info_t){.movie_id = 1, .info_type_id = 1, .info = "Horror"},
      (movie_info_t){.movie_id = 2, .info_type_id = 1, .info = "Comedy"}};
  int movie_info_len = sizeof(movie_info) / sizeof(movie_info[0]);
  movie_info_idx_t movie_info_idx[] = {
      (movie_info_idx_t){.movie_id = 1, .info_type_id = 2, .info = 100},
      (movie_info_idx_t){.movie_id = 2, .info_type_id = 2, .info = 50}};
  int movie_info_idx_len = sizeof(movie_info_idx) / sizeof(movie_info_idx[0]);
  movie_keyword_t movie_keyword[] = {
      (movie_keyword_t){.movie_id = 1, .keyword_id = 1},
      (movie_keyword_t){.movie_id = 2, .keyword_id = 2}};
  int movie_keyword_len = sizeof(movie_keyword) / sizeof(movie_keyword[0]);
  name_t name[] = {(name_t){.id = 1, .name = "Mike", .gender = "m"},
                   (name_t){.id = 2, .name = "Sue", .gender = "f"}};
  int name_len = sizeof(name) / sizeof(name[0]);
  title_t title[] = {(title_t){.id = 1, .title = "Scary Movie"},
                     (title_t){.id = 2, .title = "Funny Movie"}};
  int title_len = sizeof(title) / sizeof(title[0]);
  list_string allowed_notes = list_string_create(5);
  allowed_notes.data[0] = "(writer)";
  allowed_notes.data[1] = "(head writer)";
  allowed_notes.data[2] = "(written by)";
  allowed_notes.data[3] = "(story)";
  allowed_notes.data[4] = "(story editor)";
  int allowed_notes = allowed_notes;
  list_string allowed_keywords = list_string_create(5);
  allowed_keywords.data[0] = "murder";
  allowed_keywords.data[1] = "blood";
  allowed_keywords.data[2] = "gore";
  allowed_keywords.data[3] = "death";
  allowed_keywords.data[4] = "female-nudity";
  int allowed_keywords = allowed_keywords;
  matches_item_list_t tmp4 = create_matches_item_list(
      cast_info_len * info_type_len * info_type_len * keyword_len *
      movie_info_len * movie_info_idx_len * movie_keyword_len * name_len *
      title_len);
  int tmp5 = 0;
  for (int ci_idx = 0; ci_idx < cast_info_len; ci_idx++) {
    cast_info_t ci = cast_info[ci_idx];
    for (int it1_idx = 0; it1_idx < info_type_len; it1_idx++) {
      info_type_t it1 = info_type[it1_idx];
      for (int it2_idx = 0; it2_idx < info_type_len; it2_idx++) {
        info_type_t it2 = info_type[it2_idx];
        for (int k_idx = 0; k_idx < keyword_len; k_idx++) {
          keyword_t k = keyword[k_idx];
          for (int mi_idx = 0; mi_idx < movie_info_len; mi_idx++) {
            movie_info_t mi = movie_info[mi_idx];
            for (int mi_idx_idx = 0; mi_idx_idx < movie_info_idx_len;
                 mi_idx_idx++) {
              movie_info_idx_t mi_idx = movie_info_idx[mi_idx_idx];
              for (int mk_idx = 0; mk_idx < movie_keyword_len; mk_idx++) {
                movie_keyword_t mk = movie_keyword[mk_idx];
                for (int n_idx = 0; n_idx < name_len; n_idx++) {
                  name_t n = name[n_idx];
                  for (int t_idx = 0; t_idx < title_len; t_idx++) {
                    title_t t = title[t_idx];
                    if (!(((contains_list_string(allowed_notes, ci.note)) &&
                           it1.info == "genres" && it2.info == "votes" &&
                           (contains_list_string(allowed_keywords,
                                                 k.keyword)) &&
                           mi.info == "Horror" && n.gender == "m" &&
                           t.id == mi.movie_id && t.id == mi_idx.movie_id &&
                           t.id == ci.movie_id && t.id == mk.movie_id &&
                           ci.movie_id == mi.movie_id &&
                           ci.movie_id == mi_idx.movie_id &&
                           ci.movie_id == mk.movie_id &&
                           mi.movie_id == mi_idx.movie_id &&
                           mi.movie_id == mk.movie_id &&
                           mi_idx.movie_id == mk.movie_id &&
                           n.id == ci.person_id && it1.id == mi.info_type_id &&
                           it2.id == mi_idx.info_type_id &&
                           k.id == mk.keyword_id))) {
                      continue;
                    }
                    tmp4.data[tmp5] = (matches_item_t){.budget = mi.info,
                                                       .votes = mi_idx.info,
                                                       .writer = n.name,
                                                       .title = t.title};
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
  matches_item_list_t matches = tmp4;
  int tmp6 = int_create(matches.len);
  int tmp7 = 0;
  for (int tmp8 = 0; tmp8 < matches.len; tmp8++) {
    matches_item_t x = matches.data[tmp8];
    tmp6.data[tmp7] = x.budget;
    tmp7++;
  }
  tmp6.len = tmp7;
  list_int tmp9 = list_int_create(matches.len);
  int tmp10 = 0;
  for (int tmp11 = 0; tmp11 < matches.len; tmp11++) {
    matches_item_t x = matches.data[tmp11];
    tmp9.data[tmp10] = x.votes;
    tmp10++;
  }
  tmp9.len = tmp10;
  int tmp12 = int_create(matches.len);
  int tmp13 = 0;
  for (int tmp14 = 0; tmp14 < matches.len; tmp14++) {
    matches_item_t x = matches.data[tmp14];
    tmp12.data[tmp13] = x.writer;
    tmp13++;
  }
  tmp12.len = tmp13;
  int tmp15 = int_create(matches.len);
  int tmp16 = 0;
  for (int tmp17 = 0; tmp17 < matches.len; tmp17++) {
    matches_item_t x = matches.data[tmp17];
    tmp15.data[tmp16] = x.title;
    tmp16++;
  }
  tmp15.len = tmp16;
  result_t result[] = {(result_t){.movie_budget = _min_string(tmp6),
                                  .movie_votes = _min_int(tmp9),
                                  .male_writer = _min_string(tmp12),
                                  .violent_movie_title = _min_string(tmp15)}};
  int result_len = sizeof(result) / sizeof(result[0]);
  printf("[");
  for (int i18 = 0; i18 < result_len; i18++) {
    if (i18 > 0)
      printf(",");
    result_t it = result[i18];
    printf("{");
    _json_string("movie_budget");
    printf(":");
    _json_string(it.movie_budget);
    printf(",");
    _json_string("movie_votes");
    printf(":");
    _json_int(it.movie_votes);
    printf(",");
    _json_string("male_writer");
    printf(":");
    _json_string(it.male_writer);
    printf(",");
    _json_string("violent_movie_title");
    printf(":");
    _json_string(it.violent_movie_title);
    printf("}");
  }
  printf("]");
  test_Q25_finds_male_horror_writer_with_violent_keywords_result = result;
  test_Q25_finds_male_horror_writer_with_violent_keywords();
  free(matches.data);
  free(tmp6.data);
  free(tmp9.data);
  free(tmp12.data);
  free(tmp15.data);
  return 0;
}
