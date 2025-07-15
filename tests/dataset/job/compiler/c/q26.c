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
static double _min_float(list_float v) {
  if (v.len == 0)
    return 0;
  double m = v.data[0];
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
  const char *character_name;
  double rating;
  const char *playing_actor;
  const char *complete_hero_movie;
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
  int person_role_id;
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
  int id;
  const char *kind;
} kind_type_t;
typedef struct {
  int len;
  kind_type_t *data;
} kind_type_list_t;
kind_type_list_t create_kind_type_list(int len) {
  kind_type_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(kind_type_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int movie_id;
  int info_type_id;
  double info;
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
  int kind_id;
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
  const char *character;
  double rating;
  const char *actor;
  const char *movie;
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
  const char *character_name;
  double rating;
  const char *playing_actor;
  const char *complete_hero_movie;
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

static list_int test_Q26_finds_hero_movies_with_rating_above_7_result;
static void test_Q26_finds_hero_movies_with_rating_above_7() {
  tmp1_t tmp1[] = {(tmp1_t){.character_name = "Spider-Man",
                            .rating = 8.5,
                            .playing_actor = "Actor One",
                            .complete_hero_movie = "Hero Movie"}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_Q26_finds_hero_movies_with_rating_above_7_result.len != tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0;
         i3 < test_Q26_finds_hero_movies_with_rating_above_7_result.len; i3++) {
      if (test_Q26_finds_hero_movies_with_rating_above_7_result.data[i3] !=
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
  complete_cast_t complete_cast[] = {
      (complete_cast_t){.movie_id = 1, .subject_id = 1, .status_id = 2},
      (complete_cast_t){.movie_id = 2, .subject_id = 1, .status_id = 2}};
  int complete_cast_len = sizeof(complete_cast) / sizeof(complete_cast[0]);
  comp_cast_type_t comp_cast_type[] = {
      (comp_cast_type_t){.id = 1, .kind = "cast"},
      (comp_cast_type_t){.id = 2, .kind = "complete"}};
  int comp_cast_type_len = sizeof(comp_cast_type) / sizeof(comp_cast_type[0]);
  char_name_t char_name[] = {(char_name_t){.id = 1, .name = "Spider-Man"},
                             (char_name_t){.id = 2, .name = "Villain"}};
  int char_name_len = sizeof(char_name) / sizeof(char_name[0]);
  cast_info_t cast_info[] = {
      (cast_info_t){.movie_id = 1, .person_role_id = 1, .person_id = 1},
      (cast_info_t){.movie_id = 2, .person_role_id = 2, .person_id = 2}};
  int cast_info_len = sizeof(cast_info) / sizeof(cast_info[0]);
  info_type_t info_type[] = {(info_type_t){.id = 1, .info = "rating"}};
  int info_type_len = sizeof(info_type) / sizeof(info_type[0]);
  keyword_t keyword[] = {(keyword_t){.id = 1, .keyword = "superhero"},
                         (keyword_t){.id = 2, .keyword = "comedy"}};
  int keyword_len = sizeof(keyword) / sizeof(keyword[0]);
  kind_type_t kind_type[] = {(kind_type_t){.id = 1, .kind = "movie"}};
  int kind_type_len = sizeof(kind_type) / sizeof(kind_type[0]);
  movie_info_idx_t movie_info_idx[] = {
      (movie_info_idx_t){.movie_id = 1, .info_type_id = 1, .info = 8.5},
      (movie_info_idx_t){.movie_id = 2, .info_type_id = 1, .info = 6.5}};
  int movie_info_idx_len = sizeof(movie_info_idx) / sizeof(movie_info_idx[0]);
  movie_keyword_t movie_keyword[] = {
      (movie_keyword_t){.movie_id = 1, .keyword_id = 1},
      (movie_keyword_t){.movie_id = 2, .keyword_id = 2}};
  int movie_keyword_len = sizeof(movie_keyword) / sizeof(movie_keyword[0]);
  name_t name[] = {(name_t){.id = 1, .name = "Actor One"},
                   (name_t){.id = 2, .name = "Actor Two"}};
  int name_len = sizeof(name) / sizeof(name[0]);
  title_t title[] = {
      (title_t){.id = 1,
                .kind_id = 1,
                .production_year = 2005,
                .title = "Hero Movie"},
      (title_t){
          .id = 2, .kind_id = 1, .production_year = 1999, .title = "Old Film"}};
  int title_len = sizeof(title) / sizeof(title[0]);
  list_string allowed_keywords = list_string_create(10);
  allowed_keywords.data[0] = "superhero";
  allowed_keywords.data[1] = "marvel-comics";
  allowed_keywords.data[2] = "based-on-comic";
  allowed_keywords.data[3] = "tv-special";
  allowed_keywords.data[4] = "fight";
  allowed_keywords.data[5] = "violence";
  allowed_keywords.data[6] = "magnet";
  allowed_keywords.data[7] = "web";
  allowed_keywords.data[8] = "claw";
  allowed_keywords.data[9] = "laser";
  int allowed_keywords = allowed_keywords;
  rows_item_list_t tmp4 = rows_item_list_t_create(
      complete_cast.len * comp_cast_type.len * comp_cast_type.len *
      cast_info.len * char_name.len * name.len * title.len * kind_type.len *
      movie_keyword.len * keyword.len * movie_info_idx.len * info_type.len);
  int tmp5 = 0;
  for (int tmp6 = 0; tmp6 < complete_cast_len; tmp6++) {
    complete_cast_t cc = complete_cast[tmp6];
    for (int tmp7 = 0; tmp7 < comp_cast_type_len; tmp7++) {
      comp_cast_type_t cct1 = comp_cast_type[tmp7];
      if (!(cct1.id == cc.subject_id)) {
        continue;
      }
      for (int tmp8 = 0; tmp8 < comp_cast_type_len; tmp8++) {
        comp_cast_type_t cct2 = comp_cast_type[tmp8];
        if (!(cct2.id == cc.status_id)) {
          continue;
        }
        for (int tmp9 = 0; tmp9 < cast_info_len; tmp9++) {
          cast_info_t ci = cast_info[tmp9];
          if (!(ci.movie_id == cc.movie_id)) {
            continue;
          }
          for (int tmp10 = 0; tmp10 < char_name_len; tmp10++) {
            char_name_t chn = char_name[tmp10];
            if (!(chn.id == ci.person_role_id)) {
              continue;
            }
            for (int tmp11 = 0; tmp11 < name_len; tmp11++) {
              name_t n = name[tmp11];
              if (!(n.id == ci.person_id)) {
                continue;
              }
              for (int tmp12 = 0; tmp12 < title_len; tmp12++) {
                title_t t = title[tmp12];
                if (!(t.id == ci.movie_id)) {
                  continue;
                }
                for (int tmp13 = 0; tmp13 < kind_type_len; tmp13++) {
                  kind_type_t kt = kind_type[tmp13];
                  if (!(kt.id == t.kind_id)) {
                    continue;
                  }
                  for (int tmp14 = 0; tmp14 < movie_keyword_len; tmp14++) {
                    movie_keyword_t mk = movie_keyword[tmp14];
                    if (!(mk.movie_id == t.id)) {
                      continue;
                    }
                    for (int tmp15 = 0; tmp15 < keyword_len; tmp15++) {
                      keyword_t k = keyword[tmp15];
                      if (!(k.id == mk.keyword_id)) {
                        continue;
                      }
                      for (int tmp16 = 0; tmp16 < movie_info_idx_len; tmp16++) {
                        movie_info_idx_t mi_idx = movie_info_idx[tmp16];
                        if (!(mi_idx.movie_id == t.id)) {
                          continue;
                        }
                        for (int tmp17 = 0; tmp17 < info_type_len; tmp17++) {
                          info_type_t it2 = info_type[tmp17];
                          if (!(it2.id == mi_idx.info_type_id)) {
                            continue;
                          }
                          if (!((strcmp(cct1.kind, "cast") == 0) &&
                                contains_string(cct2.kind, "complete") &&
                                chn.name != 0 &&
                                (contains_string(chn.name, "man") ||
                                 contains_string(chn.name, "Man")) &&
                                it2.info == "rating" &&
                                (contains_list_string(allowed_keywords,
                                                      k.keyword)) &&
                                kt.kind == "movie" && mi_idx.info > 7.0 &&
                                t.production_year > 2000)) {
                            continue;
                          }
                          tmp4.data[tmp5] = (rows_item_t){.character = chn.name,
                                                          .rating = mi_idx.info,
                                                          .actor = n.name,
                                                          .movie = t.title};
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
  rows_item_list_t rows = tmp4;
  int tmp18 = int_create(rows.len);
  int tmp19 = 0;
  for (int tmp20 = 0; tmp20 < rows.len; tmp20++) {
    rows_item_t r = rows.data[tmp20];
    tmp18.data[tmp19] = r.character;
    tmp19++;
  }
  tmp18.len = tmp19;
  list_float tmp21 = list_float_create(rows.len);
  int tmp22 = 0;
  for (int tmp23 = 0; tmp23 < rows.len; tmp23++) {
    rows_item_t r = rows.data[tmp23];
    tmp21.data[tmp22] = r.rating;
    tmp22++;
  }
  tmp21.len = tmp22;
  int tmp24 = int_create(rows.len);
  int tmp25 = 0;
  for (int tmp26 = 0; tmp26 < rows.len; tmp26++) {
    rows_item_t r = rows.data[tmp26];
    tmp24.data[tmp25] = r.actor;
    tmp25++;
  }
  tmp24.len = tmp25;
  int tmp27 = int_create(rows.len);
  int tmp28 = 0;
  for (int tmp29 = 0; tmp29 < rows.len; tmp29++) {
    rows_item_t r = rows.data[tmp29];
    tmp27.data[tmp28] = r.movie;
    tmp28++;
  }
  tmp27.len = tmp28;
  result_t result[] = {(result_t){.character_name = _min_string(tmp18),
                                  .rating = _min_float(tmp21),
                                  .playing_actor = _min_string(tmp24),
                                  .complete_hero_movie = _min_string(tmp27)}};
  int result_len = sizeof(result) / sizeof(result[0]);
  printf("[");
  for (int i30 = 0; i30 < result_len; i30++) {
    if (i30 > 0)
      printf(",");
    result_t it = result[i30];
    printf("{");
    _json_string("character_name");
    printf(":");
    _json_string(it.character_name);
    printf(",");
    _json_string("rating");
    printf(":");
    _json_float(it.rating);
    printf(",");
    _json_string("playing_actor");
    printf(":");
    _json_string(it.playing_actor);
    printf(",");
    _json_string("complete_hero_movie");
    printf(":");
    _json_string(it.complete_hero_movie);
    printf("}");
  }
  printf("]");
  test_Q26_finds_hero_movies_with_rating_above_7_result = result;
  test_Q26_finds_hero_movies_with_rating_above_7();
  free(tmp18.data);
  free(tmp21.data);
  free(tmp24.data);
  free(tmp27.data);
  return 0;
}
