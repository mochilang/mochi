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
  const char *complete_violent_movie;
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
  const char *budget;
  int votes;
  const char *writer;
  const char *movie;
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
  const char *complete_violent_movie;
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
    test_Q30_finds_violent_horror_thriller_movies_with_male_writer_result;
static void test_Q30_finds_violent_horror_thriller_movies_with_male_writer() {
  tmp1_t tmp1[] = {(tmp1_t){.movie_budget = "Horror",
                            .movie_votes = 2000,
                            .writer = "John Writer",
                            .complete_violent_movie = "Violent Horror"}};
  int tmp1_len = sizeof(tmp1) / sizeof(tmp1[0]);
  int tmp2 = 1;
  if (test_Q30_finds_violent_horror_thriller_movies_with_male_writer_result
          .len != tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0;
         i3 <
         test_Q30_finds_violent_horror_thriller_movies_with_male_writer_result
             .len;
         i3++) {
      if (test_Q30_finds_violent_horror_thriller_movies_with_male_writer_result
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
  comp_cast_type_t comp_cast_type[] = {
      (comp_cast_type_t){.id = 1, .kind = "cast"},
      (comp_cast_type_t){.id = 2, .kind = "complete+verified"},
      (comp_cast_type_t){.id = 3, .kind = "crew"}};
  int comp_cast_type_len = sizeof(comp_cast_type) / sizeof(comp_cast_type[0]);
  complete_cast_t complete_cast[] = {
      (complete_cast_t){.movie_id = 1, .subject_id = 1, .status_id = 2},
      (complete_cast_t){.movie_id = 2, .subject_id = 3, .status_id = 2}};
  int complete_cast_len = sizeof(complete_cast) / sizeof(complete_cast[0]);
  cast_info_t cast_info[] = {
      (cast_info_t){.movie_id = 1, .person_id = 10, .note = "(writer)"},
      (cast_info_t){.movie_id = 2, .person_id = 11, .note = "(actor)"}};
  int cast_info_len = sizeof(cast_info) / sizeof(cast_info[0]);
  info_type_t info_type[] = {(info_type_t){.id = 1, .info = "genres"},
                             (info_type_t){.id = 2, .info = "votes"}};
  int info_type_len = sizeof(info_type) / sizeof(info_type[0]);
  keyword_t keyword[] = {(keyword_t){.id = 1, .keyword = "murder"},
                         (keyword_t){.id = 2, .keyword = "comedy"}};
  int keyword_len = sizeof(keyword) / sizeof(keyword[0]);
  movie_info_t movie_info[] = {
      (movie_info_t){.movie_id = 1, .info_type_id = 1, .info = "Horror"},
      (movie_info_t){.movie_id = 2, .info_type_id = 1, .info = "Comedy"}};
  int movie_info_len = sizeof(movie_info) / sizeof(movie_info[0]);
  movie_info_idx_t movie_info_idx[] = {
      (movie_info_idx_t){.movie_id = 1, .info_type_id = 2, .info = 2000},
      (movie_info_idx_t){.movie_id = 2, .info_type_id = 2, .info = 150}};
  int movie_info_idx_len = sizeof(movie_info_idx) / sizeof(movie_info_idx[0]);
  movie_keyword_t movie_keyword[] = {
      (movie_keyword_t){.movie_id = 1, .keyword_id = 1},
      (movie_keyword_t){.movie_id = 2, .keyword_id = 2}};
  int movie_keyword_len = sizeof(movie_keyword) / sizeof(movie_keyword[0]);
  name_t name[] = {(name_t){.id = 10, .name = "John Writer", .gender = "m"},
                   (name_t){.id = 11, .name = "Jane Actor", .gender = "f"}};
  int name_len = sizeof(name) / sizeof(name[0]);
  title_t title[] = {
      (title_t){.id = 1, .title = "Violent Horror", .production_year = 2005},
      (title_t){.id = 2, .title = "Old Comedy", .production_year = 1995}};
  int title_len = sizeof(title) / sizeof(title[0]);
  list_string violent_keywords = list_string_create(7);
  violent_keywords.data[0] = "murder";
  violent_keywords.data[1] = "violence";
  violent_keywords.data[2] = "blood";
  violent_keywords.data[3] = "gore";
  violent_keywords.data[4] = "death";
  violent_keywords.data[5] = "female-nudity";
  violent_keywords.data[6] = "hospital";
  int violent_keywords = violent_keywords;
  list_string writer_notes = list_string_create(5);
  writer_notes.data[0] = "(writer)";
  writer_notes.data[1] = "(head writer)";
  writer_notes.data[2] = "(written by)";
  writer_notes.data[3] = "(story)";
  writer_notes.data[4] = "(story editor)";
  int writer_notes = writer_notes;
  list_string matches = list_string_create(2);
  matches.data[0] = "cast";
  matches.data[1] = "crew";
  list_string matches = list_string_create(2);
  matches.data[0] = "Horror";
  matches.data[1] = "Thriller";
  matches_item_list_t tmp4 = matches_item_list_t_create(
      complete_cast.len * comp_cast_type.len * comp_cast_type.len *
      cast_info.len * movie_info.len * movie_info_idx.len * movie_keyword.len *
      info_type.len * info_type.len * keyword.len * name.len * title.len);
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
          for (int tmp10 = 0; tmp10 < movie_info_len; tmp10++) {
            movie_info_t mi = movie_info[tmp10];
            if (!(mi.movie_id == cc.movie_id)) {
              continue;
            }
            for (int tmp11 = 0; tmp11 < movie_info_idx_len; tmp11++) {
              movie_info_idx_t mi_idx = movie_info_idx[tmp11];
              if (!(mi_idx.movie_id == cc.movie_id)) {
                continue;
              }
              for (int tmp12 = 0; tmp12 < movie_keyword_len; tmp12++) {
                movie_keyword_t mk = movie_keyword[tmp12];
                if (!(mk.movie_id == cc.movie_id)) {
                  continue;
                }
                for (int tmp13 = 0; tmp13 < info_type_len; tmp13++) {
                  info_type_t it1 = info_type[tmp13];
                  if (!(it1.id == mi.info_type_id)) {
                    continue;
                  }
                  for (int tmp14 = 0; tmp14 < info_type_len; tmp14++) {
                    info_type_t it2 = info_type[tmp14];
                    if (!(it2.id == mi_idx.info_type_id)) {
                      continue;
                    }
                    for (int tmp15 = 0; tmp15 < keyword_len; tmp15++) {
                      keyword_t k = keyword[tmp15];
                      if (!(k.id == mk.keyword_id)) {
                        continue;
                      }
                      for (int tmp16 = 0; tmp16 < name_len; tmp16++) {
                        name_t n = name[tmp16];
                        if (!(n.id == ci.person_id)) {
                          continue;
                        }
                        for (int tmp17 = 0; tmp17 < title_len; tmp17++) {
                          title_t t = title[tmp17];
                          if (!(t.id == cc.movie_id)) {
                            continue;
                          }
                          if (!((contains_list_string(matches, cct1.kind)) &&
                                cct2.kind == "complete+verified" &&
                                (contains_list_string(writer_notes, ci.note)) &&
                                it1.info == "genres" && it2.info == "votes" &&
                                (contains_list_string(violent_keywords,
                                                      k.keyword)) &&
                                (contains_list_string(matches, mi.info)) &&
                                n.gender == "m" && t.production_year > 2000)) {
                            continue;
                          }
                          tmp4.data[tmp5] =
                              (matches_item_t){.budget = mi.info,
                                               .votes = mi_idx.info,
                                               .writer = n.name,
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
  matches_item_list_t matches = tmp4;
  int tmp18 = int_create(matches.len);
  int tmp19 = 0;
  for (int tmp20 = 0; tmp20 < matches.len; tmp20++) {
    matches_item_t x = matches.data[tmp20];
    tmp18.data[tmp19] = x.budget;
    tmp19++;
  }
  tmp18.len = tmp19;
  list_int tmp21 = list_int_create(matches.len);
  int tmp22 = 0;
  for (int tmp23 = 0; tmp23 < matches.len; tmp23++) {
    matches_item_t x = matches.data[tmp23];
    tmp21.data[tmp22] = x.votes;
    tmp22++;
  }
  tmp21.len = tmp22;
  int tmp24 = int_create(matches.len);
  int tmp25 = 0;
  for (int tmp26 = 0; tmp26 < matches.len; tmp26++) {
    matches_item_t x = matches.data[tmp26];
    tmp24.data[tmp25] = x.writer;
    tmp25++;
  }
  tmp24.len = tmp25;
  int tmp27 = int_create(matches.len);
  int tmp28 = 0;
  for (int tmp29 = 0; tmp29 < matches.len; tmp29++) {
    matches_item_t x = matches.data[tmp29];
    tmp27.data[tmp28] = x.movie;
    tmp28++;
  }
  tmp27.len = tmp28;
  result_t result[] = {
      (result_t){.movie_budget = _min_string(tmp18),
                 .movie_votes = _min_int(tmp21),
                 .writer = _min_string(tmp24),
                 .complete_violent_movie = _min_string(tmp27)}};
  int result_len = sizeof(result) / sizeof(result[0]);
  printf("[");
  for (int i30 = 0; i30 < result_len; i30++) {
    if (i30 > 0)
      printf(",");
    result_t it = result[i30];
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
    _json_string("complete_violent_movie");
    printf(":");
    _json_string(it.complete_violent_movie);
    printf("}");
  }
  printf("]");
  test_Q30_finds_violent_horror_thriller_movies_with_male_writer_result =
      result;
  test_Q30_finds_violent_horror_thriller_movies_with_male_writer();
  free(tmp18.data);
  free(tmp21.data);
  free(tmp24.data);
  free(tmp27.data);
  return 0;
}
