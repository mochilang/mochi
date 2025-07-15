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
  const char *release_date;
  const char *rating;
  const char *german_movie;
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
  int id;
  int kind_id;
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
  int movie_id;
  int company_id;
  int company_type_id;
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
  const char *info;
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
  const char *release_date;
  const char *rating;
  const char *german_movie;
} candidates_item_t;
typedef struct {
  int len;
  candidates_item_t *data;
} candidates_item_list_t;
candidates_item_list_t create_candidates_item_list(int len) {
  candidates_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(candidates_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  const char *release_date;
  const char *rating;
  const char *german_movie;
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

static int test_Q13_finds_earliest_German_movie_info_result;
static void test_Q13_finds_earliest_German_movie_info() {
  if (!(test_Q13_finds_earliest_German_movie_info_result ==
        (tmp_item_t){.release_date = "1997-05-10",
                     .rating = "6.0",
                     .german_movie = "Alpha"})) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  company_name_t company_name[] = {
      (company_name_t){.id = 1, .country_code = "[de]"},
      (company_name_t){.id = 2, .country_code = "[us]"}};
  int company_name_len = sizeof(company_name) / sizeof(company_name[0]);
  company_type_t company_type[] = {
      (company_type_t){.id = 1, .kind = "production companies"},
      (company_type_t){.id = 2, .kind = "distributors"}};
  int company_type_len = sizeof(company_type) / sizeof(company_type[0]);
  info_type_t info_type[] = {(info_type_t){.id = 1, .info = "rating"},
                             (info_type_t){.id = 2, .info = "release dates"}};
  int info_type_len = sizeof(info_type) / sizeof(info_type[0]);
  kind_type_t kind_type[] = {(kind_type_t){.id = 1, .kind = "movie"},
                             (kind_type_t){.id = 2, .kind = "video"}};
  int kind_type_len = sizeof(kind_type) / sizeof(kind_type[0]);
  title_t title[] = {(title_t){.id = 10, .kind_id = 1, .title = "Alpha"},
                     (title_t){.id = 20, .kind_id = 1, .title = "Beta"},
                     (title_t){.id = 30, .kind_id = 2, .title = "Gamma"}};
  int title_len = sizeof(title) / sizeof(title[0]);
  movie_companie_t movie_companies[] = {
      (movie_companie_t){.movie_id = 10, .company_id = 1, .company_type_id = 1},
      (movie_companie_t){.movie_id = 20, .company_id = 1, .company_type_id = 1},
      (movie_companie_t){
          .movie_id = 30, .company_id = 2, .company_type_id = 1}};
  int movie_companies_len =
      sizeof(movie_companies) / sizeof(movie_companies[0]);
  movie_info_t movie_info[] = {
      (movie_info_t){.movie_id = 10, .info_type_id = 2, .info = "1997-05-10"},
      (movie_info_t){.movie_id = 20, .info_type_id = 2, .info = "1998-03-20"},
      (movie_info_t){.movie_id = 30, .info_type_id = 2, .info = "1999-07-30"}};
  int movie_info_len = sizeof(movie_info) / sizeof(movie_info[0]);
  movie_info_idx_t movie_info_idx[] = {
      (movie_info_idx_t){.movie_id = 10, .info_type_id = 1, .info = "6.0"},
      (movie_info_idx_t){.movie_id = 20, .info_type_id = 1, .info = "7.5"},
      (movie_info_idx_t){.movie_id = 30, .info_type_id = 1, .info = "5.5"}};
  int movie_info_idx_len = sizeof(movie_info_idx) / sizeof(movie_info_idx[0]);
  candidates_item_list_t tmp1 = candidates_item_list_t_create(
      company_name.len * movie_companies.len * company_type.len * title.len *
      kind_type.len * movie_info.len * info_type.len * movie_info_idx.len *
      info_type.len);
  int tmp2 = 0;
  for (int tmp3 = 0; tmp3 < company_name_len; tmp3++) {
    company_name_t cn = company_name[tmp3];
    for (int tmp4 = 0; tmp4 < movie_companies_len; tmp4++) {
      movie_companie_t mc = movie_companies[tmp4];
      if (!(mc.company_id == cn.id)) {
        continue;
      }
      for (int tmp5 = 0; tmp5 < company_type_len; tmp5++) {
        company_type_t ct = company_type[tmp5];
        if (!(ct.id == mc.company_type_id)) {
          continue;
        }
        for (int tmp6 = 0; tmp6 < title_len; tmp6++) {
          title_t t = title[tmp6];
          if (!(t.id == mc.movie_id)) {
            continue;
          }
          for (int tmp7 = 0; tmp7 < kind_type_len; tmp7++) {
            kind_type_t kt = kind_type[tmp7];
            if (!(kt.id == t.kind_id)) {
              continue;
            }
            for (int tmp8 = 0; tmp8 < movie_info_len; tmp8++) {
              movie_info_t mi = movie_info[tmp8];
              if (!(mi.movie_id == t.id)) {
                continue;
              }
              for (int tmp9 = 0; tmp9 < info_type_len; tmp9++) {
                info_type_t it2 = info_type[tmp9];
                if (!(it2.id == mi.info_type_id)) {
                  continue;
                }
                for (int tmp10 = 0; tmp10 < movie_info_idx_len; tmp10++) {
                  movie_info_idx_t miidx = movie_info_idx[tmp10];
                  if (!(miidx.movie_id == t.id)) {
                    continue;
                  }
                  for (int tmp11 = 0; tmp11 < info_type_len; tmp11++) {
                    info_type_t it = info_type[tmp11];
                    if (!(it.id == miidx.info_type_id)) {
                      continue;
                    }
                    if (!((strcmp(cn.country_code, "[de]") == 0) &&
                          ct.kind == "production companies" &&
                          it.info == "rating" && it2.info == "release dates" &&
                          kt.kind == "movie")) {
                      continue;
                    }
                    tmp1.data[tmp2] =
                        (candidates_item_t){.release_date = mi.info,
                                            .rating = miidx.info,
                                            .german_movie = t.title};
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
  tmp1.len = tmp2;
  candidates_item_list_t candidates = tmp1;
  int tmp12 = int_create(candidates.len);
  const char **tmp15 =
      (const char **)malloc(sizeof(const char *) * candidates.len);
  int tmp13 = 0;
  for (int tmp14 = 0; tmp14 < candidates.len; tmp14++) {
    candidates_item_t x = candidates.data[tmp14];
    tmp12.data[tmp13] = x.release_date;
    tmp15[tmp13] = x.release_date;
    tmp13++;
  }
  tmp12.len = tmp13;
  for (int i18 = 0; i18 < tmp13 - 1; i18++) {
    for (int i19 = i18 + 1; i19 < tmp13; i19++) {
      if (tmp15[i18] > tmp15[i19]) {
        const char *tmp16 = tmp15[i18];
        tmp15[i18] = tmp15[i19];
        tmp15[i19] = tmp16;
        const char *tmp17 = tmp12.data[i18];
        tmp12.data[i18] = tmp12.data[i19];
        tmp12.data[i19] = tmp17;
      }
    }
  }
  int tmp20 = int_create(candidates.len);
  const char **tmp23 =
      (const char **)malloc(sizeof(const char *) * candidates.len);
  int tmp21 = 0;
  for (int tmp22 = 0; tmp22 < candidates.len; tmp22++) {
    candidates_item_t x = candidates.data[tmp22];
    tmp20.data[tmp21] = x.rating;
    tmp23[tmp21] = x.rating;
    tmp21++;
  }
  tmp20.len = tmp21;
  for (int i26 = 0; i26 < tmp21 - 1; i26++) {
    for (int i27 = i26 + 1; i27 < tmp21; i27++) {
      if (tmp23[i26] > tmp23[i27]) {
        const char *tmp24 = tmp23[i26];
        tmp23[i26] = tmp23[i27];
        tmp23[i27] = tmp24;
        const char *tmp25 = tmp20.data[i26];
        tmp20.data[i26] = tmp20.data[i27];
        tmp20.data[i27] = tmp25;
      }
    }
  }
  int tmp28 = int_create(candidates.len);
  const char **tmp31 =
      (const char **)malloc(sizeof(const char *) * candidates.len);
  int tmp29 = 0;
  for (int tmp30 = 0; tmp30 < candidates.len; tmp30++) {
    candidates_item_t x = candidates.data[tmp30];
    tmp28.data[tmp29] = x.german_movie;
    tmp31[tmp29] = x.german_movie;
    tmp29++;
  }
  tmp28.len = tmp29;
  for (int i34 = 0; i34 < tmp29 - 1; i34++) {
    for (int i35 = i34 + 1; i35 < tmp29; i35++) {
      if (tmp31[i34] > tmp31[i35]) {
        const char *tmp32 = tmp31[i34];
        tmp31[i34] = tmp31[i35];
        tmp31[i35] = tmp32;
        const char *tmp33 = tmp28.data[i34];
        tmp28.data[i34] = tmp28.data[i35];
        tmp28.data[i35] = tmp33;
      }
    }
  }
  result_item_t result = (result_item_t){.release_date = (tmp12).data[0],
                                         .rating = (tmp20).data[0],
                                         .german_movie = (tmp28).data[0]};
  printf("{");
  _json_string("release_date");
  printf(":");
  _json_string(result.release_date);
  printf(",");
  _json_string("rating");
  printf(":");
  _json_string(result.rating);
  printf(",");
  _json_string("german_movie");
  printf(":");
  _json_string(result.german_movie);
  printf("}");
  test_Q13_finds_earliest_German_movie_info_result = result;
  test_Q13_finds_earliest_German_movie_info();
  free(tmp12.data);
  free(tmp20.data);
  free(tmp28.data);
  return 0;
}
