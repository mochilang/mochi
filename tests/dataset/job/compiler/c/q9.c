
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
  char *alternative_name;
  char *character_name;
  char *movie;
} TmpItem;
typedef struct {
  int len;
  TmpItem *data;
} list_TmpItem;
static list_TmpItem list_TmpItem_create(int len) {
  list_TmpItem l;
  l.len = len;
  l.data = calloc(len, sizeof(TmpItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int person_id;
  char *name;
} Aka_nameItem;
typedef struct {
  int len;
  Aka_nameItem *data;
} list_Aka_nameItem;
static list_Aka_nameItem list_Aka_nameItem_create(int len) {
  list_Aka_nameItem l;
  l.len = len;
  l.data = calloc(len, sizeof(Aka_nameItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  char *name;
} Char_nameItem;
typedef struct {
  int len;
  Char_nameItem *data;
} list_Char_nameItem;
static list_Char_nameItem list_Char_nameItem_create(int len) {
  list_Char_nameItem l;
  l.len = len;
  l.data = calloc(len, sizeof(Char_nameItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int person_id;
  int person_role_id;
  int movie_id;
  int role_id;
  char *note;
} Cast_infoItem;
typedef struct {
  int len;
  Cast_infoItem *data;
} list_Cast_infoItem;
static list_Cast_infoItem list_Cast_infoItem_create(int len) {
  list_Cast_infoItem l;
  l.len = len;
  l.data = calloc(len, sizeof(Cast_infoItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  char *country_code;
} Company_nameItem;
typedef struct {
  int len;
  Company_nameItem *data;
} list_Company_nameItem;
static list_Company_nameItem list_Company_nameItem_create(int len) {
  list_Company_nameItem l;
  l.len = len;
  l.data = calloc(len, sizeof(Company_nameItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int movie_id;
  int company_id;
  char *note;
} Movie_companiesItem;
typedef struct {
  int len;
  Movie_companiesItem *data;
} list_Movie_companiesItem;
static list_Movie_companiesItem list_Movie_companiesItem_create(int len) {
  list_Movie_companiesItem l;
  l.len = len;
  l.data = calloc(len, sizeof(Movie_companiesItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  char *name;
  char *gender;
} NameItem;
typedef struct {
  int len;
  NameItem *data;
} list_NameItem;
static list_NameItem list_NameItem_create(int len) {
  list_NameItem l;
  l.len = len;
  l.data = calloc(len, sizeof(NameItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  char *role;
} Role_typeItem;
typedef struct {
  int len;
  Role_typeItem *data;
} list_Role_typeItem;
static list_Role_typeItem list_Role_typeItem_create(int len) {
  list_Role_typeItem l;
  l.len = len;
  l.data = calloc(len, sizeof(Role_typeItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  char *title;
  int production_year;
} TitleItem;
typedef struct {
  int len;
  TitleItem *data;
} list_TitleItem;
static list_TitleItem list_TitleItem_create(int len) {
  list_TitleItem l;
  l.len = len;
  l.data = calloc(len, sizeof(TitleItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  char *alt;
  char *character;
  char *movie;
} MatchesItem;
typedef struct {
  int len;
  MatchesItem *data;
} list_MatchesItem;
static list_MatchesItem list_MatchesItem_create(int len) {
  list_MatchesItem l;
  l.len = len;
  l.data = calloc(len, sizeof(MatchesItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  char *alternative_name;
  char *character_name;
  char *movie;
} ResultItem;
typedef struct {
  int len;
  ResultItem *data;
} list_ResultItem;
static list_ResultItem list_ResultItem_create(int len) {
  list_ResultItem l;
  l.len = len;
  l.data = calloc(len, sizeof(ResultItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

static list_int
    test_Q9_selects_minimal_alternative_name__character_and_movie_result;
static void test_Q9_selects_minimal_alternative_name__character_and_movie() {
  list_int tmp1 = list_int_create(1);
  tmp1.data[0] = (TmpItem){.alternative_name = "A. N. G.",
                           .character_name = "Angel",
                           .movie = "Famous Film"};
  int tmp2 = 1;
  if (test_Q9_selects_minimal_alternative_name__character_and_movie_result
          .len != tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0;
         i3 <
         test_Q9_selects_minimal_alternative_name__character_and_movie_result
             .len;
         i3++) {
      if (test_Q9_selects_minimal_alternative_name__character_and_movie_result
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
  Aka_nameItem tmp4_data[] = {
      (Aka_nameItem){.person_id = 1, .name = "A. N. G."},
      (Aka_nameItem){.person_id = 2, .name = "J. D."}};
  list_Aka_nameItem tmp4 = {2, tmp4_data};
  list_Aka_nameItem aka_name = tmp4;
  Char_nameItem tmp5_data[] = {(Char_nameItem){.id = 10, .name = "Angel"},
                               (Char_nameItem){.id = 20, .name = "Devil"}};
  list_Char_nameItem tmp5 = {2, tmp5_data};
  list_Char_nameItem char_name = tmp5;
  Cast_infoItem tmp6_data[] = {(Cast_infoItem){.person_id = 1,
                                               .person_role_id = 10,
                                               .movie_id = 100,
                                               .role_id = 1000,
                                               .note = "(voice)"},
                               (Cast_infoItem){.person_id = 2,
                                               .person_role_id = 20,
                                               .movie_id = 200,
                                               .role_id = 1000,
                                               .note = "(voice)"}};
  list_Cast_infoItem tmp6 = {2, tmp6_data};
  list_Cast_infoItem cast_info = tmp6;
  Company_nameItem tmp7_data[] = {
      (Company_nameItem){.id = 100, .country_code = "[us]"},
      (Company_nameItem){.id = 200, .country_code = "[gb]"}};
  list_Company_nameItem tmp7 = {2, tmp7_data};
  list_Company_nameItem company_name = tmp7;
  Movie_companiesItem tmp8_data[] = {
      (Movie_companiesItem){
          .movie_id = 100, .company_id = 100, .note = "ACME Studios (USA)"},
      (Movie_companiesItem){
          .movie_id = 200, .company_id = 200, .note = "Maple Films"}};
  list_Movie_companiesItem tmp8 = {2, tmp8_data};
  list_Movie_companiesItem movie_companies = tmp8;
  NameItem tmp9_data[] = {
      (NameItem){.id = 1, .name = "Angela Smith", .gender = "f"},
      (NameItem){.id = 2, .name = "John Doe", .gender = "m"}};
  list_NameItem tmp9 = {2, tmp9_data};
  list_NameItem name = tmp9;
  Role_typeItem tmp10_data[] = {(Role_typeItem){.id = 1000, .role = "actress"},
                                (Role_typeItem){.id = 2000, .role = "actor"}};
  list_Role_typeItem tmp10 = {2, tmp10_data};
  list_Role_typeItem role_type = tmp10;
  TitleItem tmp11_data[] = {
      (TitleItem){.id = 100, .title = "Famous Film", .production_year = 2010},
      (TitleItem){.id = 200, .title = "Old Movie", .production_year = 1999}};
  list_TitleItem tmp11 = {2, tmp11_data};
  list_TitleItem title = tmp11;
  list_string tmp12 = list_string_create(4);
  tmp12.data[0] = "(voice)";
  tmp12.data[1] = "(voice: Japanese version)";
  tmp12.data[2] = "(voice) (uncredited)";
  tmp12.data[3] = "(voice: English version)";
  list_MatchesItem tmp13 = list_MatchesItem_create(
      aka_name.len * name.len * cast_info.len * char_name.len * title.len *
      movie_companies.len * company_name.len * role_type.len);
  int tmp14 = 0;
  for (int tmp15 = 0; tmp15 < aka_name.len; tmp15++) {
    Aka_nameItem an = aka_name.data[tmp15];
    for (int tmp16 = 0; tmp16 < name.len; tmp16++) {
      NameItem n = name.data[tmp16];
      if (!(an.person_id == n.id)) {
        continue;
      }
      for (int tmp17 = 0; tmp17 < cast_info.len; tmp17++) {
        Cast_infoItem ci = cast_info.data[tmp17];
        if (!(ci.person_id == n.id)) {
          continue;
        }
        for (int tmp18 = 0; tmp18 < char_name.len; tmp18++) {
          Char_nameItem chn = char_name.data[tmp18];
          if (!(chn.id == ci.person_role_id)) {
            continue;
          }
          for (int tmp19 = 0; tmp19 < title.len; tmp19++) {
            TitleItem t = title.data[tmp19];
            if (!(t.id == ci.movie_id)) {
              continue;
            }
            for (int tmp20 = 0; tmp20 < movie_companies.len; tmp20++) {
              Movie_companiesItem mc = movie_companies.data[tmp20];
              if (!(mc.movie_id == t.id)) {
                continue;
              }
              for (int tmp21 = 0; tmp21 < company_name.len; tmp21++) {
                Company_nameItem cn = company_name.data[tmp21];
                if (!(cn.id == mc.company_id)) {
                  continue;
                }
                for (int tmp22 = 0; tmp22 < role_type.len; tmp22++) {
                  Role_typeItem rt = role_type.data[tmp22];
                  if (!(rt.id == ci.role_id)) {
                    continue;
                  }
                  if (!((contains_list_string(tmp12, ci.note)) &&
                        cn.country_code == "[us]" &&
                        (contains_string(mc.note, "(USA)") ||
                         contains_string(mc.note, "(worldwide)")) &&
                        n.gender == "f" && contains_string(n.name, "Ang") &&
                        rt.role == "actress" && t.production_year >= 2005 &&
                        t.production_year <= 2015)) {
                    continue;
                  }
                  tmp13.data[tmp14] = (MatchesItem){
                      .alt = an.name, .character = chn.name, .movie = t.title};
                  tmp14++;
                }
              }
            }
          }
        }
      }
    }
  }
  tmp13.len = tmp14;
  list_MatchesItem matches = tmp13;
  list_string tmp24 = list_string_create(matches.len);
  int tmp25 = 0;
  for (int tmp26 = 0; tmp26 < matches.len; tmp26++) {
    MatchesItem x = matches.data[tmp26];
    tmp24.data[tmp25] = x.alt;
    tmp25++;
  }
  tmp24.len = tmp25;
  list_string tmp27 = list_string_create(matches.len);
  int tmp28 = 0;
  for (int tmp29 = 0; tmp29 < matches.len; tmp29++) {
    MatchesItem x = matches.data[tmp29];
    tmp27.data[tmp28] = x.character;
    tmp28++;
  }
  tmp27.len = tmp28;
  list_string tmp30 = list_string_create(matches.len);
  int tmp31 = 0;
  for (int tmp32 = 0; tmp32 < matches.len; tmp32++) {
    MatchesItem x = matches.data[tmp32];
    tmp30.data[tmp31] = x.movie;
    tmp31++;
  }
  tmp30.len = tmp31;
  ResultItem tmp23_data[] = {
      (ResultItem){.alternative_name = _min_string(tmp24),
                   .character_name = _min_string(tmp27),
                   .movie = _min_string(tmp30)}};
  list_ResultItem tmp23 = {1, tmp23_data};
  list_ResultItem result = tmp23;
  printf("[");
  for (int i33 = 0; i33 < result.len; i33++) {
    if (i33 > 0)
      printf(",");
    ResultItem it = result.data[i33];
    printf("{");
    _json_string("alternative_name");
    printf(":");
    _json_string(it.alternative_name);
    printf(",");
    _json_string("character_name");
    printf(":");
    _json_string(it.character_name);
    printf(",");
    _json_string("movie");
    printf(":");
    _json_string(it.movie);
    printf("}");
  }
  printf("]");
  test_Q9_selects_minimal_alternative_name__character_and_movie_result = result;
  test_Q9_selects_minimal_alternative_name__character_and_movie();
  return 0;
}
