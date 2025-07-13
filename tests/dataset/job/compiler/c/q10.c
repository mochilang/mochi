
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
  char *uncredited_voiced_character;
  char *russian_movie;
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
  int movie_id;
  int person_role_id;
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
  int id;
} Company_typeItem;
typedef struct {
  int len;
  Company_typeItem *data;
} list_Company_typeItem;
static list_Company_typeItem list_Company_typeItem_create(int len) {
  list_Company_typeItem l;
  l.len = len;
  l.data = calloc(len, sizeof(Company_typeItem));
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
  char *uncredited_voiced_character;
  char *russian_movie;
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

static list_int test_Q10_finds_uncredited_voice_actor_in_Russian_movie_result;
static void test_Q10_finds_uncredited_voice_actor_in_Russian_movie() {
  list_int tmp1 = list_int_create(1);
  tmp1.data[0] = (TmpItem){.uncredited_voiced_character = "Ivan",
                           .russian_movie = "Vodka Dreams"};
  int tmp2 = 1;
  if (test_Q10_finds_uncredited_voice_actor_in_Russian_movie_result.len !=
      tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0;
         i3 < test_Q10_finds_uncredited_voice_actor_in_Russian_movie_result.len;
         i3++) {
      if (test_Q10_finds_uncredited_voice_actor_in_Russian_movie_result
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
  Char_nameItem tmp4_data[] = {(Char_nameItem){.id = 1, .name = "Ivan"},
                               (Char_nameItem){.id = 2, .name = "Alex"}};
  list_Char_nameItem tmp4 = {2, tmp4_data};
  list_Char_nameItem char_name = tmp4;
  Cast_infoItem tmp5_data[] = {
      (Cast_infoItem){.movie_id = 10,
                      .person_role_id = 1,
                      .role_id = 1,
                      .note = "Soldier (voice) (uncredited)"},
      (Cast_infoItem){.movie_id = 11,
                      .person_role_id = 2,
                      .role_id = 1,
                      .note = "(voice)"}};
  list_Cast_infoItem tmp5 = {2, tmp5_data};
  list_Cast_infoItem cast_info = tmp5;
  Company_nameItem tmp6_data[] = {
      (Company_nameItem){.id = 1, .country_code = "[ru]"},
      (Company_nameItem){.id = 2, .country_code = "[us]"}};
  list_Company_nameItem tmp6 = {2, tmp6_data};
  list_Company_nameItem company_name = tmp6;
  Company_typeItem tmp7_data[] = {(Company_typeItem){.id = 1},
                                  (Company_typeItem){.id = 2}};
  list_Company_typeItem tmp7 = {2, tmp7_data};
  list_Company_typeItem company_type = tmp7;
  Movie_companiesItem tmp8_data[] = {
      (Movie_companiesItem){
          .movie_id = 10, .company_id = 1, .company_type_id = 1},
      (Movie_companiesItem){
          .movie_id = 11, .company_id = 2, .company_type_id = 1}};
  list_Movie_companiesItem tmp8 = {2, tmp8_data};
  list_Movie_companiesItem movie_companies = tmp8;
  Role_typeItem tmp9_data[] = {(Role_typeItem){.id = 1, .role = "actor"},
                               (Role_typeItem){.id = 2, .role = "director"}};
  list_Role_typeItem tmp9 = {2, tmp9_data};
  list_Role_typeItem role_type = tmp9;
  TitleItem tmp10_data[] = {
      (TitleItem){.id = 10, .title = "Vodka Dreams", .production_year = 2006},
      (TitleItem){.id = 11, .title = "Other Film", .production_year = 2004}};
  list_TitleItem tmp10 = {2, tmp10_data};
  list_TitleItem title = tmp10;
  list_MatchesItem tmp11 = list_MatchesItem_create(
      char_name.len * cast_info.len * role_type.len * title.len *
      movie_companies.len * company_name.len * company_type.len);
  int tmp12 = 0;
  for (int tmp13 = 0; tmp13 < char_name.len; tmp13++) {
    Char_nameItem chn = char_name.data[tmp13];
    for (int tmp14 = 0; tmp14 < cast_info.len; tmp14++) {
      Cast_infoItem ci = cast_info.data[tmp14];
      if (!(chn.id == ci.person_role_id)) {
        continue;
      }
      for (int tmp15 = 0; tmp15 < role_type.len; tmp15++) {
        Role_typeItem rt = role_type.data[tmp15];
        if (!(rt.id == ci.role_id)) {
          continue;
        }
        for (int tmp16 = 0; tmp16 < title.len; tmp16++) {
          TitleItem t = title.data[tmp16];
          if (!(t.id == ci.movie_id)) {
            continue;
          }
          for (int tmp17 = 0; tmp17 < movie_companies.len; tmp17++) {
            Movie_companiesItem mc = movie_companies.data[tmp17];
            if (!(mc.movie_id == t.id)) {
              continue;
            }
            for (int tmp18 = 0; tmp18 < company_name.len; tmp18++) {
              Company_nameItem cn = company_name.data[tmp18];
              if (!(cn.id == mc.company_id)) {
                continue;
              }
              for (int tmp19 = 0; tmp19 < company_type.len; tmp19++) {
                Company_typeItem ct = company_type.data[tmp19];
                if (!(ct.id == mc.company_type_id)) {
                  continue;
                }
                if (!(contains_string(ci.note, "(voice)") &&
                      contains_string(ci.note, "(uncredited)") &&
                      cn.country_code == "[ru]" && rt.role == "actor" &&
                      t.production_year > 2005)) {
                  continue;
                }
                tmp11.data[tmp12] =
                    (MatchesItem){.character = chn.name, .movie = t.title};
                tmp12++;
              }
            }
          }
        }
      }
    }
  }
  tmp11.len = tmp12;
  list_MatchesItem matches = tmp11;
  list_string tmp21 = list_string_create(matches.len);
  int tmp22 = 0;
  for (int tmp23 = 0; tmp23 < matches.len; tmp23++) {
    MatchesItem x = matches.data[tmp23];
    tmp21.data[tmp22] = x.character;
    tmp22++;
  }
  tmp21.len = tmp22;
  list_string tmp24 = list_string_create(matches.len);
  int tmp25 = 0;
  for (int tmp26 = 0; tmp26 < matches.len; tmp26++) {
    MatchesItem x = matches.data[tmp26];
    tmp24.data[tmp25] = x.movie;
    tmp25++;
  }
  tmp24.len = tmp25;
  ResultItem tmp20_data[] = {
      (ResultItem){.uncredited_voiced_character = _min_string(tmp21),
                   .russian_movie = _min_string(tmp24)}};
  list_ResultItem tmp20 = {1, tmp20_data};
  list_ResultItem result = tmp20;
  printf("[");
  for (int i27 = 0; i27 < result.len; i27++) {
    if (i27 > 0)
      printf(",");
    ResultItem it = result.data[i27];
    printf("{");
    _json_string("uncredited_voiced_character");
    printf(":");
    _json_string(it.uncredited_voiced_character);
    printf(",");
    _json_string("russian_movie");
    printf(":");
    _json_string(it.russian_movie);
    printf("}");
  }
  printf("]");
  test_Q10_finds_uncredited_voice_actor_in_Russian_movie_result = result;
  test_Q10_finds_uncredited_voice_actor_in_Russian_movie();
  return 0;
}
