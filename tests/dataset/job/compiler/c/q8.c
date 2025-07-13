
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
  char *actress_pseudonym;
  char *japanese_movie_dubbed;
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
  int person_id;
  int movie_id;
  char *note;
  int role_id;
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
  char *pseudonym;
  char *movie_title;
} EligibleItem;
typedef struct {
  int len;
  EligibleItem *data;
} list_EligibleItem;
static list_EligibleItem list_EligibleItem_create(int len) {
  list_EligibleItem l;
  l.len = len;
  l.data = calloc(len, sizeof(EligibleItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  char *actress_pseudonym;
  char *japanese_movie_dubbed;
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
    test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing_result;
static void
test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing() {
  list_int tmp1 = list_int_create(1);
  tmp1.data[0] = (TmpItem){.actress_pseudonym = "Y. S.",
                           .japanese_movie_dubbed = "Dubbed Film"};
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
  Aka_nameItem tmp4_data[] = {(Aka_nameItem){.person_id = 1, .name = "Y. S."}};
  list_Aka_nameItem tmp4 = {1, tmp4_data};
  list_Aka_nameItem aka_name = tmp4;
  Cast_infoItem tmp5_data[] = {
      (Cast_infoItem){.person_id = 1,
                      .movie_id = 10,
                      .note = "(voice: English version)",
                      .role_id = 1000}};
  list_Cast_infoItem tmp5 = {1, tmp5_data};
  list_Cast_infoItem cast_info = tmp5;
  Company_nameItem tmp6_data[] = {
      (Company_nameItem){.id = 50, .country_code = "[jp]"}};
  list_Company_nameItem tmp6 = {1, tmp6_data};
  list_Company_nameItem company_name = tmp6;
  Movie_companiesItem tmp7_data[] = {(Movie_companiesItem){
      .movie_id = 10, .company_id = 50, .note = "Studio (Japan)"}};
  list_Movie_companiesItem tmp7 = {1, tmp7_data};
  list_Movie_companiesItem movie_companies = tmp7;
  NameItem tmp8_data[] = {(NameItem){.id = 1, .name = "Yoko Ono"},
                          (NameItem){.id = 2, .name = "Yuichi"}};
  list_NameItem tmp8 = {2, tmp8_data};
  list_NameItem name = tmp8;
  Role_typeItem tmp9_data[] = {(Role_typeItem){.id = 1000, .role = "actress"}};
  list_Role_typeItem tmp9 = {1, tmp9_data};
  list_Role_typeItem role_type = tmp9;
  TitleItem tmp10_data[] = {(TitleItem){.id = 10, .title = "Dubbed Film"}};
  list_TitleItem tmp10 = {1, tmp10_data};
  list_TitleItem title = tmp10;
  list_EligibleItem tmp11 = list_EligibleItem_create(
      aka_name.len * name.len * cast_info.len * title.len *
      movie_companies.len * company_name.len * role_type.len);
  int tmp12 = 0;
  for (int tmp13 = 0; tmp13 < aka_name.len; tmp13++) {
    Aka_nameItem an1 = aka_name.data[tmp13];
    for (int tmp14 = 0; tmp14 < name.len; tmp14++) {
      NameItem n1 = name.data[tmp14];
      if (!(n1.id == an1.person_id)) {
        continue;
      }
      for (int tmp15 = 0; tmp15 < cast_info.len; tmp15++) {
        Cast_infoItem ci = cast_info.data[tmp15];
        if (!(ci.person_id == an1.person_id)) {
          continue;
        }
        for (int tmp16 = 0; tmp16 < title.len; tmp16++) {
          TitleItem t = title.data[tmp16];
          if (!(t.id == ci.movie_id)) {
            continue;
          }
          for (int tmp17 = 0; tmp17 < movie_companies.len; tmp17++) {
            Movie_companiesItem mc = movie_companies.data[tmp17];
            if (!(mc.movie_id == ci.movie_id)) {
              continue;
            }
            for (int tmp18 = 0; tmp18 < company_name.len; tmp18++) {
              Company_nameItem cn = company_name.data[tmp18];
              if (!(cn.id == mc.company_id)) {
                continue;
              }
              for (int tmp19 = 0; tmp19 < role_type.len; tmp19++) {
                Role_typeItem rt = role_type.data[tmp19];
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
                tmp11.data[tmp12] = (EligibleItem){.pseudonym = an1.name,
                                                   .movie_title = t.title};
                tmp12++;
              }
            }
          }
        }
      }
    }
  }
  tmp11.len = tmp12;
  list_EligibleItem eligible = tmp11;
  list_string tmp21 = list_string_create(eligible.len);
  int tmp22 = 0;
  for (int tmp23 = 0; tmp23 < eligible.len; tmp23++) {
    EligibleItem x = eligible.data[tmp23];
    tmp21.data[tmp22] = x.pseudonym;
    tmp22++;
  }
  tmp21.len = tmp22;
  list_string tmp24 = list_string_create(eligible.len);
  int tmp25 = 0;
  for (int tmp26 = 0; tmp26 < eligible.len; tmp26++) {
    EligibleItem x = eligible.data[tmp26];
    tmp24.data[tmp25] = x.movie_title;
    tmp25++;
  }
  tmp24.len = tmp25;
  ResultItem tmp20_data[] = {
      (ResultItem){.actress_pseudonym = _min_string(tmp21),
                   .japanese_movie_dubbed = _min_string(tmp24)}};
  list_ResultItem tmp20 = {1, tmp20_data};
  list_ResultItem result = tmp20;
  printf("[");
  for (int i27 = 0; i27 < result.len; i27++) {
    if (i27 > 0)
      printf(",");
    ResultItem it = result.data[i27];
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
  return 0;
}
