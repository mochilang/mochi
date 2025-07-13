
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
  char *of_person;
  char *biography_movie;
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
  char *info;
} Info_typeItem;
typedef struct {
  int len;
  Info_typeItem *data;
} list_Info_typeItem;
static list_Info_typeItem list_Info_typeItem_create(int len) {
  list_Info_typeItem l;
  l.len = len;
  l.data = calloc(len, sizeof(Info_typeItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  char *link;
} Link_typeItem;
typedef struct {
  int len;
  Link_typeItem *data;
} list_Link_typeItem;
static list_Link_typeItem list_Link_typeItem_create(int len) {
  list_Link_typeItem l;
  l.len = len;
  l.data = calloc(len, sizeof(Link_typeItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int linked_movie_id;
  int link_type_id;
} Movie_linkItem;
typedef struct {
  int len;
  Movie_linkItem *data;
} list_Movie_linkItem;
static list_Movie_linkItem list_Movie_linkItem_create(int len) {
  list_Movie_linkItem l;
  l.len = len;
  l.data = calloc(len, sizeof(Movie_linkItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  int id;
  char *name;
  char *name_pcode_cf;
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
  int person_id;
  int info_type_id;
  char *note;
} Person_infoItem;
typedef struct {
  int len;
  Person_infoItem *data;
} list_Person_infoItem;
static list_Person_infoItem list_Person_infoItem_create(int len) {
  list_Person_infoItem l;
  l.len = len;
  l.data = calloc(len, sizeof(Person_infoItem));
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
  char *person_name;
  char *movie_title;
} RowsItem;
typedef struct {
  int len;
  RowsItem *data;
} list_RowsItem;
static list_RowsItem list_RowsItem_create(int len) {
  list_RowsItem l;
  l.len = len;
  l.data = calloc(len, sizeof(RowsItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  char *of_person;
  char *biography_movie;
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

static list_int test_Q7_finds_movie_features_biography_for_person_result;
static void test_Q7_finds_movie_features_biography_for_person() {
  list_int tmp1 = list_int_create(1);
  tmp1.data[0] =
      (TmpItem){.of_person = "Alan Brown", .biography_movie = "Feature Film"};
  int tmp2 = 1;
  if (test_Q7_finds_movie_features_biography_for_person_result.len !=
      tmp1.len) {
    tmp2 = 0;
  } else {
    for (int i3 = 0;
         i3 < test_Q7_finds_movie_features_biography_for_person_result.len;
         i3++) {
      if (test_Q7_finds_movie_features_biography_for_person_result.data[i3] !=
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
  Aka_nameItem tmp4_data[] = {
      (Aka_nameItem){.person_id = 1, .name = "Anna Mae"},
      (Aka_nameItem){.person_id = 2, .name = "Chris"}};
  list_Aka_nameItem tmp4 = {2, tmp4_data};
  list_Aka_nameItem aka_name = tmp4;
  Cast_infoItem tmp5_data[] = {(Cast_infoItem){.person_id = 1, .movie_id = 10},
                               (Cast_infoItem){.person_id = 2, .movie_id = 20}};
  list_Cast_infoItem tmp5 = {2, tmp5_data};
  list_Cast_infoItem cast_info = tmp5;
  Info_typeItem tmp6_data[] = {
      (Info_typeItem){.id = 1, .info = "mini biography"},
      (Info_typeItem){.id = 2, .info = "trivia"}};
  list_Info_typeItem tmp6 = {2, tmp6_data};
  list_Info_typeItem info_type = tmp6;
  Link_typeItem tmp7_data[] = {(Link_typeItem){.id = 1, .link = "features"},
                               (Link_typeItem){.id = 2, .link = "references"}};
  list_Link_typeItem tmp7 = {2, tmp7_data};
  list_Link_typeItem link_type = tmp7;
  Movie_linkItem tmp8_data[] = {
      (Movie_linkItem){.linked_movie_id = 10, .link_type_id = 1},
      (Movie_linkItem){.linked_movie_id = 20, .link_type_id = 2}};
  list_Movie_linkItem tmp8 = {2, tmp8_data};
  list_Movie_linkItem movie_link = tmp8;
  NameItem tmp9_data[] = {
      (NameItem){
          .id = 1, .name = "Alan Brown", .name_pcode_cf = "B", .gender = "m"},
      (NameItem){.id = 2, .name = "Zoe", .name_pcode_cf = "Z", .gender = "f"}};
  list_NameItem tmp9 = {2, tmp9_data};
  list_NameItem name = tmp9;
  Person_infoItem tmp10_data[] = {
      (Person_infoItem){
          .person_id = 1, .info_type_id = 1, .note = "Volker Boehm"},
      (Person_infoItem){.person_id = 2, .info_type_id = 1, .note = "Other"}};
  list_Person_infoItem tmp10 = {2, tmp10_data};
  list_Person_infoItem person_info = tmp10;
  TitleItem tmp11_data[] = {
      (TitleItem){.id = 10, .title = "Feature Film", .production_year = 1990},
      (TitleItem){.id = 20, .title = "Late Film", .production_year = 2000}};
  list_TitleItem tmp11 = {2, tmp11_data};
  list_TitleItem title = tmp11;
  list_RowsItem tmp12 = list_RowsItem_create(
      aka_name.len * name.len * person_info.len * info_type.len *
      cast_info.len * title.len * movie_link.len * link_type.len);
  int tmp13 = 0;
  for (int tmp14 = 0; tmp14 < aka_name.len; tmp14++) {
    Aka_nameItem an = aka_name.data[tmp14];
    for (int tmp15 = 0; tmp15 < name.len; tmp15++) {
      NameItem n = name.data[tmp15];
      if (!(n.id == an.person_id)) {
        continue;
      }
      for (int tmp16 = 0; tmp16 < person_info.len; tmp16++) {
        Person_infoItem pi = person_info.data[tmp16];
        if (!(pi.person_id == an.person_id)) {
          continue;
        }
        for (int tmp17 = 0; tmp17 < info_type.len; tmp17++) {
          Info_typeItem it = info_type.data[tmp17];
          if (!(it.id == pi.info_type_id)) {
            continue;
          }
          for (int tmp18 = 0; tmp18 < cast_info.len; tmp18++) {
            Cast_infoItem ci = cast_info.data[tmp18];
            if (!(ci.person_id == n.id)) {
              continue;
            }
            for (int tmp19 = 0; tmp19 < title.len; tmp19++) {
              TitleItem t = title.data[tmp19];
              if (!(t.id == ci.movie_id)) {
                continue;
              }
              for (int tmp20 = 0; tmp20 < movie_link.len; tmp20++) {
                Movie_linkItem ml = movie_link.data[tmp20];
                if (!(ml.linked_movie_id == t.id)) {
                  continue;
                }
                for (int tmp21 = 0; tmp21 < link_type.len; tmp21++) {
                  Link_typeItem lt = link_type.data[tmp21];
                  if (!(lt.id == ml.link_type_id)) {
                    continue;
                  }
                  if (!((contains_string(an.name, "a") &&
                         it.info == "mini biography" && lt.link == "features" &&
                         n.name_pcode_cf >= "A" && n.name_pcode_cf <= "F" &&
                         ((strcmp(n.gender, "m") == 0) ||
                          ((strcmp(n.gender, "f") == 0) &&
                           n.name.starts_with("B"))) &&
                         pi.note == "Volker Boehm" &&
                         t.production_year >= 1980 &&
                         t.production_year <= 1995 &&
                         pi.person_id == an.person_id &&
                         pi.person_id == ci.person_id &&
                         an.person_id == ci.person_id &&
                         ci.movie_id == ml.linked_movie_id))) {
                    continue;
                  }
                  tmp12.data[tmp13] =
                      (RowsItem){.person_name = n.name, .movie_title = t.title};
                  tmp13++;
                }
              }
            }
          }
        }
      }
    }
  }
  tmp12.len = tmp13;
  list_RowsItem rows = tmp12;
  list_string tmp23 = list_string_create(rows.len);
  int tmp24 = 0;
  for (int tmp25 = 0; tmp25 < rows.len; tmp25++) {
    RowsItem r = rows.data[tmp25];
    tmp23.data[tmp24] = r.person_name;
    tmp24++;
  }
  tmp23.len = tmp24;
  list_string tmp26 = list_string_create(rows.len);
  int tmp27 = 0;
  for (int tmp28 = 0; tmp28 < rows.len; tmp28++) {
    RowsItem r = rows.data[tmp28];
    tmp26.data[tmp27] = r.movie_title;
    tmp27++;
  }
  tmp26.len = tmp27;
  ResultItem tmp22_data[] = {(ResultItem){
      .of_person = _min_string(tmp23), .biography_movie = _min_string(tmp26)}};
  list_ResultItem tmp22 = {1, tmp22_data};
  list_ResultItem result = tmp22;
  printf("[");
  for (int i29 = 0; i29 < result.len; i29++) {
    if (i29 > 0)
      printf(",");
    ResultItem it = result.data[i29];
    printf("{");
    _json_string("of_person");
    printf(":");
    _json_string(it.of_person);
    printf(",");
    _json_string("biography_movie");
    printf(":");
    _json_string(it.biography_movie);
    printf("}");
  }
  printf("]");
  test_Q7_finds_movie_features_biography_for_person_result = result;
  test_Q7_finds_movie_features_biography_for_person();
  return 0;
}
