// Generated by Mochi compiler v0.10.28 on 2025-07-18T10:18:09Z
// Generated by Mochi compiler v0.10.28 on 2025-07-18T10:18:09Z
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
static char *concat_string(char *a, char *b) {
  size_t len1 = strlen(a);
  size_t len2 = strlen(b);
  char *buf = (char *)malloc(len1 + len2 + 1);
  memcpy(buf, a, len1);
  memcpy(buf + len1, b, len2);
  buf[len1 + len2] = '\0';
  return buf;
}
static char *_str(int v) {
  char *buf = (char *)malloc(32);
  sprintf(buf, "%d", v);
  return buf;
}
typedef struct info_t info_t;

typedef struct info_t {
  char *animal;
  char *yinYang;
  char *element;
  char *stemBranch;
  int cycle;
} info_t;
typedef struct {
  int len;
  info_t *data;
} info_list_t;
info_list_t create_info_list(int len) {
  info_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(info_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

info_t cz(int yr, list_string animal, list_string yinYang, list_string element,
          list_string sc, list_string bc) {
  int y = yr - 4;
  int stem = y % 10;
  int branch = y % 12;
  char *tmp1 = concat_string(sc.data[stem], bc.data[branch]);
  char *sb = tmp1;
  return (info_t){.animal = (char *)(animal.data[branch]),
                  .yin_yang = (char *)(yinYang.data[stem % 2]),
                  .element = (char *)(element.data[(int)((stem / 2))]),
                  .stem_branch = sb,
                  .cycle = y % 60 + 1};
}

int _mochi_main() {
  list_string animal = list_string_create(12);
  animal.data[0] = "Rat";
  animal.data[1] = "Ox";
  animal.data[2] = "Tiger";
  animal.data[3] = "Rabbit";
  animal.data[4] = "Dragon";
  animal.data[5] = "Snake";
  animal.data[6] = "Horse";
  animal.data[7] = "Goat";
  animal.data[8] = "Monkey";
  animal.data[9] = "Rooster";
  animal.data[10] = "Dog";
  animal.data[11] = "Pig";
  list_string yinYang = list_string_create(2);
  yinYang.data[0] = "Yang";
  yinYang.data[1] = "Yin";
  list_string element = list_string_create(5);
  element.data[0] = "Wood";
  element.data[1] = "Fire";
  element.data[2] = "Earth";
  element.data[3] = "Metal";
  element.data[4] = "Water";
  list_string stemChArr = list_string_create(10);
  stemChArr.data[0] = "甲";
  stemChArr.data[1] = "乙";
  stemChArr.data[2] = "丙";
  stemChArr.data[3] = "丁";
  stemChArr.data[4] = "戊";
  stemChArr.data[5] = "己";
  stemChArr.data[6] = "庚";
  stemChArr.data[7] = "辛";
  stemChArr.data[8] = "壬";
  stemChArr.data[9] = "癸";
  list_string branchChArr = list_string_create(12);
  branchChArr.data[0] = "子";
  branchChArr.data[1] = "丑";
  branchChArr.data[2] = "寅";
  branchChArr.data[3] = "卯";
  branchChArr.data[4] = "辰";
  branchChArr.data[5] = "巳";
  branchChArr.data[6] = "午";
  branchChArr.data[7] = "未";
  branchChArr.data[8] = "申";
  branchChArr.data[9] = "酉";
  branchChArr.data[10] = "戌";
  branchChArr.data[11] = "亥";
  list_int tmp2 = list_int_create(5);
  tmp2.data[0] = 1935;
  tmp2.data[1] = 1938;
  tmp2.data[2] = 1968;
  tmp2.data[3] = 1972;
  tmp2.data[4] = 1976;
  for (int tmp3 = 0; tmp3 < 5; tmp3++) {
    int yr = tmp2.data[tmp3];
    info_t r = cz(yr, animal, yinYang, element, stemChArr, branchChArr);
    char *tmp4 = _str(yr);
    char *tmp5 = concat_string(tmp4, ": ");
    char *tmp6 = concat_string(tmp5, r.element);
    char *tmp7 = concat_string(tmp6, " ");
    char *tmp8 = concat_string(tmp7, r.animal);
    char *tmp9 = concat_string(tmp8, ", ");
    char *tmp10 = concat_string(tmp9, r.yin_yang);
    char *tmp11 = concat_string(tmp10, ", Cycle year ");
    char *tmp12 = _str(r.cycle);
    char *tmp13 = concat_string(tmp11, tmp12);
    char *tmp14 = concat_string(tmp13, " ");
    char *tmp15 = concat_string(tmp14, r.stem_branch);
    printf("%s\n", tmp15);
  }
  return 0;
}
int main() { return _mochi_main(); }
