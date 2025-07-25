// Generated by Mochi compiler v0.10.28 on 2025-07-18T10:18:06Z
// Generated by Mochi compiler v0.10.28 on 2025-07-18T10:18:06Z
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
static list_int concat_list_int(list_int a, list_int b) {
  list_int r = list_int_create(a.len + b.len);
  for (int i = 0; i < a.len; i++)
    r.data[i] = a.data[i];
  for (int i = 0; i < b.len; i++)
    r.data[a.len + i] = b.data[i];
  return r;
}
static list_list_int concat_list_list_int(list_list_int a, list_list_int b) {
  list_list_int r = list_list_int_create(a.len + b.len);
  for (int i = 0; i < a.len; i++)
    r.data[i] = a.data[i];
  for (int i = 0; i < b.len; i++)
    r.data[a.len + i] = b.data[i];
  return r;
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
static list_int slice_list_int(list_int v, int start, int end) {
  if (start < 0)
    start += v.len;
  if (end < 0)
    end += v.len;
  if (start < 0)
    start = 0;
  if (end > v.len)
    end = v.len;
  if (start > end)
    start = end;
  list_int r = list_int_create(end - start);
  for (int i = 0; i < r.len; i++)
    r.data[i] = v.data[start + i];
  return r;
}
char *listStr(list_int xs) {
  char *s = "[";
  int i = 0;
  while (i < xs.len) {
    char *tmp1 = _str(xs.data[i]);
    char *tmp2 = concat_string(s, tmp1);
    s = tmp2;
    if (i < xs.len - 1) {
      char *tmp3 = concat_string(s, " ");
      s = tmp3;
    }
    i = i + 1;
  }
  char *tmp4 = concat_string(s, "]");
  s = tmp4;
  return s;
}

char *llStr(list_list_int lst) {
  char *s = "[";
  int i = 0;
  while (i < lst.len) {
    s = s + listStr(lst.data[i]);
    if (i < lst.len - 1) {
      char *tmp5 = concat_string(s, " ");
      s = tmp5;
    }
    i = i + 1;
  }
  char *tmp6 = concat_string(s, "]");
  s = tmp6;
  return s;
}

list_int concat(list_int a, list_int b) {
  list_int out = list_int_create(0);
  for (int tmp7 = 0; tmp7 < a.len; tmp7++) {
    int v = a.data[tmp7];
    int tmp8_data[1];
    list_int tmp8 = {1, tmp8_data};
    tmp8.data[0] = v;
    list_int tmp9 = concat_list_int(out, tmp8);
    out = tmp9;
  }
  for (int tmp10 = 0; tmp10 < b.len; tmp10++) {
    int v = b.data[tmp10];
    int tmp11_data[1];
    list_int tmp11 = {1, tmp11_data};
    tmp11.data[0] = v;
    list_int tmp12 = concat_list_int(out, tmp11);
    out = tmp12;
  }
  return out;
}

list_list_int cartN(int lists) {
  if ((lists == 0)) {
    list_int tmp13 = list_int_create(0);
    return tmp13;
  }
  list_list_int a = (list_list_int)(lists);
  if (a.len == 0) {
    list_int tmp15 = list_int_create(0);
    list_int tmp14_data[] = {tmp15};
    list_list_int tmp14 = {1, tmp14_data};
    return tmp14;
  }
  list_list_int out = {0, NULL};
  list_int tmp16 = slice_list_int(a, 1, a.len);
  list_list_int rest = cartN(tmp16);
  for (int tmp17 = 0; tmp17 < a.data[0].len; tmp17++) {
    int x = a.data[0].data[tmp17];
    for (int tmp18 = 0; tmp18 < rest.len; tmp18++) {
      list_int p = rest.data[tmp18];
      list_int tmp19 = list_int_create(1);
      tmp19.data[0] = x;
      list_list_int tmp20 = list_list_int_create(1);
      tmp20.data[0] = concat(tmp19, p);
      list_list_int tmp21 = concat_list_list_int(out, tmp20);
      out = tmp21;
    }
  }
  return out;
}

int mochi_main() {
  list_int tmp23 = list_int_create(2);
  tmp23.data[0] = 1;
  tmp23.data[1] = 2;
  list_int tmp24 = list_int_create(2);
  tmp24.data[0] = 3;
  tmp24.data[1] = 4;
  list_int tmp22_data[] = {tmp23, tmp24};
  list_list_int tmp22 = {2, tmp22_data};
  printf("%s\n", llStr(cartN(tmp22)));
  list_int tmp26 = list_int_create(2);
  tmp26.data[0] = 3;
  tmp26.data[1] = 4;
  list_int tmp27 = list_int_create(2);
  tmp27.data[0] = 1;
  tmp27.data[1] = 2;
  list_int tmp25_data[] = {tmp26, tmp27};
  list_list_int tmp25 = {2, tmp25_data};
  printf("%s\n", llStr(cartN(tmp25)));
  list_int tmp29 = list_int_create(2);
  tmp29.data[0] = 1;
  tmp29.data[1] = 2;
  list_int tmp30 = list_int_create(0);
  list_int tmp28_data[] = {tmp29, tmp30};
  list_list_int tmp28 = {2, tmp28_data};
  printf("%s\n", llStr(cartN(tmp28)));
  list_int tmp32 = list_int_create(0);
  list_int tmp33 = list_int_create(2);
  tmp33.data[0] = 1;
  tmp33.data[1] = 2;
  list_int tmp31_data[] = {tmp32, tmp33};
  list_list_int tmp31 = {2, tmp31_data};
  printf("%s\n", llStr(cartN(tmp31)));
  printf("\n");
  printf("[\n");
  list_int tmp35 = list_int_create(2);
  tmp35.data[0] = 1776;
  tmp35.data[1] = 1789;
  list_int tmp36 = list_int_create(2);
  tmp36.data[0] = 7;
  tmp36.data[1] = 12;
  list_int tmp37 = list_int_create(3);
  tmp37.data[0] = 4;
  tmp37.data[1] = 14;
  tmp37.data[2] = 23;
  list_int tmp38 = list_int_create(2);
  tmp38.data[0] = 0;
  tmp38.data[1] = 1;
  list_int tmp34_data[] = {tmp35, tmp36, tmp37, tmp38};
  list_list_int tmp34 = {4, tmp34_data};
  for (int tmp39 = 0; tmp39 < cartN(tmp34).len; tmp39++) {
    list_int p = cartN(tmp34).data[tmp39];
    printf("%s\n", " " + listStr(p));
  }
  printf("]\n");
  list_int tmp41 = list_int_create(3);
  tmp41.data[0] = 1;
  tmp41.data[1] = 2;
  tmp41.data[2] = 3;
  list_int tmp42 = list_int_create(1);
  tmp42.data[0] = 30;
  list_int tmp43 = list_int_create(2);
  tmp43.data[0] = 500;
  tmp43.data[1] = 100;
  list_int tmp40_data[] = {tmp41, tmp42, tmp43};
  list_list_int tmp40 = {3, tmp40_data};
  printf("%s\n", llStr(cartN(tmp40)));
  list_int tmp45 = list_int_create(3);
  tmp45.data[0] = 1;
  tmp45.data[1] = 2;
  tmp45.data[2] = 3;
  list_int tmp46 = list_int_create(0);
  list_int tmp47 = list_int_create(2);
  tmp47.data[0] = 500;
  tmp47.data[1] = 100;
  list_int tmp44_data[] = {tmp45, tmp46, tmp47};
  list_list_int tmp44 = {3, tmp44_data};
  printf("%s\n", llStr(cartN(tmp44)));
  printf("\n");
  printf("%s\n", llStr(cartN(0)));
  list_int tmp48 = list_int_create(0);
  printf("%s\n", llStr(cartN(tmp48)));
}

int _mochi_main() {
  mochi_main();
  return 0;
}
int main() { return _mochi_main(); }
