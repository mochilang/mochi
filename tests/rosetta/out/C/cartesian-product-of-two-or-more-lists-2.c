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

list_list_int cartN(int lists) {
  if ((lists == 0)) {
    list_int tmp7 = list_int_create(0);
    return tmp7;
  }
  list_list_int a = (list_list_int)(lists);
  if (a.len == 0) {
    list_int tmp9 = list_int_create(0);
    list_int tmp8_data[] = {tmp9};
    list_list_int tmp8 = {1, tmp8_data};
    return tmp8;
  }
  int c = 1;
  for (int tmp10 = 0; tmp10 < a.len; tmp10++) {
    list_int xs = a.data[tmp10];
    c = c * xs.len;
  }
  if (c == 0) {
    list_int tmp11 = list_int_create(0);
    return tmp11;
  }
  list_list_int res = {0, NULL};
  list_int idx = list_int_create(0);
  for (int tmp12 = 0; tmp12 < a.len; tmp12++) {
    int tmp13_data[1];
    list_int tmp13 = {1, tmp13_data};
    tmp13.data[0] = 0;
    list_int tmp14 = concat_list_int(idx, tmp13);
    idx = tmp14;
  }
  int n = a.len;
  int count = 0;
  while ((count < c)) {
    list_int row = list_int_create(0);
    int j = 0;
    while (j < n) {
      int tmp15_data[1];
      list_int tmp15 = {1, tmp15_data};
      tmp15.data[0] = a.data[j].data[idx.data[j]];
      list_int tmp16 = concat_list_int(row, tmp15);
      row = tmp16;
      j = j + 1;
    }
    list_list_int tmp17 = list_list_int_create(1);
    tmp17.data[0] = row;
    list_list_int tmp18 = concat_list_list_int(res, tmp17);
    res = tmp18;
    int k = n - 1;
    while (k >= 0) {
      idx.data[k] = idx.data[k] + 1;
      if ((idx.data[k] < a.data[k].len)) {
        break;
      }
      idx.data[k] = 0;
      k = k - 1;
    }
    count = count + 1;
  }
  return res;
}

int mochi_main() {
  list_int tmp20 = list_int_create(2);
  tmp20.data[0] = 1;
  tmp20.data[1] = 2;
  list_int tmp21 = list_int_create(2);
  tmp21.data[0] = 3;
  tmp21.data[1] = 4;
  list_int tmp19_data[] = {tmp20, tmp21};
  list_list_int tmp19 = {2, tmp19_data};
  printf("%s\n", llStr(cartN(tmp19)));
  list_int tmp23 = list_int_create(2);
  tmp23.data[0] = 3;
  tmp23.data[1] = 4;
  list_int tmp24 = list_int_create(2);
  tmp24.data[0] = 1;
  tmp24.data[1] = 2;
  list_int tmp22_data[] = {tmp23, tmp24};
  list_list_int tmp22 = {2, tmp22_data};
  printf("%s\n", llStr(cartN(tmp22)));
  list_int tmp26 = list_int_create(2);
  tmp26.data[0] = 1;
  tmp26.data[1] = 2;
  list_int tmp27 = list_int_create(0);
  list_int tmp25_data[] = {tmp26, tmp27};
  list_list_int tmp25 = {2, tmp25_data};
  printf("%s\n", llStr(cartN(tmp25)));
  list_int tmp29 = list_int_create(0);
  list_int tmp30 = list_int_create(2);
  tmp30.data[0] = 1;
  tmp30.data[1] = 2;
  list_int tmp28_data[] = {tmp29, tmp30};
  list_list_int tmp28 = {2, tmp28_data};
  printf("%s\n", llStr(cartN(tmp28)));
  printf("\n");
  printf("[\n");
  list_int tmp32 = list_int_create(2);
  tmp32.data[0] = 1776;
  tmp32.data[1] = 1789;
  list_int tmp33 = list_int_create(2);
  tmp33.data[0] = 7;
  tmp33.data[1] = 12;
  list_int tmp34 = list_int_create(3);
  tmp34.data[0] = 4;
  tmp34.data[1] = 14;
  tmp34.data[2] = 23;
  list_int tmp35 = list_int_create(2);
  tmp35.data[0] = 0;
  tmp35.data[1] = 1;
  list_int tmp31_data[] = {tmp32, tmp33, tmp34, tmp35};
  list_list_int tmp31 = {4, tmp31_data};
  for (int tmp36 = 0; tmp36 < cartN(tmp31).len; tmp36++) {
    list_int p = cartN(tmp31).data[tmp36];
    printf("%s\n", " " + listStr(p));
  }
  printf("]\n");
  list_int tmp38 = list_int_create(3);
  tmp38.data[0] = 1;
  tmp38.data[1] = 2;
  tmp38.data[2] = 3;
  list_int tmp39 = list_int_create(1);
  tmp39.data[0] = 30;
  list_int tmp40 = list_int_create(2);
  tmp40.data[0] = 500;
  tmp40.data[1] = 100;
  list_int tmp37_data[] = {tmp38, tmp39, tmp40};
  list_list_int tmp37 = {3, tmp37_data};
  printf("%s\n", llStr(cartN(tmp37)));
  list_int tmp42 = list_int_create(3);
  tmp42.data[0] = 1;
  tmp42.data[1] = 2;
  tmp42.data[2] = 3;
  list_int tmp43 = list_int_create(0);
  list_int tmp44 = list_int_create(2);
  tmp44.data[0] = 500;
  tmp44.data[1] = 100;
  list_int tmp41_data[] = {tmp42, tmp43, tmp44};
  list_list_int tmp41 = {3, tmp41_data};
  printf("%s\n", llStr(cartN(tmp41)));
  printf("\n");
  printf("%s\n", llStr(cartN(0)));
  list_int tmp45 = list_int_create(0);
  printf("%s\n", llStr(cartN(tmp45)));
}

int _mochi_main() {
  mochi_main();
  return 0;
}
int main() { return _mochi_main(); }
