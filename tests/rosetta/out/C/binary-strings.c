// Generated by Mochi compiler v0.10.30 on 2025-07-18T17:09:40Z
// Generated by Mochi compiler v0.10.30 on 2025-07-18T17:09:40Z
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
static list_int concat_list_int(list_int a, list_int b) {
  list_int r = list_int_create(a.len + b.len);
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
static char *slice_string(char *s, int start, int end) {
  int len = strlen(s);
  if (start < 0)
    start += len;
  if (end < 0)
    end += len;
  if (start < 0)
    start = 0;
  if (end > len)
    end = len;
  if (start > end)
    start = end;
  char *buf = (char *)malloc(end - start + 1);
  memcpy(buf, s + start, end - start);
  buf[end - start] = '\0';
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
static list_int b;
static list_int c;
static list_int d;
static int i = 0;
static list_int z;
static list_int sub;
static list_int f;
static list_int rem;

char *_char(int n) {
  __auto_type letters = "abcdefghijklmnopqrstuvwxyz";
  __auto_type idx = n - 97;
  if (idx < 0 || idx >= strlen(letters)) {
    return "?";
  }
  return slice_string(letters, idx, idx + 1);
}

char *fromBytes(list_int bs) {
  __auto_type s = "";
  __auto_type i = 0;
  while (i < bs.len) {
    char *tmp1 = concat_string(s, _char(bs.data[i]));
    s = tmp1;
    i = i + 1;
  }
  return s;
}

int _mochi_main() {
  list_int tmp2 = list_int_create(6);
  tmp2.data[0] = 98;
  tmp2.data[1] = 105;
  tmp2.data[2] = 110;
  tmp2.data[3] = 97;
  tmp2.data[4] = 114;
  tmp2.data[5] = 121;
  b = tmp2;
  c = b;
  list_int tmp3 = list_int_create(0);
  d = tmp3;
  int tmp4_data[1];
  list_int tmp4 = {1, tmp4_data};
  tmp4.data[0] = 122;
  list_int tmp5 = concat_list_int(b, tmp4);
  z = tmp5;
  list_int tmp6 = slice_list_int(b, 1, 3);
  sub = tmp6;
  list_int tmp7 = list_int_create(0);
  f = tmp7;
  list_int tmp8 = list_int_create(0);
  rem = tmp8;
  char *tmp9 = _str(b);
  printf("%s\n", tmp9);
  char *tmp10 = _str(c);
  printf("%s\n", tmp10);
  int tmp11 = 1;
  if (b.len != c.len) {
    tmp11 = 0;
  } else {
    for (int i12 = 0; i12 < b.len; i12++) {
      if (b.data[i12] != c.data[i12]) {
        tmp11 = 0;
        break;
      }
    }
  }
  char *tmp13 = _str(tmp11);
  printf("%s\n", tmp13);
  while (i < b.len) {
    int tmp14_data[1];
    list_int tmp14 = {1, tmp14_data};
    tmp14.data[0] = b.data[i];
    list_int tmp15 = concat_list_int(d, tmp14);
    d = tmp15;
    i = i + 1;
  }
  d.data[1] = 97;
  d.data[4] = 110;
  printf("%s\n", fromBytes(b));
  printf("%s\n", fromBytes(d));
  char *tmp16 = _str(b.len == 0);
  printf("%s\n", tmp16);
  printf("%s\n", fromBytes(z));
  printf("%s\n", fromBytes(sub));
  i = 0;
  while (i < d.len) {
    __auto_type val = d.data[i];
    if (val == 110) {
      int tmp17_data[1];
      list_int tmp17 = {1, tmp17_data};
      tmp17.data[0] = 109;
      list_int tmp18 = concat_list_int(f, tmp17);
      f = tmp18;
    } else {
      int tmp19_data[1];
      list_int tmp19 = {1, tmp19_data};
      tmp19.data[0] = val;
      list_int tmp20 = concat_list_int(f, tmp19);
      f = tmp20;
    }
    i = i + 1;
  }
  char *tmp21 = concat_string(fromBytes(d), " -> ");
  char *tmp22 = concat_string(tmp21, fromBytes(f));
  printf("%s\n", tmp22);
  int tmp23_data[1];
  list_int tmp23 = {1, tmp23_data};
  tmp23.data[0] = b.data[0];
  list_int tmp24 = concat_list_int(rem, tmp23);
  rem = tmp24;
  i = 3;
  while (i < b.len) {
    int tmp25_data[1];
    list_int tmp25 = {1, tmp25_data};
    tmp25.data[0] = b.data[i];
    list_int tmp26 = concat_list_int(rem, tmp25);
    rem = tmp26;
    i = i + 1;
  }
  printf("%s\n", fromBytes(rem));
  return 0;
}
int main() { return _mochi_main(); }
