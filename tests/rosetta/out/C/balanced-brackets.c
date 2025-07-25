// Generated by Mochi compiler v0.10.30 on 2025-07-18T17:09:37Z
// Generated by Mochi compiler v0.10.30 on 2025-07-18T17:09:37Z
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
static list_string concat_list_string(list_string a, list_string b) {
  list_string r = list_string_create(a.len + b.len);
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
static int seed = 1;

int prng(int max) {
  seed = (seed * 1103515245 + 12345) % 2147483648;
  return seed % max;
}

char *gen(int n) {
  list_string arr = list_string_create(0);
  __auto_type i = 0;
  while (i < n) {
    list_string tmp1 = list_string_create(1);
    tmp1.data[0] = "[";
    list_string tmp2 = concat_list_string(arr, tmp1);
    arr = tmp2;
    list_string tmp3 = list_string_create(1);
    tmp3.data[0] = "]";
    list_string tmp4 = concat_list_string(arr, tmp3);
    arr = tmp4;
    i = i + 1;
  }
  __auto_type j = arr.len - 1;
  while (j > 0) {
    __auto_type k = prng(j + 1);
    __auto_type tmp = arr.data[j];
    arr.data[j] = arr.data[k];
    arr.data[k] = tmp;
    j = j - 1;
  }
  __auto_type out = "";
  for (int tmp5 = 0; tmp5 < arr.len; tmp5++) {
    char *ch = arr.data[tmp5];
    char *tmp6 = concat_string(out, ch);
    out = tmp6;
  }
  return out;
}

int testBalanced(char *s) {
  __auto_type open = 0;
  __auto_type i = 0;
  while (i < strlen(s)) {
    char *tmp7 = slice_string(s, i, i + 1);
    __auto_type c = tmp7;
    if ((strcmp(c, "[") == 0)) {
      open = open + 1;
    } else if ((strcmp(c, "]") == 0)) {
      if (open == 0) {
        char *tmp8 = concat_string(s, ": not ok");
        printf("%s\n", tmp8);
        return;
      }
      open = open - 1;
    } else {
      char *tmp9 = concat_string(s, ": not ok");
      printf("%s\n", tmp9);
      return;
    }
    i = i + 1;
  }
  if (open == 0) {
    char *tmp10 = concat_string(s, ": ok");
    printf("%s\n", tmp10);
  } else {
    char *tmp11 = concat_string(s, ": not ok");
    printf("%s\n", tmp11);
  }
}

int mochi_main() {
  __auto_type i = 0;
  while (i < 10) {
    testBalanced(gen(i));
    i = i + 1;
  }
  testBalanced("()");
}

int _mochi_main() {
  mochi_main();
  return 0;
}
int main() { return _mochi_main(); }
