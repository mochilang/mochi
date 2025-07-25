// Generated by Mochi compiler v0.10.28 on 2025-07-18T10:18:07Z
// Generated by Mochi compiler v0.10.28 on 2025-07-18T10:18:06Z
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
static int idx = 0;

typedef struct {
  int base;
  char *begin;
  char *end;
  list_string kaprekar;
} test_case_t;
typedef struct {
  int len;
  test_case_t *data;
} test_case_list_t;
test_case_list_t create_test_case_list(int len) {
  test_case_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(test_case_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int parseIntBase(char *s, int base) {
  char *digits = "0123456789abcdefghijklmnopqrstuvwxyz";
  int n = 0;
  int i = 0;
  while (i < strlen(s)) {
    int j = 0;
    int v = 0;
    while (j < strlen(digits)) {
      char *tmp1 = slice_string(s, i, i + 1);
      if (slice_string(digits, j, j + 1) == tmp1) {
        v = j;
        break;
      }
      j = j + 1;
    }
    n = n * base + v;
    i = i + 1;
  }
  return n;
}

char *intToBase(int n, int base) {
  char *digits = "0123456789abcdefghijklmnopqrstuvwxyz";
  if (n == 0) {
    return "0";
  }
  char *out = "";
  int v = n;
  while (v > 0) {
    int d = v % base;
    char *tmp2 = slice_string(digits, d, d + 1);
    char *tmp3 = concat_string(tmp2, out);
    out = tmp3;
    v = ((double)v) / ((double)base);
  }
  return out;
}

list_string subset(int base, char *begin, char *end) {
  int b = parseIntBase(begin, base);
  int e = parseIntBase(end, base);
  list_string out = list_string_create(0);
  int k = b;
  while (k <= e) {
    char *ks = intToBase(k, base);
    int mod = base - 1;
    int r1 = parseIntBase(ks, base) % mod;
    int r2 = (parseIntBase(ks, base) * parseIntBase(ks, base)) % mod;
    if (r1 == r2) {
      list_string tmp4 = list_string_create(1);
      tmp4.data[0] = ks;
      list_string tmp5 = concat_list_string(out, tmp4);
      out = tmp5;
    }
    k = k + 1;
  }
  return out;
}

int _mochi_main() {
  list_string tmp6 = list_string_create(5);
  tmp6.data[0] = "1";
  tmp6.data[1] = "9";
  tmp6.data[2] = "45";
  tmp6.data[3] = "55";
  tmp6.data[4] = "99";
  list_string tmp7 = list_string_create(3);
  tmp7.data[0] = "3d";
  tmp7.data[1] = "d4";
  tmp7.data[2] = "gg";
  test_case_t testCases[] = {
      (test_case_t){.base = 10, .begin = "1", .end = "100", .kaprekar = tmp6},
      (test_case_t){.base = 17, .begin = "10", .end = "gg", .kaprekar = tmp7}};
  int testCases_len = sizeof(testCases) / sizeof(testCases[0]);
  while (idx < testCases_len) {
    test_case_t tc = testCases[idx];
    char *tmp8 = _str(tc.base);
    char *tmp9 = concat_string("\nTest case base = ", tmp8);
    char *tmp10 = concat_string(tmp9, ", begin = ");
    char *tmp11 = _str(tc.base);
    char *tmp12 = concat_string("\nTest case base = ", tmp11);
    char *tmp13 = concat_string(tmp12, ", begin = ");
    printf("%d\n", tmp13 + tc.begin + ", end = " + tc.end + ":");
    int s = subset(tc.base, tc.begin, tc.end);
    char *tmp14 = _str(s);
    char *tmp15 = concat_string("Subset:  ", tmp14);
    printf("%s\n", tmp15);
    char *tmp16 = _str(tc.kaprekar);
    char *tmp17 = concat_string("Kaprekar:", tmp16);
    char *tmp18 = _str(tc.kaprekar);
    char *tmp19 = concat_string("Kaprekar:", tmp18);
    printf("%d\n", tmp19);
    int sx = 0;
    int valid = 1;
    int i = 0;
    while ((i < tc.kaprekar.len)) {
      int k = tc.kaprekar.data[i];
      int found = 0;
      while (sx < s.len) {
        if ((s.data[sx] == k)) {
          found = 1;
          sx = sx + 1;
          break;
        }
        sx = sx + 1;
      }
      if ((!found)) {
        printf("%d\n", "Fail:" + k + " not in subset");
        valid = 0;
        break;
      }
      i = i + 1;
    }
    if (valid) {
      printf("Valid subset.\n");
    }
    idx = idx + 1;
  }
  return 0;
}
int main() { return _mochi_main(); }
