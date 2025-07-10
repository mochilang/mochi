#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int len;
  int *data;
} list_int;
static list_int list_int_create(int len) {
  list_int l;
  l.len = len;
  l.data = (int *)malloc(sizeof(int) * len);
  return l;
}
typedef struct {
  int len;
  list_int *data;
} list_list_int;
static list_list_int list_list_int_create(int len) {
  list_list_int l;
  l.len = len;
  l.data = (list_int *)malloc(sizeof(list_int) * len);
  return l;
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
static void _print_list_int(list_int v) {
  for (int i = 0; i < v.len; i++) {
    if (i > 0)
      printf(" ");
    printf("%d", v.data[i]);
  }
}
static char *prefix = "fore";
static char *s1 = "forest";
static char *s2 = "desert";

int main() {
  list_int _t1 = slice_list_int(s1, 0, prefix.len);
  _print_list_int(_t1 == prefix);
  printf("\n");
  list_int _t2 = slice_list_int(s2, 0, prefix.len);
  _print_list_int(_t2 == prefix);
  printf("\n");
  return 0;
}
