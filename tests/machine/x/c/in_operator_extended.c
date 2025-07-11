#include <stdio.h>
#include <stdlib.h>

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
static int contains_list_int(list_int v, int item) {
  for (int i = 0; i < v.len; i++)
    if (v.data[i] == item)
      return 1;
  return 0;
}
static char *s = "hello";

typedef struct {
  int a;
} MItem;
typedef struct {
  int len;
  MItem *data;
} list_MItem;
static list_MItem list_MItem_create(int len) {
  list_MItem l;
  l.len = len;
  l.data = calloc(len, sizeof(MItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  int tmp1_data[] = {1, 2, 3};
  list_int tmp1 = {3, tmp1_data};
  list_int xs = tmp1;
  list_int tmp2 = list_int_create(xs.len);
  int tmp3 = 0;
  for (int tmp4 = 0; tmp4 < xs.len; tmp4++) {
    int x = xs.data[tmp4];
    if (!(x % 2 == 1)) {
      continue;
    }
    tmp2.data[tmp3] = x;
    tmp3++;
  }
  tmp2.len = tmp3;
  list_int ys = tmp2;
  printf("%s\n", (contains_list_int(ys, 1)) ? "true" : "false");
  printf("%s\n", (contains_list_int(ys, 2)) ? "true" : "false");
  mItem m = (MItem){.a = 1};
  printf("%d\n", "a" in m);
  printf("%d\n", "b" in m);
  printf("%d\n", "ell" in s);
  printf("%d\n", "foo" in s);
  return 0;
}
