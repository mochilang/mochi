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
static void _print_list_int(list_int v) {
  for (int i = 0; i < v.len; i++) {
    if (i > 0)
      printf(" ");
    printf("%d", v.data[i]);
  }
}
int main() {
  int tmp1_data[] = {1, 2, 3};
  list_int tmp1 = {3, tmp1_data};
  _print_list_int(tmp1);
  printf("\n");
  int tmp2_data[] = {1, 3};
  list_int tmp2 = {2, tmp2_data};
  _print_list_int(tmp2);
  printf("\n");
  int tmp3_data[] = {2};
  list_int tmp3 = {1, tmp3_data};
  _print_list_int(tmp3);
  printf("\n");
  printf("%d\n", 4);
  return 0;
}
