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
static void _print_list_int(list_int v) {
  for (int i = 0; i < v.len; i++) {
    if (i > 0)
      printf(" ");
    printf("%d", v.data[i]);
  }
}
int main() {
  int _t1_data[] = {1, 2, 3};
  list_int _t1 = {3, _t1_data};
  _print_list_int(_t1);
  printf("\n");
  int _t2_data[] = {1, 3};
  list_int _t2 = {2, _t2_data};
  _print_list_int(_t2);
  printf("\n");
  int _t3_data[] = {2};
  list_int _t3 = {1, _t3_data};
  _print_list_int(_t3);
  printf("\n");
  printf("%d\n", 4);
  return 0;
}
