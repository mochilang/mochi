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
int main() {
  int tmp2_data[] = {1, 2};
  list_int tmp2 = {2, tmp2_data};
  int tmp3_data[] = {3, 4};
  list_int tmp3 = {2, tmp3_data};
  list_int tmp1_data[] = {tmp2, tmp3};
  list_list_int tmp1 = {2, tmp1_data};
  list_list_int matrix = tmp1;
  matrix.data[1].data[0] = 5;
  printf("%d\n", matrix.data[1].data[0]);
  return 0;
}
