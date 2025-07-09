#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int len;
  int cap;
  int *data;
} list_int;
static list_int list_int_create(int len) {
  list_int l;
  l.len = len;
  l.cap = len;
  l.data = len ? (int *)malloc(sizeof(int) * len) : NULL;
  return l;
}
static void list_int_free(list_int *l) {
  free(l->data);
  l->data = NULL;
  l->len = 0;
  l->cap = 0;
}
int main() {
  list_int _t1 = list_int_create(3);
  _t1.data[0] = 3;
  _t1.data[1] = 1;
  _t1.data[2] = 4;
  list_int nums = _t1;
  printf("%d\n", ({
           int m = nums.len ? nums.data[0] : 0;
           for (int i = 1; i < nums.len; i++)
             if (nums.data[i] < m)
               m = nums.data[i];
           m;
         }));
  printf("%d\n", ({
           int m = nums.len ? nums.data[0] : 0;
           for (int i = 1; i < nums.len; i++)
             if (nums.data[i] > m)
               m = nums.data[i];
           m;
         }));
  return 0;
}
