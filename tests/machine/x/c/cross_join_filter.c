#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
typedef struct {
  int len;
  char **data;
} list_string;
static list_string list_string_create(int len) {
  list_string l;
  l.len = len;
  l.data = (char **)malloc(sizeof(char *) * len);
  return l;
}
typedef struct {
  int n;
  int l;
} pairsItem;
typedef struct {
  int len;
  pairsItem *data;
} list_pairsItem;
static list_pairsItem list_pairsItem_create(int len) {
  list_pairsItem l;
  l.len = len;
  l.data = (pairsItem *)malloc(sizeof(pairsItem) * len);
  return l;
}

int main() {
  list_int _t1 = list_int_create(3);
  _t1.data[0] = 1;
  _t1.data[1] = 2;
  _t1.data[2] = 3;
  list_int nums = _t1;
  list_string _t2 = list_string_create(2);
  _t2.data[0] = "A";
  _t2.data[1] = "B";
  list_string letters = _t2;
  list_pairsItem _t3 = list_pairsItem_create(nums.len * letters.len);
  int _t4 = 0;
  for (int _t5 = 0; _t5 < nums.len; _t5++) {
    int n = nums.data[_t5];
    for (int _t6 = 0; _t6 < letters.len; _t6++) {
      char *l = letters.data[_t6];
      if (!(((n % 2) == 0))) {
        continue;
      }
      _t3.data[_t4] = (pairsItem){.n = n, .l = l};
      _t4++;
    }
  }
  _t3.len = _t4;
  list_pairsItem pairs = _t3;
  printf("%s\n", "--- Even pairs ---");
  for (int _t7 = 0; _t7 < pairs.len; _t7++) {
    pairsItem p = pairs.data[_t7];
    printf("%d ", p.n);
    printf("%d\n", p.l);
  }
  return 0;
}
