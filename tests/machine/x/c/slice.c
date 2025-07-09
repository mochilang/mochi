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
  l.data = (int *)malloc(sizeof(int) * len);
  return l;
}
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
int main() {
  list_int _t1 = list_int_create(3);
  _t1.data[0] = 1;
  _t1.data[1] = 2;
  _t1.data[2] = 3;
  list_int _t2 = slice_list_int(_t1, 1, 3);
  _print_list_int(_t2);
  printf("\n");
  list_int _t3 = list_int_create(3);
  _t3.data[0] = 1;
  _t3.data[1] = 2;
  _t3.data[2] = 3;
  list_int _t4 = slice_list_int(_t3, 0, 2);
  _print_list_int(_t4);
  printf("\n");
  char *_t5 = ({
    int _len = strlen("hello");
    int _s = 1;
    int _e = 4;
    if (_s < 0)
      _s += _len;
    if (_e < 0)
      _e += _len;
    if (_s < 0)
      _s = 0;
    if (_e > _len)
      _e = _len;
    if (_s > _e)
      _s = _e;
    char *_b = (char *)malloc(_e - _s + 1);
    memcpy(_b, "hello" + _s, _e - _s);
    _b[_e - _s] = '\0';
    _b;
  });
  printf("%s\n", _t5);
  return 0;
}
