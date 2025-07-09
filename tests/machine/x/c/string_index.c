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
int main() {
  char *s = "mochi";
  char *_t1 = ({
    int _len = strlen(s);
    int _i = 1;
    if (_i < 0)
      _i += _len;
    if (_i < 0 || _i >= _len) {
      fprintf(stderr, "index out of range\n");
      exit(1);
    }
    char *_b = (char *)malloc(2);
    _b[0] = s[_i];
    _b[1] = '\0';
    _b;
  });
  printf("%s\n", _t1);
  return 0;
}
