// Generated by Mochi compiler v0.10.28 on 2025-07-18T10:18:03Z
// Generated by Mochi compiler v0.10.28 on 2025-07-18T10:18:03Z
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
static list_int concat_list_int(list_int a, list_int b) {
  list_int r = list_int_create(a.len + b.len);
  for (int i = 0; i < a.len; i++)
    r.data[i] = a.data[i];
  for (int i = 0; i < b.len; i++)
    r.data[a.len + i] = b.data[i];
  return r;
}
int mochi_main() {
  list_int list = list_int_create(0);
  int a = 1;
  int d = 2;
  int e = 3;
  int i = 4;
  int tmp1_data[1];
  list_int tmp1 = {1, tmp1_data};
  tmp1.data[0] = a;
  list_int tmp2 = concat_list_int(list, tmp1);
  list = tmp2;
  int tmp3_data[1];
  list_int tmp3 = {1, tmp3_data};
  tmp3.data[0] = d;
  list_int tmp4 = concat_list_int(list, tmp3);
  list = tmp4;
  int tmp5_data[1];
  list_int tmp5 = {1, tmp5_data};
  tmp5.data[0] = e;
  list_int tmp6 = concat_list_int(list, tmp5);
  list = tmp6;
  int tmp7_data[1];
  list_int tmp7 = {1, tmp7_data};
  tmp7.data[0] = i;
  list_int tmp8 = concat_list_int(list, tmp7);
  list = tmp8;
  i = list.len;
}

int _mochi_main() {
  mochi_main();
  return 0;
}
int main() { return _mochi_main(); }
