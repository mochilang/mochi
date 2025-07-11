#include <stdio.h>
#include <stdlib.h>

static int x = 3;
static int y = 4;

typedef struct {
  int a;
  int b;
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
static void _print_list_mItem(list_MItem v) {
  for (int i = 0; i < v.len; i++) {
    MItem s = v.data[i];
    printf("map[");
    printf("a:");
    printf("%d", s.a);
    printf(" ");
    printf("b:");
    printf("%d", s.b);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

int main() {
  MItem m = (MItem){.a = x, .b = y};
  printf("%d ", m.a);
  printf("%d\n", m.b);
  return 0;
}
