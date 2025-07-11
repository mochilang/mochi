#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int alice;
} ScoresItem;
typedef struct {
  int len;
  ScoresItem *data;
} list_ScoresItem;
static list_ScoresItem list_ScoresItem_create(int len) {
  list_ScoresItem l;
  l.len = len;
  l.data = calloc(len, sizeof(ScoresItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _print_list_scoresItem(list_ScoresItem v) {
  for (int i = 0; i < v.len; i++) {
    ScoresItem s = v.data[i];
    printf("map[");
    printf("alice:");
    printf("%d", s.alice);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

int main() {
  ScoresItem scores = (ScoresItem){.alice = 1};
  scores.data["bob"] = 2;
  printf("%d\n", scores.data["bob"]);
  return 0;
}
