#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int alice;
} scoresItem;
typedef struct {
  int len;
  scoresItem *data;
} list_scoresItem;
static list_scoresItem list_scoresItem_create(int len) {
  list_scoresItem l;
  l.len = len;
  l.data = calloc(len, sizeof(scoresItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  scoresItem scores = (scoresItem){.alice = 1};
  scores.data["bob"] = 2;
  printf("%d\n", scores.data["bob"]);
  return 0;
}
