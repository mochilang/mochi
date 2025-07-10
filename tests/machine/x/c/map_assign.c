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
  l.data = (scoresItem *)malloc(sizeof(scoresItem) * len);
  return l;
}

int main() {
  __auto_type scores = (scoresItem){.alice = 1};
  scores.data["bob"] = 2;
  printf("%d\n", scores.data["bob"]);
  return 0;
}
