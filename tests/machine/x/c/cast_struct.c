#include <stdio.h>
#include <stdlib.h>

typedef struct Todo Todo;

typedef struct Todo {
  char *title;
} Todo;
typedef struct {
  int len;
  Todo *data;
} list_Todo;
static list_Todo list_Todo_create(int len) {
  list_Todo l;
  l.len = len;
  l.data = (Todo *)malloc(sizeof(Todo) * len);
  return l;
}

int main() {
  Todo todo = (Todo){.title = "hi"};
  printf("%s\n", todo.title);
  return 0;
}
