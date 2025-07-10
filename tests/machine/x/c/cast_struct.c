#include <stdio.h>
#include <stdlib.h>

typedef struct Todo Todo;

typedef struct {
  char *title;
} todoItem;
typedef struct {
  int len;
  todoItem *data;
} list_todoItem;
static list_todoItem list_todoItem_create(int len) {
  list_todoItem l;
  l.len = len;
  l.data = (todoItem *)malloc(sizeof(todoItem) * len);
  return l;
}

typedef struct Todo {
  char *title;
} Todo;

int main() {
  __auto_type todo = (Todo){.title = "hi"};
  printf("%s\n", todo.title);
  return 0;
}
