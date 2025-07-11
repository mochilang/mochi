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
  l.data = calloc(len, sizeof(Todo));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _print_list_Todo(list_Todo v) {
  for (int i = 0; i < v.len; i++) {
    Todo s = v.data[i];
    printf("map[");
    printf("title:");
    printf("%s", s.title);
    printf(" ");
    printf("title:");
    printf("%s", s.title);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

int main() {
  Todo todo = (Todo){.title = "hi"};
  printf("%s\n", todo.title);
  return 0;
}
