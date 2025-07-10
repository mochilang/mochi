#include <stdio.h>
#include <stdlib.h>

typedef struct Todo Todo;

typedef struct Todo {
  char *title;
} Todo;

int main() {
  Todo todo = (Todo){.title = "hi"};
  printf("%s\n", todo.title);
  return 0;
}
