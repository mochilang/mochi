#include <stdio.h>
#include <stdlib.h>

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
typedef struct Person Person;
typedef struct Book Book;

typedef struct {
  char *name;
  int age;
} Person;

typedef struct {
  char *title;
  Person author;
} Book;

int main() {
  Book book =
      (Book){.title = "Go", .author = (Person){.name = "Bob", .age = 42}};
  printf("%s\n", book.author.name);
  return 0;
}
