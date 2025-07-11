#include <stdio.h>
#include <stdlib.h>

typedef struct Person Person;
typedef struct Book Book;

typedef struct Person {
  char *name;
  int age;
} Person;
typedef struct {
  int len;
  Person *data;
} list_Person;
static list_Person list_Person_create(int len) {
  list_Person l;
  l.len = len;
  l.data = calloc(len, sizeof(Person));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _print_list_Person(list_Person v) {
  for (int i = 0; i < v.len; i++) {
    Person s = v.data[i];
    printf("map[");
    printf("name:");
    printf("%s", s.name);
    printf(" ");
    printf("age:");
    printf("%d", s.age);
    printf(" ");
    printf("name:");
    printf("%s", s.name);
    printf(" ");
    printf("age:");
    printf("%d", s.age);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

typedef struct Book {
  char *title;
  Person author;
} Book;
typedef struct {
  int len;
  Book *data;
} list_Book;
static list_Book list_Book_create(int len) {
  list_Book l;
  l.len = len;
  l.data = calloc(len, sizeof(Book));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _print_list_Book(list_Book v) {
  for (int i = 0; i < v.len; i++) {
    Book s = v.data[i];
    printf("map[");
    printf("title:");
    printf("%s", s.title);
    printf(" ");
    printf("author:");
    printf(" ");
    printf("title:");
    printf("%s", s.title);
    printf(" ");
    printf("author:");
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

int main() {
  Book book =
      (Book){.title = "Go", .author = (Person){.name = "Bob", .age = 42}};
  printf("%s\n", book.author.name);
  return 0;
}
