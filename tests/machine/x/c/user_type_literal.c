#include <stdio.h>
#include <stdlib.h>

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
