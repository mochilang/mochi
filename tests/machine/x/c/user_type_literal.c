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
  l.data = (Person *)malloc(sizeof(Person) * len);
  return l;
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
  l.data = (Book *)malloc(sizeof(Book) * len);
  return l;
}

int main() {
  Book book =
      (Book){.title = "Go", .author = (Person){.name = "Bob", .age = 42}};
  printf("%s\n", book.author.name);
  return 0;
}
