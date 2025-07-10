#include <stdio.h>
#include <stdlib.h>

typedef struct Person Person;

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

typedef struct Person {
  char *name;
  int age;
  char *status;
} Person;

static void test_update_adult_status() {
  list_Person _t1 = list_Person_create(4);
  _t1.data[0] = (Person){.name = "Alice", .age = 17, .status = "minor"};
  _t1.data[1] = (Person){.name = "Bob", .age = 26, .status = "adult"};
  _t1.data[2] = (Person){.name = "Charlie", .age = 19, .status = "adult"};
  _t1.data[3] = (Person){.name = "Diana", .age = 16, .status = "minor"};
  if (!(people == _t1)) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  list_Person _t2 = list_Person_create(4);
  _t2.data[0] = (Person){.name = "Alice", .age = 17, .status = "minor"};
  _t2.data[1] = (Person){.name = "Bob", .age = 25, .status = "unknown"};
  _t2.data[2] = (Person){.name = "Charlie", .age = 18, .status = "unknown"};
  _t2.data[3] = (Person){.name = "Diana", .age = 16, .status = "minor"};
  list_Person people = _t2;
  for (int _t3 = 0; _t3 < people.len; _t3++) {
    Person _t4 = people.data[_t3];
    char *name = _t4.name;
    int age = _t4.age;
    char *status = _t4.status;
    char *name = _t4.name;
    int age = _t4.age;
    char *status = _t4.status;
    if (_t4.age >= 18) {
      _t4.status = "adult";
      _t4.age = _t4.age + 1;
    }
    people.data[_t3] = _t4;
  }
  printf("%s\n", "ok");
  test_update_adult_status();
  return 0;
}
