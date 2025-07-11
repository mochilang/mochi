#include <stdio.h>
#include <stdlib.h>

typedef struct Person Person;

typedef struct Person {
  char *name;
  int age;
  char *status;
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
    printf("status:");
    printf("%s", s.status);
    printf(" ");
    printf("name:");
    printf("%s", s.name);
    printf(" ");
    printf("age:");
    printf("%d", s.age);
    printf(" ");
    printf("status:");
    printf("%s", s.status);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

static void test_update_adult_status() {
  Person tmp1_data[] = {
      (Person){.name = "Alice", .age = 17, .status = "minor"},
      (Person){.name = "Bob", .age = 26, .status = "adult"},
      (Person){.name = "Charlie", .age = 19, .status = "adult"},
      (Person){.name = "Diana", .age = 16, .status = "minor"}};
  list_Person tmp1 = {4, tmp1_data};
  if (!(people == tmp1)) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  Person tmp2_data[] = {
      (Person){.name = "Alice", .age = 17, .status = "minor"},
      (Person){.name = "Bob", .age = 25, .status = "unknown"},
      (Person){.name = "Charlie", .age = 18, .status = "unknown"},
      (Person){.name = "Diana", .age = 16, .status = "minor"}};
  list_Person tmp2 = {4, tmp2_data};
  list_Person people = tmp2;
  for (int tmp3 = 0; tmp3 < people.len; tmp3++) {
    Person tmp4 = people.data[tmp3];
    char *name = tmp4.name;
    int age = tmp4.age;
    char *status = tmp4.status;
    char *name = tmp4.name;
    int age = tmp4.age;
    char *status = tmp4.status;
    if (tmp4.age >= 18) {
      tmp4.status = "adult";
      tmp4.age = tmp4.age + 1;
    }
    people.data[tmp3] = tmp4;
  }
  printf("%s\n", "ok");
  test_update_adult_status();
  return 0;
}
