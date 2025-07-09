#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int len;
  int *data;
} list_int;
static list_int list_int_create(int len) {
  list_int l;
  l.len = len;
  l.data = (int *)malloc(sizeof(int) * len);
  return l;
}
typedef struct {
  char *name;
  int age;
} peopleItem;
typedef struct {
  int len;
  peopleItem *data;
} list_peopleItem;
static list_peopleItem list_peopleItem_create(int len) {
  list_peopleItem l;
  l.len = len;
  l.data = (peopleItem *)malloc(sizeof(peopleItem) * len);
  return l;
}

typedef struct {
  int name;
  int age;
  int is_senior;
} adultsItem;
typedef struct {
  int len;
  adultsItem *data;
} list_adultsItem;
static list_adultsItem list_adultsItem_create(int len) {
  list_adultsItem l;
  l.len = len;
  l.data = (adultsItem *)malloc(sizeof(adultsItem) * len);
  return l;
}

int main() {
  list_peopleItem _t1 = list_peopleItem_create(4);
  _t1.data[0] = (peopleItem){.name = "Alice", .age = 30};
  _t1.data[1] = (peopleItem){.name = "Bob", .age = 15};
  _t1.data[2] = (peopleItem){.name = "Charlie", .age = 65};
  _t1.data[3] = (peopleItem){.name = "Diana", .age = 45};
  list_peopleItem people = _t1;
  list_adultsItem _t2 = list_adultsItem_create(people.len);
  int _t3 = 0;
  for (int _t4 = 0; _t4 < people.len; _t4++) {
    peopleItem person = people.data[_t4];
    if (!((person.age >= 18))) {
      continue;
    }
    _t2.data[_t3] = (adultsItem){.name = person.name,
                                 .age = person.age,
                                 .is_senior = (person.age >= 60)};
    _t3++;
  }
  _t2.len = _t3;
  list_adultsItem adults = _t2;
  printf("%s\n", "--- Adults ---");
  for (int _t5 = 0; _t5 < adults.len; _t5++) {
    adultsItem person = adults.data[_t5];
    printf("%d ", person.name);
    printf("%s ", "is");
    printf("%d ", person.age);
    printf("%s\n", (person.is_senior ? " (senior)" : ""));
  }
  return 0;
}
