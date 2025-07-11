#include <stdio.h>
#include <stdlib.h>

typedef struct {
  char *name;
  int age;
} PeopleItem;
typedef struct {
  int len;
  PeopleItem *data;
} list_PeopleItem;
static list_PeopleItem list_PeopleItem_create(int len) {
  list_PeopleItem l;
  l.len = len;
  l.data = calloc(len, sizeof(PeopleItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  char *name;
  int age;
  int is_senior;
} AdultsItem;
typedef struct {
  int len;
  AdultsItem *data;
} list_AdultsItem;
static list_AdultsItem list_AdultsItem_create(int len) {
  list_AdultsItem l;
  l.len = len;
  l.data = calloc(len, sizeof(AdultsItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  PeopleItem tmp1_data[] = {(PeopleItem){.name = "Alice", .age = 30},
                            (PeopleItem){.name = "Bob", .age = 15},
                            (PeopleItem){.name = "Charlie", .age = 65},
                            (PeopleItem){.name = "Diana", .age = 45}};
  list_PeopleItem tmp1 = {4, tmp1_data};
  list_PeopleItem people = tmp1;
  list_adultsItem tmp2 = list_adultsItem_create(people.len);
  int tmp3 = 0;
  for (int tmp4 = 0; tmp4 < people.len; tmp4++) {
    PeopleItem person = people.data[tmp4];
    if (!(person.age >= 18)) {
      continue;
    }
    tmp2.data[tmp3] = (AdultsItem){
        .name = person.name, .age = person.age, .is_senior = person.age >= 60};
    tmp3++;
  }
  tmp2.len = tmp3;
  list_AdultsItem adults = tmp2;
  printf("%s\n", "--- Adults ---");
  for (int tmp5 = 0; tmp5 < adults.len; tmp5++) {
    AdultsItem person = adults.data[tmp5];
    printf("%s ", person.name);
    printf("%s ", "is");
    printf("%d ", person.age);
    printf("%s\n", (person.is_senior ? " (senior)" : ""));
  }
  return 0;
}
