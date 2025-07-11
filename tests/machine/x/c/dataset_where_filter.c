#include <stdio.h>
#include <stdlib.h>

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
  l.data = calloc(len, sizeof(peopleItem));
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
} adultsItem;
typedef struct {
  int len;
  adultsItem *data;
} list_adultsItem;
static list_adultsItem list_adultsItem_create(int len) {
  list_adultsItem l;
  l.len = len;
  l.data = calloc(len, sizeof(adultsItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  peopleItem tmp1_data[] = {(peopleItem){.name = "Alice", .age = 30},
                            (peopleItem){.name = "Bob", .age = 15},
                            (peopleItem){.name = "Charlie", .age = 65},
                            (peopleItem){.name = "Diana", .age = 45}};
  list_peopleItem tmp1 = {4, tmp1_data};
  list_peopleItem people = tmp1;
  list_adultsItem tmp2 = list_adultsItem_create(people.len);
  int tmp3 = 0;
  for (int tmp4 = 0; tmp4 < people.len; tmp4++) {
    peopleItem person = people.data[tmp4];
    if (!(person.age >= 18)) {
      continue;
    }
    tmp2.data[tmp3] = (adultsItem){
        .name = person.name, .age = person.age, .is_senior = person.age >= 60};
    tmp3++;
  }
  tmp2.len = tmp3;
  list_adultsItem adults = tmp2;
  printf("%s\n", "--- Adults ---");
  for (int tmp5 = 0; tmp5 < adults.len; tmp5++) {
    adultsItem person = adults.data[tmp5];
    printf("%s ", person.name);
    printf("%s ", "is");
    printf("%.16g ", person.age);
    printf("%s\n", (person.is_senior ? " (senior)" : ""));
  }
  return 0;
}
