// Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
#include <stdio.h>
#include <stdlib.h>

typedef struct {
  char *name;
  int age;
} people_t;
typedef struct {
  int len;
  people_t *data;
} people_list_t;
people_list_t create_people_list(int len) {
  people_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(people_t));
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
} adults_item_t;
typedef struct {
  int len;
  adults_item_t *data;
} adults_item_list_t;
adults_item_list_t create_adults_item_list(int len) {
  adults_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(adults_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int _mochi_main() {
  people_t people[] = {
      {"Alice", 30}, {"Bob", 15}, {"Charlie", 65}, {"Diana", 45}};
  int people_len = sizeof(people) / sizeof(people[0]);
  adults_item_list_t tmp1 = create_adults_item_list(people_len);
  int tmp2 = 0;
  for (int tmp3 = 0; tmp3 < people_len; tmp3++) {
    people_t person = people[tmp3];
    if (!(person.age >= 18)) {
      continue;
    }
    tmp1.data[tmp2] = (adults_item_t){
        .name = person.name, .age = person.age, .is_senior = person.age >= 60};
    tmp2++;
  }
  tmp1.len = tmp2;
  adults_item_list_t adults = tmp1;
  printf("--- Adults ---\n");
  for (int tmp4 = 0; tmp4 < adults.len; tmp4++) {
    adults_item_t person = adults.data[tmp4];
    printf("%s is %d %s\n", person.name, person.age,
           (person.is_senior ? " (senior)" : ""));
  }
  free(adults.data);
  return 0;
}
int main() { return _mochi_main(); }
