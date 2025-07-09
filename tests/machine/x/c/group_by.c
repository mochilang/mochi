#include <stdio.h>
#include <stdlib.h>

typedef struct {
  char *name;
  int age;
  char *city;
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
  int city;
  int count;
  double avg_age;
} statsItem;
typedef struct {
  int len;
  statsItem *data;
} list_statsItem;
static list_statsItem list_statsItem_create(int len) {
  list_statsItem l;
  l.len = len;
  l.data = (statsItem *)malloc(sizeof(statsItem) * len);
  return l;
}

int main() {
  list_peopleItem _t1 = list_peopleItem_create(6);
  _t1.data[0] = (peopleItem){.name = "Alice", .age = 30, .city = "Paris"};
  _t1.data[1] = (peopleItem){.name = "Bob", .age = 15, .city = "Hanoi"};
  _t1.data[2] = (peopleItem){.name = "Charlie", .age = 65, .city = "Paris"};
  _t1.data[3] = (peopleItem){.name = "Diana", .age = 45, .city = "Hanoi"};
  _t1.data[4] = (peopleItem){.name = "Eve", .age = 70, .city = "Paris"};
  _t1.data[5] = (peopleItem){.name = "Frank", .age = 22, .city = "Hanoi"};
  list_peopleItem people = _t1;
  list_statsItem stats = 0;
  printf("%s\n", "--- People grouped by city ---");
  for (int _t2 = 0; _t2 < stats.len; _t2++) {
    statsItem s = stats.data[_t2];
    printf("%d ", s.city);
    printf("%s ", ": count =");
    printf("%d ", s.count);
    printf("%s ", ", avg_age =");
    printf("%g\n", s.avg_age);
  }
  return 0;
}
