#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  int len;
  char **data;
} list_string;
static list_string list_string_create(int len) {
  list_string l;
  l.len = len;
  l.data = calloc(len, sizeof(char *));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _print_list_string(list_string v) {
  for (int i = 0; i < v.len; i++) {
    if (i > 0)
      printf(" ");
    printf("%s", v.data[i]);
  }
}
typedef struct {
  int n;
  char *v;
} itemsItem;
typedef struct {
  int len;
  itemsItem *data;
} list_itemsItem;
static list_itemsItem list_itemsItem_create(int len) {
  list_itemsItem l;
  l.len = len;
  l.data = calloc(len, sizeof(itemsItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  itemsItem tmp1_data[] = {(itemsItem){.n = 1, .v = "a"},
                           (itemsItem){.n = 1, .v = "b"},
                           (itemsItem){.n = 2, .v = "c"}};
  list_itemsItem tmp1 = {3, tmp1_data};
  list_itemsItem items = tmp1;
  list_string tmp2 = list_string_create(items.len);
  int *tmp5 = (int *)malloc(sizeof(int) * items.len);
  int tmp3 = 0;
  for (int tmp4 = 0; tmp4 < items.len; tmp4++) {
    itemsItem i = items.data[tmp4];
    tmp2.data[tmp3] = i.v;
    tmp5[tmp3] = i.n;
    tmp3++;
  }
  tmp2.len = tmp3;
  for (int i = 0; i < tmp3 - 1; i++) {
    for (int j = i + 1; j < tmp3; j++) {
      if (tmp5[i] > tmp5[j]) {
        int tmp6 = tmp5[i];
        tmp5[i] = tmp5[j];
        tmp5[j] = tmp6;
        char *tmp7 = tmp2.data[i];
        tmp2.data[i] = tmp2.data[j];
        tmp2.data[j] = tmp7;
      }
    }
  }
  list_string result = tmp2;
  _print_list_string(result);
  printf("\n");
  return 0;
}
