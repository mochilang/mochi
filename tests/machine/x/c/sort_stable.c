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
} ItemsItem;
typedef struct {
  int len;
  ItemsItem *data;
} list_ItemsItem;
static list_ItemsItem list_ItemsItem_create(int len) {
  list_ItemsItem l;
  l.len = len;
  l.data = calloc(len, sizeof(ItemsItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _print_list_itemsItem(list_ItemsItem v) {
  for (int i = 0; i < v.len; i++) {
    ItemsItem s = v.data[i];
    printf("map[");
    printf("n:");
    printf("%d", s.n);
    printf(" ");
    printf("v:");
    printf("%s", s.v);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

int main() {
  ItemsItem tmp1_data[] = {(ItemsItem){.n = 1, .v = "a"},
                           (ItemsItem){.n = 1, .v = "b"},
                           (ItemsItem){.n = 2, .v = "c"}};
  list_ItemsItem tmp1 = {3, tmp1_data};
  list_ItemsItem items = tmp1;
  list_string tmp2 = list_string_create(items.len);
  int *tmp5 = (int *)malloc(sizeof(int) * items.len);
  int tmp3 = 0;
  for (int tmp4 = 0; tmp4 < items.len; tmp4++) {
    ItemsItem i = items.data[tmp4];
    tmp2.data[tmp3] = i.v;
    tmp5[tmp3] = i.n;
    tmp3++;
  }
  tmp2.len = tmp3;
  for (int i8 = 0; i8 < tmp3 - 1; i8++) {
    for (int i9 = i8 + 1; i9 < tmp3; i9++) {
      if (tmp5[i8] > tmp5[i9]) {
        int tmp6 = tmp5[i8];
        tmp5[i8] = tmp5[i9];
        tmp5[i9] = tmp6;
        char *tmp7 = tmp2.data[i8];
        tmp2.data[i8] = tmp2.data[i9];
        tmp2.data[i9] = tmp7;
      }
    }
  }
  list_string result = tmp2;
  _print_list_string(result);
  printf("\n");
  return 0;
}
