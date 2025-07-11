#include <stdio.h>
#include <stdlib.h>

typedef struct {
  char *name;
  int price;
} productsItem;
typedef struct {
  int len;
  productsItem *data;
} list_productsItem;
static list_productsItem list_productsItem_create(int len) {
  list_productsItem l;
  l.len = len;
  l.data = calloc(len, sizeof(productsItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  productsItem tmp1_data[] = {
      (productsItem){.name = "Laptop", .price = 1500},
      (productsItem){.name = "Smartphone", .price = 900},
      (productsItem){.name = "Tablet", .price = 600},
      (productsItem){.name = "Monitor", .price = 300},
      (productsItem){.name = "Keyboard", .price = 100},
      (productsItem){.name = "Mouse", .price = 50},
      (productsItem){.name = "Headphones", .price = 200}};
  list_productsItem tmp1 = {7, tmp1_data};
  list_productsItem products = tmp1;
  list_productsItem tmp2 = list_productsItem_create(products.len);
  int *tmp5 = (int *)malloc(sizeof(int) * products.len);
  int tmp3 = 0;
  int tmp6 = 1;
  int tmp7 = 3;
  int tmp8 = 0;
  for (int tmp4 = 0; tmp4 < products.len; tmp4++) {
    productsItem p = products.data[tmp4];
    if (tmp8 < tmp6) {
      tmp8++;
      continue;
    }
    if (tmp7 >= 0 && tmp3 >= tmp7) {
      break;
    }
    tmp8++;
    tmp2.data[tmp3] = p;
    tmp5[tmp3] = (-p.price);
    tmp3++;
  }
  tmp2.len = tmp3;
  for (int i = 0; i < tmp3 - 1; i++) {
    for (int j = i + 1; j < tmp3; j++) {
      if (tmp5[i] > tmp5[j]) {
        int tmp9 = tmp5[i];
        tmp5[i] = tmp5[j];
        tmp5[j] = tmp9;
        productsItem tmp10 = tmp2.data[i];
        tmp2.data[i] = tmp2.data[j];
        tmp2.data[j] = tmp10;
      }
    }
  }
  list_productsItem expensive = tmp2;
  printf("%s\n", "--- Top products (excluding most expensive) ---");
  for (int tmp11 = 0; tmp11 < expensive.len; tmp11++) {
    productsItem item = expensive.data[tmp11];
    printf("%s ", item.name);
    printf("%s ", "costs $");
    printf("%.16g\n", item.price);
  }
  return 0;
}
