#include <stdio.h>
#include <stdlib.h>

typedef struct {
  char *name;
  int price;
} ProductsItem;
typedef struct {
  int len;
  ProductsItem *data;
} list_ProductsItem;
static list_ProductsItem list_ProductsItem_create(int len) {
  list_ProductsItem l;
  l.len = len;
  l.data = calloc(len, sizeof(ProductsItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _print_list_productsItem(list_ProductsItem v) {
  for (int i = 0; i < v.len; i++) {
    ProductsItem s = v.data[i];
    printf("map[");
    printf("name:");
    printf("%s", s.name);
    printf(" ");
    printf("price:");
    printf("%d", s.price);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

int main() {
  ProductsItem tmp1_data[] = {
      (ProductsItem){.name = "Laptop", .price = 1500},
      (ProductsItem){.name = "Smartphone", .price = 900},
      (ProductsItem){.name = "Tablet", .price = 600},
      (ProductsItem){.name = "Monitor", .price = 300},
      (ProductsItem){.name = "Keyboard", .price = 100},
      (ProductsItem){.name = "Mouse", .price = 50},
      (ProductsItem){.name = "Headphones", .price = 200}};
  list_ProductsItem tmp1 = {7, tmp1_data};
  list_ProductsItem products = tmp1;
  list_ProductsItem tmp2 = list_ProductsItem_create(products.len);
  int *tmp5 = (int *)malloc(sizeof(int) * products.len);
  int tmp3 = 0;
  int tmp6 = 1;
  int tmp7 = 3;
  int tmp8 = 0;
  for (int tmp4 = 0; tmp4 < products.len; tmp4++) {
    ProductsItem p = products.data[tmp4];
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
  for (int i11 = 0; i11 < tmp3 - 1; i11++) {
    for (int i12 = i11 + 1; i12 < tmp3; i12++) {
      if (tmp5[i11] > tmp5[i12]) {
        int tmp9 = tmp5[i11];
        tmp5[i11] = tmp5[i12];
        tmp5[i12] = tmp9;
        ProductsItem tmp10 = tmp2.data[i11];
        tmp2.data[i11] = tmp2.data[i12];
        tmp2.data[i12] = tmp10;
      }
    }
  }
  list_ProductsItem expensive = tmp2;
  printf("%s\n", "--- Top products (excluding most expensive) ---");
  for (int tmp13 = 0; tmp13 < expensive.len; tmp13++) {
    ProductsItem item = expensive.data[tmp13];
    printf("%s ", item.name);
    printf("%s ", "costs $");
    printf("%d\n", item.price);
  }
  return 0;
}
