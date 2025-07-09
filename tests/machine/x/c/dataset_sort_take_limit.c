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
  l.data = (productsItem *)malloc(sizeof(productsItem) * len);
  return l;
}

int main() {
  list_productsItem _t1 = list_productsItem_create(7);
  _t1.data[0] = (productsItem){.name = "Laptop", .price = 1500};
  _t1.data[1] = (productsItem){.name = "Smartphone", .price = 900};
  _t1.data[2] = (productsItem){.name = "Tablet", .price = 600};
  _t1.data[3] = (productsItem){.name = "Monitor", .price = 300};
  _t1.data[4] = (productsItem){.name = "Keyboard", .price = 100};
  _t1.data[5] = (productsItem){.name = "Mouse", .price = 50};
  _t1.data[6] = (productsItem){.name = "Headphones", .price = 200};
  list_productsItem products = _t1;
  list_productsItem _t2 = list_productsItem_create(products.len);
  int *_t5 = (int *)malloc(sizeof(int) * products.len);
  int _t3 = 0;
  int _t6 = 1;
  int _t7 = 3;
  int _t8 = 0;
  for (int _t4 = 0; _t4 < products.len; _t4++) {
    productsItem p = products.data[_t4];
    if (_t8 < _t6) {
      _t8++;
      continue;
    }
    if (_t7 >= 0 && _t3 >= _t7) {
      break;
    }
    _t8++;
    _t2.data[_t3] = p;
    _t5[_t3] = (-p.price);
    _t3++;
  }
  _t2.len = _t3;
  for (int i = 0; i < _t3 - 1; i++) {
    for (int j = i + 1; j < _t3; j++) {
      if (_t5[i] > _t5[j]) {
        int _t9 = _t5[i];
        _t5[i] = _t5[j];
        _t5[j] = _t9;
        productsItem _t10 = _t2.data[i];
        _t2.data[i] = _t2.data[j];
        _t2.data[j] = _t10;
      }
    }
  }
  list_productsItem expensive = _t2;
  printf("%s\n", "--- Top products (excluding most expensive) ---");
  for (int _t11 = 0; _t11 < expensive.len; _t11++) {
    productsItem item = expensive.data[_t11];
    printf("%s ", item.name);
    printf("%s ", "costs $");
    printf("%d\n", item.price);
  }
  return 0;
}
